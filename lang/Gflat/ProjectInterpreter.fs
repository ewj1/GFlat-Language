module ProjInterpreter

open ProjAST


type Env = Map<Expr, Expr> //variables link to sections

let noteList =
    [|C; Csharp; D; Dsharp; E; F; Fsharp; G; Gsharp; A; Asharp; B|]
    
//music xml start of a midi-convertable file
let prefix =
    "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>  
       <!DOCTYPE score-partwise PUBLIC
         \"-//Recordare//DTD MusicXML 4.0 Partwise//EN\"
         \"http://www.musicxml.org/dtds/partwise.dtd\">
       <score-partwise version=\"4.0\">
         <part-list>
           <score-part id=\"P1\">
             <part-name>Music</part-name>
           </score-part>
         </part-list>
     <part id=\"P1\">"

//start of a measure (in the key of C in 4:4)
let measureStart = 
   "<measure number=\"1\">
      <attributes>
        <divisions>1</divisions>
        <key>
          <fifths>0</fifths>
        </key>
        <time>
          <beats>4</beats>
          <beat-type>4</beat-type>
        </time>
      </attributes>"

//end of a measure
let measureEnd =
    "</measure>"

//end of a part and the piece of music
let suffix = "</part>
</score-partwise>"

let xmlPitch note octave = //returns an XML representation of a pitch
    let noteInfo = //tuples with string note and boolean alter vlaue (true for sharp false for natural)
        match note with //we allow users to write chords with sharps or flats (e.g. C# maj or Db maj) but for simplicity in implementation use only sharp version of equivalent notes when evaluating
        | C -> ("C", false)
        | Csharp -> ("C", true)
        | D -> ("D", false)
        | Dsharp -> ("D", true)
        | E -> ("E", false)
        | F -> ("F", false)
        | Fsharp -> ("F", true)
        | G -> ("G", false)
        | Gsharp -> ("G", true)
        | A -> ("A", false)
        | Asharp -> ("A", true)
        | B -> ("B", false)
        | _ -> failwith "cannot call xmlPitch on a flat version of a note" // this should be unreachable because noteList doesn't include flats
    match noteInfo with //return the XML string representation of a pitch given the info above
    | (note, alter) when alter ->
        "<pitch>
           <step>" + note + "</step>
           <alter>1</alter>
           <octave>" + (string octave) + "</octave>
         </pitch>"
    | (note, alter) ->
        "<pitch>
           <step>" + note + "</step>
           <octave>" + (string octave) + "</octave>
         </pitch>"

let xmlChord chord  duration = //return a string representing an XML chord

    let noteNum (note): int = //convert a note into a number
        match note with
        | C -> 0
        | Csharp -> 1
        | Dflat -> 1
        | D -> 2
        | Dsharp -> 3
        | Eflat -> 3
        | E -> 4
        | F -> 5
        | Fsharp -> 6
        | Gflat -> 6
        | G -> 7
        | Gsharp -> 8
        | Aflat -> 8
        | A -> 9
        | Asharp -> 10
        | Bflat -> 10
        | B -> 11
        
    let noteNums: int list = //a list representing the nums of all the notes in a chord
        let adjust list note = ////adjust by the value of the starting note in a chord
            List.map (fun x -> (x + (noteNum note))) list 
        match chord with //the note intervals in a chord
        | Maj(note) -> adjust [0;4;7] note
        | Min(note) -> adjust [0;3;7] note 
    let firstNote = "<note>" + (xmlPitch (noteList[((noteNums.Head)%12)]) (4 + (noteNums.Head/12))) + "<duration>" + (string duration) + "</duration></note>" //first note does not include <chord/> add-on
    
    let rec noteNumsToXml (noteNumList: int list) =
        match noteNumList with //starting with the tail because head is handled in special first case
        | [] -> ""
        | head :: tail -> "<note><chord/>" + (xmlPitch (noteList[head % 12]) (4 + (head/12))) + "<duration>" + (string duration) + "</duration></note>" + (noteNumsToXml tail)

    
    firstNote + (noteNumsToXml (List.tail noteNums))  //return the xml representation of the chord as a string
    
    
        

//goes through a list of soundInstructions and interprets them as musicxml chords
let rec evalSounds input =
    printfn "got evalSounds with %A" input
    match input with
    | [] -> ""
    | head::tail ->
        let sound =
            match head with
            | Sound(chord, duration) -> xmlChord chord duration
        let sounds = evalSounds tail
        sound + sounds

let rec evalAssignments input (env:Env) = //input is a list of assignments
    printfn "got evalAssignments with %A" input
    match input with
    | [] -> env
    | head::tail ->
        let env' =
            match head with
            | Assignment(variable: Expr, section: Expr) -> env.Add(variable, section)
            | _ -> failwith ("Section definitions must come before play")
        evalAssignments tail env'
    

let playToXml input (env: Env) =
    printfn "got evalAssignments with %A" input
    match input with
    | (var, num) ->
        if env.ContainsKey var then
            let output = 
                match (env.Item var) with
                | Section(a) -> a
                | _ -> failwith("values in environment must be sections")

                |> evalSounds
            String.replicate num output
        else
            let v =
                match var with
                | Variable(v) -> v
                | _ -> failwith ("keys in environment must be variables")
            failwith ("Undefined variable '" + v + "'")                   
                
let evalPlay input env =
    match input with
    | Play(a) ->
        List.fold (fun x y -> x + (playToXml y env)) "" a
    | _ -> failwith ("there must be a play in the program")
        
//creates a complete musicxml file, with starting stuff, chords in the program, and ending stuff
let eval e (env: Env) =
    let str =
        match e with //deconstructing the program tuple
        | Program(assignments, play) ->
            let env' = evalAssignments assignments env //evaluates all the assignments
            evalPlay play env'               //returns the xml representation of the played sections
        | _ -> failwith("highest level should be a program of a list of assignments and play")
             
    prefix + measureStart + str + measureEnd + suffix

