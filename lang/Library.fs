module GFlat
open Parser



(* AST *)

type Instrument =
| Piano

type Note =
| C
| Csharp
| Dflat
| D
| Dsharp
| Eflat
| E
| F
| Fsharp
| Gflat
| G
| Gsharp
| Aflat
| A
| Asharp
| Bflat
| B

type Chord =
| Maj of Note
| Min of Note

type SoundInstruction =
| Sound of Instrument * Chord

type Expr =
| SoundInstructions of SoundInstruction list


(* Grammar *)

//parses a note (the letter name of a chord)
let pnote: Parser<Note> = (pfresult (pstr "C#") Csharp) <|> (pfresult (pstr "C") C) <|> (pfresult (pstr "Db") Dflat) <|> (pfresult (pstr "D#") Dsharp) <|> (pfresult (pstr "D") D) <|> (pfresult (pstr "Eb") Eflat) <|> (pfresult (pstr "E") E) <|> (pfresult (pstr "F#") Fsharp) <|> (pfresult (pstr "F") F) <|> (pfresult (pstr "Gb") Gflat) <|> (pfresult (pstr "G#") Gsharp) <|> (pfresult (pstr "G") G) <|> (pfresult (pstr "Ab") Aflat) <|> (pfresult (pstr "A#") Asharp) <|> (pfresult (pstr "A") A) <|> (pfresult (pstr "Bb") Bflat) <|> (pfresult (pstr "B") B)
//parses the quality of a chord
let pchordquality = (pfresult (pstr "maj") Maj) <|> (pfresult (pstr "min") Min)
//parses an instrument
let pinstrument: Parser<Instrument> = pleft (pstr "Piano" |>> (fun _ -> Piano)) pws1
// parses a chord
let pchord: Parser<Chord> = pleft (pseq (pleft pnote pws0) pchordquality (fun (a,b) -> b a)) pws0
//parses an instrument and chord together
let psound: Parser<SoundInstruction> = pseq pinstrument pchord (fun a -> Sound a)
//parses many instrument and chords pairs
let psoundInstruction: Parser<SoundInstruction> = pleft psound pws0
//parses many of the series of pairs referred to above
let pexpr: Parser<Expr> = pmany1 psoundInstruction |>> (fun ss -> SoundInstructions ss)
//parses an expression and makes sure we reach the end
let grammar: Parser<Expr> = pleft pexpr peof 

let parse(input: string) : Expr option =
    let i = prepare input
    match grammar i with
    | Success(ast,_) -> Some ast
    | Failure(_,_) -> None




 (* Eval *)
let chordIntervals =
    [(Maj, [0;4;7]), (Min, [0;3;7])]

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

let xmlChord chord  = //return a string representing an XML chord

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
    let firstNote = "<note>" + (xmlPitch (noteList[((noteNums.Head)%12)]) (4 + (noteNums.Head/12))) + "<duration>4</duration></note>" //first note does not include <chord/> add-on
    
    let rec noteNumsToXml (noteNumList: int list) =
        match noteNumList with //starting with the tail because head is handled in special first case
        | [] -> ""
        | head :: tail -> "<note><chord/>" + (xmlPitch (noteList[head % 12]) (4 + (head/12))) + "<duration>4</duration></note>" + (noteNumsToXml tail)

    
    firstNote + (noteNumsToXml (List.tail noteNums))  //return the xml representation of the chord as a string
    
    
        

//goes through a list of soundInstructions and interprets them as musicxml chords
let rec evalSoundInstructions input =
    printfn "got evalSoundInstructions with %A" input
    match input with
    | [] -> ""
    | head::tail ->
        let sound =
            match head with
            | Sound(instrument, chord) -> xmlChord chord
        let sounds = evalSoundInstructions tail
        sound + sounds

//creates a complete musicxml file, with starting stuff, chords in the program, and ending stuff
let eval e =
    let str =
        match e with
        | SoundInstructions ss -> evalSoundInstructions ss
    prefix + measureStart + str + measureEnd + suffix
