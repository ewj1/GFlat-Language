module GFlat
open Parser



(* AST *)
type Instrument =
| Piano

type Chord =
| Maj of string
| Min of string

type SoundInstruction =
| Sound of Instrument * Chord

type Expr =
| SoundInstructions of SoundInstruction list

// will be used later when we are able to parse many different chords
(*let chordSorter s =
    if s[1] = " " then 
        match s[2..] with //potentially add other ways of writing chords (e.g. major, M, Maj, etc.)
        | "maj" -> Maj s[0]
        | "min" -> Min s[0]
        | _     -> Assert.fail()
        
    else if s[2] = " " then
        match s[3..] with
        | "maj" -> Maj s[0..1]
        | "min" -> Min s[0..1]
        | _     -> Assert.fail()
*)

(* Grammar *)

let pinstrument: Parser<Instrument> = pleft (pstr "Piano" |>> (fun _ -> Piano)) pws1  //parses an instrument
let pchord: Parser<Chord> = pleft (pstr "C maj" |>> (fun _ -> Maj "C")) pws0          //parses a chord (right now only C maj but soon to be expanded!)
let psound: Parser<SoundInstruction> = pseq pinstrument pchord (fun a -> Sound a)     //parses an instrument and chord together
let psoundInstruction: Parser<SoundInstruction> = pleft psound pws0                   //parses many instrument and chords pairs
let pexpr: Parser<Expr> = pmany1 psoundInstruction |>> (fun ss -> SoundInstructions ss) //parses many of the series of pairs referred to above
let grammar: Parser<Expr> = pleft pexpr peof //parses an expression and makes sure we reach the end

let parse(input: string) : Expr option =
    let i = prepare input
    match grammar i with
    | Success(ast,_) -> Some ast
    | Failure(_,_) -> None




 (* Eval *)


//music xml start of a midi-convertable file
let prefix = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>  
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


//goes through a list of soundInstructions and interprets them as musicxml chords
let rec evalSoundInstructions ss =
    printfn "got evalSoundInstructions with %A" ss
    match ss with
    | [] -> ""
    | s::ss' ->
        let sound = "<note>
    <pitch>
      <step>C</step>
      <octave>4</octave>
    </pitch>
    <duration>1</duration>
  </note>
  <note>
    <chord/>
    <pitch>
      <step>E</step>
      <octave>4</octave>
    </pitch>
    <duration>1</duration>
  </note>
  <note>
    <chord/>
    <pitch>
      <step>G</step>
      <octave>4</octave>
    </pitch>
    <duration>1</duration>
  </note>"
        let sounds = evalSoundInstructions ss'
        sound + sounds

//creates a complete musicxml file, with starting stuff, chords in the program, and ending stuff
let eval e =
    let str =
        match e with
        | SoundInstructions ss -> evalSoundInstructions ss
    prefix + measureStart + str + measureEnd + suffix
