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
