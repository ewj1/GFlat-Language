module GFlat
open Parser



(* AST *)
type Instrument =
| Piano

type Chord =
| Cmaj

type SoundInstruction =
| Sound of Instrument * Chord

type Expr =
| SoundInstructions of SoundInstruction list




(* Grammar *)

let pinstrument: Parser<Instrument> = pleft (pstr "Piano" |>> (fun _ -> Piano)) pws1
let pchord: Parser<Chord> = pleft (pstr "Cmaj" |>> (fun _ -> Cmaj)) pws0
let psound: Parser<SoundInstruction> = pseq pinstrument pchord (fun a -> Sound a)
let psoundInstruction: Parser<SoundInstruction> = pleft psound pws0
let pexpr: Parser<Expr> = pmany1 psoundInstruction |>> (fun ss -> SoundInstructions ss) 
let grammar: Parser<Expr> = pleft pexpr peof

let parse(input: string) : Expr option =
    let i = prepare input
    match grammar i with
    | Success(ast,_) -> Some ast
    | Failure(_,_) -> None




 (* Eval *)

let prefix = "prefix"
let doctype = "doctype"
let suffix = "suffix"


let rec evalSoundInstructions ss =
    printfn "got evalSoundInstructions with %A" ss
    match ss with
    | [] -> ""
    | s::ss' ->
        let sound = "something abc to midi format: https://abcmidi.sourceforge.io/#top"
        let sounds = evalSoundInstructions ss'
        sound + sounds
        
let eval e =
    let str =
        match e with
        | SoundInstructions ss -> evalSoundInstructions ss
    doctype + prefix + str + suffix
