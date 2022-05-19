module ProjParser

open ProjAST
open Parser

(* Grammar *)

let reserved = ["play"; "let"; "="]

let pad p = pbetween pws0 pws0 p

//parses a note (the letter name of a chord)
let pnote: Parser<Note> = (pfresult (pstr "C#") Csharp) <|> (pfresult (pstr "C") C) <|> (pfresult (pstr "Db") Dflat) <|> (pfresult (pstr "D#") Dsharp) <|> (pfresult (pstr "D") D) <|> (pfresult (pstr "Eb") Eflat) <|> (pfresult (pstr "E") E) <|> (pfresult (pstr "F#") Fsharp) <|> (pfresult (pstr "F") F) <|> (pfresult (pstr "Gb") Gflat) <|> (pfresult (pstr "G#") Gsharp) <|> (pfresult (pstr "G") G) <|> (pfresult (pstr "Ab") Aflat) <|> (pfresult (pstr "A#") Asharp) <|> (pfresult (pstr "A") A) <|> (pfresult (pstr "Bb") Bflat) <|> (pfresult (pstr "B") B) <!> "pnote"

//parses the quality of a chord
let pchordquality = (pfresult (pstr "maj7") Maj7) <|> (pfresult (pstr "min7") Min7) <|> (pfresult (pstr "7") Dom7) <|> (pfresult (pstr "maj") Maj) <|> (pfresult (pstr "min") Min) <|> (pfresult (pstr "dim") Dim) <|> (pfresult (pstr "aug") Aug) <!> "pchordquality"

// parses a chord
let pchord: Parser<Chord> = pleft (pseq (pleft pnote pws0) pchordquality (fun (a,b) -> b a)) pws0 <!> "pchord"

let pnum: Parser<int> = pmany1 pdigit
                             |>> (fun digits ->
                                    let s = stringify digits
                                    let n = int s
                                    n
                                  ) <!> "pnum"
                                  
//parses a chord and duration together
let psound: Parser<Expr> = pseq pchord pnum (fun (a,b) -> Sound(a,b)) <!> "psound"

let pvarchar: Parser<char> = pletter <|> pdigit <!> "pvarchar"

let pvar: Parser<Expr> = pseq pletter (pmany0 pvarchar |>> stringify)
                           (fun (c: char, s: string) -> (string c) + s)
                           |>> (fun v ->
                                 if List.contains v reserved then
                                     failwith ("'" + v + "' is a reserved word.")
                                 else
                                     Variable v
                               ) <!> "pvar"

//
let psubsection: Parser<Expr> = pseq (pad pvar) (pad pnum) (fun (a,b) -> Subsection(a,b))

//parses a list of sounds or variable representing other sections
let psection: Parser<Expr> = (pmany1 (pad psound <|> psubsection)) |>> (fun soundList -> Section soundList) <!> "psection"

let pcurlybraces: Parser<Expr> = pbetween (pad (pchar '{')) (pad (pchar '}')) psection  <!> "pcurlybraces"

let passign: Parser<Expr> = pseq (pright (pstr "let") (pleft (pad pvar) (pad (pstr "=")))) pcurlybraces Assignment <!> "passign"

let pplay = pright (pad (pstr "play")) ((pmany1 (pseq (pad pvar) (pad pnum) (fun a -> a))) |>> (fun a -> Play a)) <!> "pplay"

//parses many of the series of pairs referred to above
let pexpr: Parser<Expr> = pseq (pmany1 passign) pplay (fun(assignments, play) -> Program (assignments, play)) <!> "pexpr" 


//parses an expression and makes sure we reach the end
let grammar: Parser<Expr> = pleft pexpr peof <!> "pgrammar"

let parse(input: string) (debugInput: bool): Expr option =
    let i = if debugInput then debug input else prepare input
    match grammar i with
    | Success(ast,_) -> Some ast
    | Failure(_,_) -> None


