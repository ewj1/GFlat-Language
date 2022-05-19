module ProjAST

open Parser



(* AST *)

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
// chord qualities and their starting note
| Maj of Note
| Min of Note
| Dom7 of Note
| Maj7 of Note
| Min7 of Note
| Dim of Note
| Aug of Note

type Expr =
// variable name    
| Variable of string
// a tuple of a chord and its duration in beats
| Sound of Chord * int
// a series of sounds and/or variables (representing sections) grouped together to form a part of a song (e.g. an "intro" or "chorus")
| Section of Expr list
// assigns section to variable
| Assignment of Expr * Expr
// a list of variables representing sections and how many times they should repeat
| Play of (Expr * int) list
// tuple of Assignment list and Play
| Program of Expr list * Expr




 
