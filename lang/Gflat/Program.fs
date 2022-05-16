open ProjAST
open ProjParser
open ProjInterpreter
open System.IO

[<EntryPoint>]
(*let main argv =
    let env = Map<Expr, Expr> [] //variable to section map
    if argv.Length <> 1 then
        printfn "Usage: dotnet run <program>"
        exit 1
    let ast_maybe = parse argv.[0]
    
    match ast_maybe with
    | Some ast ->
        let output = eval ast env
        use sw = new StreamWriter("output.xml")
        sw.WriteLine(output) 
    | None     ->
        printfn "Invalid program."
        exit 1 
    0

 to test parser only *)
let main argv =
    if argv.Length <> 1 then
        printfn "Usage: dotnet run <program>"
        exit 1
    let ast_maybe = parse argv.[0]
 
    match ast_maybe with
    | Some ast ->
        printfn "%A" ast
    | None     ->
        printfn "Invalid program."
        exit 1 
    0
