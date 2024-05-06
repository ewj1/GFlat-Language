open ProjAST
open ProjParser
open ProjInterpreter
open System.IO

[<EntryPoint>]
let main argv =
    let env = Map<Expr, Expr> [] //variable to section map

(* Check for proper usage *)
    if argv.Length <> 1 && argv.Length <> 2 then
        printfn "Usage: dotnet run <file> [debug]"
        exit 1

    (* read in the input file *)
    let file = argv.[0]
    if file.[file.Length-6..file.Length-1] <> ".gflat" then
        printfn "file must end with .gflat extension"
        exit 1
    let input = File.ReadAllText file

    (* does the user want parser debugging turned on? *)
    let do_debug = if argv.Length = 2 then true else false

    (* try to parse what they gave us *)
    let ast_maybe = parse input do_debug
    
    match ast_maybe with
    | Some ast ->
        let output = eval ast env
        use sw = new StreamWriter(file.[..file.Length-7] + ".xml")
        sw.WriteLine(output)
        printfn "%s" ("Success: outputted XML file in " + file.[..file.Length-7] + ".xml")
    | None     ->
        printfn "Invalid program."
        exit 1 
    0
