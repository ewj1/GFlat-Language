open GFlat
open System.IO

[<EntryPoint>]
let main argv =
    if argv.Length <> 1 then
        printfn "Usage: dotnet run <program>"
        exit 1
    let ast_maybe = parse argv.[0]
    match ast_maybe with
    | Some ast ->
        printfn "%s" (eval ast)
    | None -> printfn "bad sentence"
   (* match ast_maybe with
    | Some ast ->
        let output = eval ast
        use sw = new StreamWriter("output.svg")
        sw.WriteLine(output)
    | None     ->
        printfn "Invalid program."
        exit 1 *)
    
    0
