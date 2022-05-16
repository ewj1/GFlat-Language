namespace ProjectTests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open ProjParser

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.ParserTest  () =
        let input = "let Song = {Cmaj 1} play Song 1"
        let expected = "Program
  ([Assignment (Variable \"Song\", Section [Sound (Maj C, 1)])],
   Play [(Variable \"Song\", 1)])"

        
        let result = parse input
        match result with
        | Some ws ->
            Assert.AreEqual(expected, string ws)
        | None ->
            Assert.IsTrue false
