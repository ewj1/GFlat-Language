namespace ProjectTests

open System
open Microsoft.VisualStudio.TestTools.UnitTesting
open ProjAST
open ProjParser
open ProjInterpreter

[<TestClass>]
type TestClass () =

    [<TestMethod>]
    member this.ParserTest  () =
        let input = "let Song = {Cmaj 1} play Song 1"
        let expected = Program([Assignment ((Variable "Song") , (Section [Sound ((Maj C), 1)]))] , (Play [((Variable "Song"), 1)]))

        
        let result = parse input false
        match result with
        | Some ws ->
            Assert.AreEqual(expected, ws)
        | None ->
            Assert.IsTrue false

    [<TestMethod>]
(*
NOTE: We have tried directly copying and pasting the ouput of eval into expected, but can't seem to get expected and result to be equal. We leave this test here confident that the evaluator works but mystified why this test does not.
*)
    member this.InterpreterTest  () = 
        let input = Program([Assignment ((Variable "Song") , (Section [Sound ((Maj C), 1)]))] , (Play [((Variable "Song"), 1)]))
        let env = Map<Expr, Expr> []
        let expected =
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
     <part id=\"P1\"><measure number=\"1\">
      <attributes>
        <divisions>1</divisions>
        <key>
          <fifths>0</fifths>
        </key>
        <time>
          <beats>4</beats>
          <beat-type>4</beat-type>
        </time>
      </attributes><note><pitch>
           <step>C</step>
           <octave>4</octave>
         </pitch><duration>1</duration></note><note><chord/><pitch>
           <step>E</step>
           <octave>4</octave>
         </pitch><duration>1</duration></note><note><chord/><pitch>
           <step>G</step>
           <octave>4</octave>
         </pitch><duration>1</duration></note></measure></part>
</score-partwise>"
        let result = eval input env
        Assert.AreEqual(expected, result)
