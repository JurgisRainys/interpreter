﻿namespace interpreter
 
open System.IO
open Microsoft.FSharp.Text.Lexing

module Entry = 
    let path = "..\\..\\program-logic\\Program.xd"

    [<EntryPoint>]
    let main argv =
        let parse json = 
            let lexbuf = LexBuffer<char>.FromString json
            let res = 
                try
                    Parser.start Lexer.tokens lexbuf
                with e ->
                    let pos = lexbuf.EndPos
                    let line = pos.Line + 1
                    let column = pos.Column
                    let message = e.Message
                    let lastToken = new System.String(lexbuf.Lexeme)
                    printfn "Parse failed at line %d, column %d" line column
                    printfn "Last token: %s" (lastToken + "; Pries sita tokena kazkur klaida.")
                    printfn "Press any key to continue..."
                    System.Console.ReadLine() |> ignore
                    exit 1
            res
        
        let code = 
            if (File.Exists path) then File.ReadAllText path
            else ""

        code 
        |> parse
        |> Semantics.analyze 
        |> Option.map Interpreter.interpret

        printfn "Press any key to continue..."
        System.Console.ReadLine() |> ignore
        0
