﻿[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverageAttribute>]
module Executor

open System.IO
open Microsoft.FSharp.Text.Lexing

let path = "..\\..\\program-logic\\Program.xd"

let parse str = 
    let lexbuf = LexBuffer<char>.FromString str
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

let interpret (code: string) = 
    code
    |> parse
    |> Semantics.analyze 
    |> Option.map Interpreter.interpret
    |> ignore

let run () =
    let code = 
        if (File.Exists path) then File.ReadAllText path
        else ""

    code |> interpret
        
    printfn "\nPress any key to continue..."
    System.Console.ReadLine() |> ignore
    0

