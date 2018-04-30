namespace interpreter

open System
open Parser
open Microsoft.FSharp.Text.Lexing
open interpreter.AST
 
module Entry = 

    [<EntryPoint>]
    let main argv =
        let parse json = 
            let lexbuf = LexBuffer<char>.FromString json
            let res = try
                        Parser.start Lexer.tokens lexbuf
                      with e ->
                        let pos = lexbuf.EndPos
                        let line = pos.Line
                        let column = pos.Column
                        let message = e.Message
                        let lastToken = new System.String(lexbuf.Lexeme)
                        printfn "Parse failed at line %d, column %d" line column
                        printfn "Last loken: %s" lastToken
                        printfn "Press any key to continue..."
                        System.Console.ReadLine() |> ignore
                        exit 1
            res

        
        let code =
            """
                INT xd = (-:zero:one:two:three:zero:one + :one) + (:two + :two);
                BOOL x2d = TRUE OR FALSE;
                xd = "SSSSS" - "xxddd";

                IF (TRUE OR FALSE) THEN ->
                    xd = "SSSSS" - "xxddd";
                <- ELSE ->
                   xd = "SSSSS" - "xxddd";
                <-

                WHILE (a > :four) ->
                    a = a - :one;
                <-

                PRINT XD;
                PRINT "LOL";

                FUN funcXD (INT i, BOOL b) ->   PRINT XD;<-
                FUN funcXD (INT i) ->   PRINT XD;<-
            """

        let parseResult = code |> parse
        printfn "%A" parseResult

        printfn "Press any key to continue..."
        System.Console.ReadLine() |> ignore
        0

    
