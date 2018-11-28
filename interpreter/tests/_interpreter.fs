[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverageAttribute>]
module _interpreter

open System.IO
open Xunit
open System
open Microsoft.FSharp.Text.Lexing

let parse str = 
    let lexbuf = LexBuffer<char>.FromString str
    let res = 
        try
            Parser.start Lexer.tokens lexbuf
        with e ->
            let pos = lexbuf.EndPos
            let line = pos.Line + 1
            let column = pos.Column
            let lastToken = new System.String(lexbuf.Lexeme)
            raise (
                System.Exception(
                    String.Format(
                        "Parse failed at line {0}, column {1}. Right before token: {2}", 
                        line, 
                        column,
                        lastToken
                    )
                )
            )
    res

let interpret (code: string) = 
    code
    |> parse
    |> Semantics.analyze 
    |> Option.map Interpreter.interpret
    |> ignore

let readFileCode (path: string) = 
    if (File.Exists path) then Some (File.ReadAllText path)
    else None

let fileStr (filename: string) (varToPrint: string) = 
    String.Format("FILE OVERWRITE \"./testData/{0}\" PRINT {1};", filename, varToPrint)

let runAndGetOutput (code: string) =
    let filename = Guid.NewGuid().ToString()
    interpret (String.Format("{0}\r\n{1}", code, fileStr filename "X"))
    readFileCode ("./testData/" + filename) 

[<Fact>]
let ``-12 + 35 should be 23`` () =
    Assert.Equal(
        Some "Number 23", 
        "INT X = -:one:two + :three:five;" |> runAndGetOutput
    )

[<Fact>]
let ``-12 - 35 should be -47`` () =
    Assert.Equal(
        Some "Number -47", 
        "INT X = -:one:two - :three:five;" |> runAndGetOutput
    )

[<Fact>]
let ``-12 * 5 should be -60`` () =
    Assert.Equal(
        Some "Number -60", 
        "INT X = -:one:two * :five;" |> runAndGetOutput
    )

[<Fact>]
let ``-12 / 2 should be -6`` () =
    Assert.Equal(
        Some "Number -6", 
        "INT X = -:one:two / :two;" |> runAndGetOutput
    )

[<Fact>]
let ``-12 > 2 should be -6`` () =
    Assert.Equal(
        Some "Boolean false", 
        "BOOL X = -:one:two > :two;" |> runAndGetOutput
    )

[<Fact>]
let ``-12 < 2 should be -6`` () =
    Assert.Equal(
        Some "Boolean true", 
        "BOOL X = -:one:two < :two;" |> runAndGetOutput
    )

[<Fact>]
let ``-12 == 2 should be -6`` () =
    Assert.Equal(
        Some "Boolean false", 
        "BOOL X = -:one:two == :two;" |> runAndGetOutput
    )

[<Fact>]
let ``-12 != 2 should be -6`` () =
    Assert.Equal(
        Some "Boolean true", 
        "BOOL X = -:one:two != :two;" |> runAndGetOutput
    )

[<Fact>]
let ``-1 > 1 || 2 > 1 should be -6`` () =
    Assert.Equal(
        Some "Boolean true", 
        "BOOL X = (-:one > :one) OR (:two > :one);" |> runAndGetOutput
    )

[<Fact>]
let ``TRUE || FALSE`` () =
    Assert.Equal(
        Some "Boolean true", 
        "BOOL X = TRUE OR FALSE;" |> runAndGetOutput
    )

[<Fact>]
let ``TRUE && FALSE`` () =
    Assert.Equal(
        Some "Boolean false", 
        "BOOL X = TRUE AND FALSE;" |> runAndGetOutput
    )

[<Fact>]
let ``FALSE || FALSE`` () =
    Assert.Equal(
        Some "Boolean false", 
        "BOOL X = FALSE OR FALSE;" |> runAndGetOutput
    )
[<Fact>]
let ``1 > 35`` () =
    Assert.Equal(
        Some "Boolean false", 
        "BOOL X = :one > :three:five;" |> runAndGetOutput
    )

[<Fact>]
let ``if conditional, then branch`` () =
    Assert.Equal(
        Some "String \"true\"", 
        """
            STR X = "";
            IF (TRUE) THEN -> X = "true"; <- ELSE -> X = "false"; <-
        """ |> runAndGetOutput
    )

[<Fact>]
let ``if conditional, else branch`` () =
    Assert.Equal(
        Some "String \"false\"", 
        """
            STR X = "";
            IF (FALSE) THEN -> X = "true"; <- ELSE -> X = "false"; <-
        """ |> runAndGetOutput
    )

[<Fact>]
let ``while loop`` () =
    Assert.Equal(
        Some "Number 20", 
        """
            INT X = :zero;
            WHILE (X < :two:zero) -> X = X + :one; <-
        """ |> runAndGetOutput
    )

[<Fact>]
let ``while loop inner var`` () =
    Assert.Equal(
        Some "Number 20", 
        """
            INT X = :zero;
            WHILE (X < :two:zero) -> 
                INT C = :one;
                X = X + C; 
            <-
        """ |> runAndGetOutput
    )

[<Fact>]
let ``function test`` () =
    Assert.Equal(
        Some "Number 5",
        """
            FUN INT five () -> 
                RETURN :five;
            <-
            INT X = five();
        """ |> runAndGetOutput
    )

[<Fact>]
let ``function nest test`` () =
    Assert.Equal(
        Some "Number 6",
        """
            FUN INT five (INT X) -> 
                FUN INT six(INT F) ->
                    INT X = :one;
                    RETURN X + F;
                <-

                RETURN six(X);
            <-
            INT X = five(:five);
        """ |> runAndGetOutput
    )
