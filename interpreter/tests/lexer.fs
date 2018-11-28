[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverageAttribute>]
module lexer

open helpers
open Xunit
open Parser
open Lexer
open System.Diagnostics.CodeAnalysis

let firstTestStr = "STR X = \"str\";"
let secondTestStr = "IF (n == :one) THEN -> PRINT \"x\"; <-"
let thirdTestStr = "FUN INT funcName (INT n)"
let fourthTestStr = "FUN nonExistingType` funcName()"

[<ExcludeFromCodeCoverageAttribute>]
type lexer() =
    [<Fact>]
    let ``STR X = "str";`` () =
        Assert.Equal<List<token>>(
            firstTestStr |> parseTokens, 
            [STR; IDENTIFIER "X"; ASSIGNMENT; STRING "str"; SEMICOLON]
        )
    
    [<Fact>]
    let ``IF (n == :one) THEN -> PRINT "x";`` () =
        Assert.Equal<List<token>>(
            secondTestStr |> parseTokens,
            [
                IF; 
                LEFT_PARENS; 
                IDENTIFIER "n"; 
                EQUALS; 
                NUMBER 1; 
                RIGHT_PARENS;
                THEN;
                OPEN_BLOCK;
                PRINT;
                STRING "x";
                SEMICOLON;
                CLOSE_BLOCK
            ]
        )
    
    [<Fact>]
    let ``FUN INT funcName (INT n)`` () =
        Assert.Equal<List<token>>(
            thirdTestStr |> parseTokens,
            [
                FUN; 
                INT;
                IDENTIFIER "funcName"; 
                LEFT_PARENS;
                INT;
                IDENTIFIER "n";
                RIGHT_PARENS;
            ]
        )
        
    [<Fact>]
    let ``FUN nonExistingType funcName()`` () =
        Assert.Throws<exn>(
            fun () -> fourthTestStr |> parseTokens |> ignore
        )
    
    [<Fact>]
    let ``scope tokens`` () =
        Assert.Equal<List<token>>(
            " (<- ->)" |> parseTokens,
            [ LEFT_PARENS; CLOSE_BLOCK; OPEN_BLOCK; RIGHT_PARENS ]
        )
    
    [<Fact>]
    let ``operators`` () =
        Assert.Equal<List<token>>(
            " +-*/<>=++" |> parseTokens,
            [ ADD; SUBTRACT; MULTIPLY; DIVIDE; LESS_THAN; MORE_THAN; ASSIGNMENT; APPEND ]
        )
    
    [<Fact>]
    let ``operators2`` () =
        Assert.Equal<List<token>>(
            " AND OR != ==" |> parseTokens,
            [ LOGICAL_AND; LOGICAL_OR; NOT_EQUAL; EQUALS ]
        )
        