module UnitTests

open Lexer
open Parser
open System.Linq
open System.IO
open System.Text
open Microsoft.FSharp.Text.Lexing
open Xunit
open Xunit

let toLexBuff str = LexBuffer<char>.FromString str

let rec parseTokensAny
    (acc: List<token>) 
    (parser: LexBuffer<char> -> token) 
    (buffer: LexBuffer<char>) = 
        let token = buffer |> tokens
        match token with 
        | EOF -> acc |> List.rev
        | _ -> parseTokensAny(token :: acc) parser buffer 
     
let parseTokens s = s |> toLexBuff |> parseTokensAny [] Lexer.tokens

let rec compareLists list1 list2 =
    if Seq.compareWith Operators.compare list1 list2 = 0 then true else false

let testLists list1 list2 = 
    try 
        let notEqual = 
            List.zip list1 list2 
            |> List.exists (fun (first, second) -> first <> second)
        not notEqual
    with e -> false

[<Fact>]
let ``Lexer: STR X = "str";`` =
    Assert.Equal<List<token>>(
        "STR X = \"str\";" |> parseTokens, 
        [STR; IDENTIFIER "X"; ASSIGNMENT; STRING "str"; SEMICOLON]
    )

[<Fact>]
let ``Lexer: IF (n == :one) THEN -> PRINT "x";`` =
    Assert.Equal<List<token>>(
        "IF (n == :one) THEN -> PRINT \"x\"; <-" |> parseTokens,
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
let ``Lexer: FUN INT funcName (INT n)`` =
    Assert.Equal<List<token>>(
        "FUN INT funcName (INT n)" |> parseTokens,
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
let ``Lexer: FUN nonExistingType funcName()`` =
    Assert.Equal<List<token>>(
        "FUN nonExistingType funcName()" |> parseTokens,
        [
            FUN; 
            IDENTIFIER "nonExistingType";
            IDENTIFIER "funcName";
            LEFT_PARENS; 
            RIGHT_PARENS
        ]
    )
        
let run =
    ``Lexer: STR X = "str";``
    ``Lexer: IF (n == :one) THEN -> PRINT "x";``