module Tests

open System
open System.Linq
open System.IO
open System.Text
open Microsoft.FSharp.Text.Lexing
open Xunit
open Lexer
open Parser
open AST

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

let firstTestStr = "STR X = \"str\";"
let secondTestStr = "IF (n == :one) THEN -> PRINT \"x\"; <-"
let thirdTestStr = "FUN INT funcName (INT n)"
let fourthTestStr = "FUN nonExistingType` funcName()"

[<Fact>]
let ``Lexer: STR X = "str";`` =
    Assert.Equal<List<token>>(
        firstTestStr |> parseTokens, 
        [STR; IDENTIFIER "X"; ASSIGNMENT; STRING "str"; SEMICOLON]
    )

[<Fact>]
let ``Lexer: IF (n == :one) THEN -> PRINT "x";`` =
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
let ``Lexer: FUN INT funcName (INT n)`` =
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
let ``Lexer: FUN nonExistingType funcName()`` =
    Assert.Throws<exn>(
        fun () -> fourthTestStr |> parseTokens |> ignore
    )
        
[<Fact>]
let ``Parser: STR X = "str";`` =
    Assert.Equal<Statement>(
        Parser.start Lexer.tokens (firstTestStr |> toLexBuff),
        [ NewVarAssignment { vartype = Str; identifier = "X"; value = Value (Value.String "str")}] 
    )
    
[<Fact>]
let ``Parser: IF (n == :one) THEN -> PRINT "x";`` =
    Assert.Equal<Statement>(
        Parser.start Lexer.tokens (secondTestStr |> toLexBuff),
        [ If {
            condition = Expression { op = Equals; left = Identifier "n"; right = Value (Value.String "str")};
            trueBranch = [ Print (PrintConsole (PrintSingle (Message "x"))) ];
            falseBranch = []
        }]
    )

//        
//let run =
//    ``Lexer: STR X = "str";``
//    ``Lexer: IF (n == :one) THEN -> PRINT "x";``