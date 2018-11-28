[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverageAttribute>]
module helpers

open Microsoft.FSharp.Text.Lexing
open Lexer
open Parser

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
