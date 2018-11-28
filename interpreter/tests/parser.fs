[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverageAttribute>]
module parser

open Xunit
open lexer
open helpers
open interpreter.AST
open System.Diagnostics.CodeAnalysis
        
[<ExcludeFromCodeCoverageAttribute>]
type parser() =
    [<Fact>]
    let ``STR X = "str";`` () =
        Assert.Equal<Statement>(
            Parser.start Lexer.tokens (firstTestStr |> toLexBuff),
            [ NewVarAssignment { vartype = Str; identifier = "X"; value = Value (Value.String "str")}] 
        )
    
    [<Fact>]
    let ``IF (n == :one) THEN -> PRINT "x";`` () =
        Assert.Equal<Statement>(
            Parser.start Lexer.tokens (secondTestStr |> toLexBuff),
            [ If {
                condition = Expression { op = Equals; left = Identifier "n"; right = Value (Value.Number 1)};
                trueBranch = [ Print (PrintConsole (PrintSingle (Message "x"))) ];
                falseBranch = []
            }]
        )
    