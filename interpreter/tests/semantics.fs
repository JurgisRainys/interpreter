[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverageAttribute>]
module semantics

open Xunit

let analyze (code: string) = 
    code
    |> _interpreter.parse
    |> Semantics.analyze 

[<Fact>]
let ``failing case 1`` () =
    Assert.Equal(
        None, 
        "INT X = \"s\";" |> analyze
    )

[<Fact>]
let ``failing case 2`` () =
    Assert.Equal(
        None, 
        "IF (:one) THEN -> INT X = \"s\"; <- ELSE -> PRINTLN X; <-"|> analyze
    )

[<Fact>]
let ``failing case 3`` () =
    Assert.Equal(
        None, 
        "FUN INT s() -> RETURN \"string\"; <-"|> analyze
    )

[<Fact>]
let ``failing case 4`` () =
    Assert.Equal(
        None, 
        "WHILE (\"s\")-> PRINTLN \"string\"; <-"|> analyze
    )

[<Fact>]
let ``failing case 5`` () =
    Assert.Equal(
        None, 
        "WHILE (\"s\")-> PRINTLN X; <-"|> analyze
    )

[<Fact>]
let ``failing case 6`` () =
    Assert.Equal(
        None, 
        "STR X = :one;" |> analyze
    )

[<Fact>]
let ``failing case 7`` () =
    Assert.Equal(
        None, 
        """
            INT X = :one;
            INT X = :one + :one;
            INT X = :one - TRUE;
            INT X = TRUE * TRUE;
            INT X = TRUE / "s";
            INT X = "s" > "s";
            INT X = :one ++ "s";
        """ |> analyze
    )