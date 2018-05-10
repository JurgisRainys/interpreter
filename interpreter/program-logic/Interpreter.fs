//module Interpreter

//open AST

//let executeStatement statement =
//    match statement with 
//    | NewVarAssignment x -> checkNewVarAssignment x
//    | ExistingVarAssignment x -> checkExistingVarAssignment x
//    | If x -> checkIf x
//    | Block x -> Analysis(x).run |> mergeAllErrors
//    | While x -> checkWhile x
//    | Print x -> Right Int
//    | Function x -> checkFunction x

//let interpret (ast: Statement list) =
//    ast 
//    |> List.iter executeStatement