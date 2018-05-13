﻿module Interpreter

open AST
open System.Collections.Generic
open System.Linq

//type Var = {
//    identifier: Identifier
//    ``type``: Vartype
//    value: Value
//}

type VarName = Identifier
type FuncName = Identifier

type VarValue = {
    vartype: Vartype
    value: Value
}

let newDictionary<'a, 'b when 'a : equality> = new Dictionary<'a, 'b>()

let copyDictionaryByValue (dictionary: Dictionary<'a, 'b>) =
    let acc = new Dictionary<'a, 'b>()
    dictionary
    |> Seq.map ``|KeyValue|``
    |> Seq.iter (fun (key, value) ->
        acc.[key] <- value
    ) 
    acc

type Scope (statements: Statement list,
            vars: Dictionary<VarName, VarValue>,
            funcs: Dictionary<FuncName, Function>,
            parentScope: Scope option) as this =

    let walkBlock (statements: Statement list) =
        let x = Scope(statements, newDictionary, newDictionary, Some this).run
        x

    let getVarValue identifier =
        match this.getVarValue identifier with
        | Some value -> value

    let getFunc name =
        match this.getFunc name with
        | Some value -> value

    let tryGetVarValue identifier =
        try Some (getVarValue identifier)
        with _ -> None

    let tryGetFunc name =
        try Some (getFunc name)
        with _ -> None
       
    //overwrites parent values with subscope values
    let combineCurrentWithParent (parentScopeDictionary: Dictionary<'a, 'b>) (currentScopeDictionary: Dictionary<'a, 'b>): Dictionary<'a, 'b> =
        let parent = parentScopeDictionary |> copyDictionaryByValue
        currentScopeDictionary
        |> Seq.map ``|KeyValue|``
        |> Seq.iter (fun (key, value) ->
            parent.[key] <- value
        )
        parent

    let rec walkFunc (f: Function) (providedArguments: Expression list) =
        let innerScopeVars = 
            providedArguments
            |> List.map calculateExpr
            |> List.zip f.args
            |> List.fold (fun acc (arg, value) -> 
                acc.[arg.identifier] <- { vartype = arg.vartype; value = value }
                acc
            ) newDictionary: Dictionary<VarName, VarValue>

        let innerScopeFuncs = newDictionary |> fun x -> x.Add(f.name, f); x

        let bodyWithReturnStatement = 
            f.body @ [ NewVarAssignment { 
                identifier = "RETURN"; 
                vartype = f.``type``; 
                value = f.toReturn } 
            ]

        let funcReturned = 
            Scope(bodyWithReturnStatement,
                    innerScopeVars, 
                    innerScopeFuncs, 
                    Some this).run

        match funcReturned with
        | Some value -> value

    and functionCallValue (call: FunctionCall) = 
        let func = getFunc call.identifier
        walkFunc func call.arguments
        
    and identifierValue i = (getVarValue i).value

    and operationValue (o: Operation): Value = 
        let (leftVal: Value, rightVal: Value) = o.left |> calculateExpr, o.right |> calculateExpr
        
        match leftVal, rightVal with
        | Number n1, Number n2 -> 
            match o.op with
            | Add           -> Number (n1 + n2)
            | Subtract      -> Number (n1 - n2)
            | Multiply      -> Number (n1 * n2)
            | Divide        -> Number (n1 / n2)
            | MoreThan      -> Boolean (n1 > n2)
            | LessThan      -> Boolean (n1 < n2)
            | Equals        -> Boolean (n1 = n2)
            | NotEqual      -> Boolean (n1 <> n2)
        | Boolean b1, Boolean b2 ->
            match o.op with
            | MoreThan      -> Boolean (b1 > b2)
            | LessThan      -> Boolean (b1 < b2)
            | LogicalOr     -> Boolean (b1 || b2)
            | LogicalAnd    -> Boolean (b1 && b2)
            | Equals        -> Boolean (b1 = b2)
            | NotEqual      -> Boolean (b1 <> b2)
        | String b1, String b2 -> String (b1 + b2)

    and calculateExpr expr : Value =
        match expr with
        | Value v -> v
        | Identifier i -> identifierValue i
        | Expression o -> operationValue o                
        | FunctionCall c -> functionCallValue c
    
    let handleNewVarAssignment (assignment: NewVarAssignment) =
        vars.[assignment.identifier] <- {
            vartype = assignment.vartype
            value = (assignment.value |> calculateExpr) 
        }

    let handleVarAssignment (assignment: ExistingVarAssignment) =
        this.setVarValue assignment.identifier (assignment.value |> calculateExpr)

    let getValueOfCondition (boolExpr: Expression) =
        match boolExpr |> calculateExpr with
        | Boolean cond -> cond

    let handleIf (ifCond: If) =
        let cond = getValueOfCondition ifCond.condition
        if cond then walkBlock ifCond.trueBranch 
        else walkBlock ifCond.falseBranch 

    let handleWhile (whileLoop: While) =
        let mutable cond = getValueOfCondition whileLoop.condition

        while (cond) do 
            walkBlock whileLoop.body |> ignore
            cond <- getValueOfCondition whileLoop.condition


    let handlePrintLine (p: PrintLine) =
        match p with
        | PrintLine.Message msg -> printfn "%s" msg
        | PrintLine.Variable ident -> printfn "%A" ((getVarValue ident).value)

    let handlePrint (p: Print) =
        match p with
        | Print.Message msg -> printf "%s" msg
        | Print.Variable ident -> printf "%A" ((getVarValue ident).value)

    let handleFunctionDeclaration (f: Function) =
        funcs.[f.name] <- f

    let executeStatement statement =
        match statement with 
        | NewVarAssignment x -> handleNewVarAssignment x
        | ExistingVarAssignment x -> handleVarAssignment x
        | If x -> handleIf x |> ignore
        | While x -> handleWhile x
        | Print x -> handlePrint x
        | PrintLine x -> handlePrintLine x
        | Function x -> handleFunctionDeclaration x

    member private this.setVarValue identifier value =
        if vars.ContainsKey identifier then
            vars.[identifier] <- {
                vartype = vars.[identifier].vartype
                value = value
            }
        else 
            parentScope 
            |> Option.map (fun x -> x.setVarValue identifier value) 
            |> ignore

    member private this.getVarValue identifier =
        if vars.ContainsKey identifier then Some vars.[identifier]
        else 
            parentScope 
            |> Option.bind (fun scope -> scope.getVarValue identifier)

    member private this.getFunc name =
        if funcs.ContainsKey name then Some funcs.[name]
        else 
            parentScope 
            |> Option.bind (fun scope -> scope.getFunc name)

    member this.run: Value option =
        statements
        |> List.iter executeStatement
        tryGetVarValue "RETURN" |> Option.map (fun x -> x.value)

    new ast = Scope(ast, newDictionary, newDictionary, None)

let interpret (ast: Statement list) =
    let scope = Scope ast
    scope.run |> ignore
