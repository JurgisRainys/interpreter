module Interpreter

open AST
open System.Collections.Generic

type Var = {
    identifier: Identifier
    ``type``: Vartype
    value: Value
}

type Scope (statements: Statement list, vars: List<Var>, functions: List<Function>) =
    let identifierValue i = vars.Find(fun x -> x.identifier = i).value

    let rec functionCallValue (call: FunctionCall) = 
        let func = functions.Find(fun x -> x.name = call.identifier)
        let varsInFunctionScope = 
            call.arguments 
            |> List.map calculateExpr
            |> List.zip func.args
            |> List.map (fun (arg, value) -> { identifier = arg.identifier; ``type`` = arg.vartype; value = value })

        vars.AddRange(varsInFunctionScope)
        vars.Add({ identifier = "RETURN"; ``type`` = func.``type``; value = func.toReturn |> calculateExpr })

        let returned = Scope(func.body, new List<Var>(vars), new List<Function>(functions)).run
        
        vars.RemoveRange(vars.Count - varsInFunctionScope.Length - 1, varsInFunctionScope.Length + 1)
        match returned with
        | Some value -> value

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
        vars.Add(
            {
            identifier = assignment.identifier
            ``type`` = assignment.vartype
            value = (assignment.value |> calculateExpr) 
        })

    let handleVarAssignment (assignment: ExistingVarAssignment) =
        let varIdx = vars.FindIndex(fun x -> x.identifier = assignment.identifier)
        vars.[varIdx] <- { 
            identifier = vars.[varIdx].identifier
            ``type`` = vars.[varIdx].``type``
            value = (assignment.value |> calculateExpr)
        }

    let getValueOfCondition (boolExpr: Expression) =
        match boolExpr |> calculateExpr with
        | Boolean cond -> cond

    let handleIf (ifCond: If) =
        let cond = getValueOfCondition ifCond.condition
        if cond = true then Scope(ifCond.trueBranch, new List<Var>(vars), new List<Function>(functions)).run |> ignore
        else Scope(ifCond.falseBranch, new List<Var>(vars), new List<Function>(functions)).run |> ignore

    let handleWhile (whileLoop: While) =
        let xx = vars
        let mutable cond = getValueOfCondition whileLoop.condition
        let vars = new List<Var>(vars)
        let funcs = new List<Function>(functions)
        while (cond) do 
            Scope(whileLoop.body, vars, funcs).run 
            cond <- getValueOfCondition whileLoop.condition
            
    let handlePrintLine (p: PrintLine) =
        match p with
        | PrintLine.Message msg -> printfn "%s" msg
        | PrintLine.Variable ident -> printfn "%A" (vars.Find(fun x -> x.identifier = ident).value)

    let handlePrint (p: Print) =
        match p with
        | Print.Message msg -> printf "%s" msg
        | Print.Variable ident -> printf "%A" (vars.Find(fun x -> x.identifier = ident).value)

    let handleFunctionDeclaration (f: Function) =
        functions.Add(f)

    let executeStatement statement =
        match statement with 
        | NewVarAssignment x -> handleNewVarAssignment x
        | ExistingVarAssignment x -> handleVarAssignment x
        | If x -> handleIf x
        | While x -> handleWhile x
        | Print x -> handlePrint x
        | PrintLine x -> handlePrintLine x
        | Function x -> handleFunctionDeclaration x

    member this.run: Value option =
        statements
        |> List.iter executeStatement

        let v = vars
        let f = functions
        vars |> List.ofSeq |> List.tryFind (fun x -> x.identifier = "RETURN") |> Option.map (fun x -> x.value)

let interpret (ast: Statement list) =
    let scope = Scope (ast, new List<Var>(), new List<Function>())
    scope.run |> ignore