module Interpreter

open AST
open System.Collections.Generic
open System.Linq

type Var = {
    identifier: Identifier
    ``type``: Vartype
    value: Value
}

type VarName = Identifier
type FuncName = Identifier

type VarValue = {
    vartype: Vartype
    value: Value
}

type Scope (statements: Statement list,
            vars: Map<VarName, VarValue>,
            funcs: Map<FuncName, Function>,
            higherScopeVars: Map<VarName, VarValue>,
            higherScopeFuncs: Map<FuncName, Function>)=

    //let analyzeBlock (statements: Statement list) (vars: List<Var>) (functions: List<Function>) =
    //    let varsBeforeBlock = vars.Count;
    //    let funcsBeforeBlock = functions.Count;
    //    let toReturn = Scope(statements, vars, functions).run
    //    if vars.Count <> 0 then vars.RemoveRange(varsBeforeBlock - 1, vars.Count - varsBeforeBlock) 
    //    if functions.Count <> 0 then functions.RemoveRange(funcsBeforeBlock - 1, functions.Count - funcsBeforeBlock) 
    //    toReturn

    //let identifierValue i = vars.Find(fun x -> x.identifier = i).value

    ////let rec functionCallValue (call: FunctionCall) = 
    ////    let func = functions.Find(fun x -> x.name = call.identifier)
    ////    let varsInFunctionScope = 
    ////        call.arguments 
    ////        |> List.map calculateExpr
    ////        |> List.zip func.args
    ////        |> List.map (fun (arg, value) -> { identifier = arg.identifier; ``type`` = arg.vartype; value = value })
    ////
    ////    let funcsInScope = 
    ////        func.body 
    ////        |> List.fold (fun acc x ->
    ////            match x with
    ////            | Function ff -> ff :: acc
    ////            | _ -> acc
    ////        ) []
    ////
    ////    varsInFunctionScope 
    ////    |> List.iter (fun x -> 
    ////        let ind = 
    ////            vars 
    ////            |> List.ofSeq
    ////            |> List.tryFindIndex (fun var -> var.identifier = x.identifier)
    ////        match ind with
    ////        | None -> vars.Add(x)
    ////        | Some i -> vars.[i] = x |> ignore
    ////    )
    ////
    ////    funcsInScope 
    ////    |> List.iter (fun x -> 
    ////        let ind = 
    ////            functions 
    ////            |> List.ofSeq
    ////            |> List.tryFindIndex (fun f -> f.name = x.name)
    ////        match ind with
    ////        | None -> functions.Add(x)
    ////        | Some i -> functions.[i] = x |> ignore
    ////    )
    ////
    ////    let returned = Scope(func.body, vars, functions).run
    ////    let xx = func.toReturn |> calculateExpr
    ////
    ////    vars.RemoveRange(vars.Count - varsInFunctionScope.Length - 1, varsInFunctionScope.Length + 1)
    ////    funcsInScope |> List.iter (fun x -> functions.Remove(x) |> ignore)
    ////    match returned with
    ////    | Some value -> value
    //let rec functionCallValue (call: FunctionCall) = 
    //    let func = functions.Find(fun x -> x.name = call.identifier)
    //    let x: Function option = None

    //    let toReturn =  { NewVarAssignment.identifier = "RETURN"; value = func.toReturn; vartype = func.``type`` }
    //    let funcBodyWithReturn = { func with body = toReturn :: func.body }





        
    //and operationValue (o: Operation): Value = 
    //    let (leftVal: Value, rightVal: Value) = o.left |> calculateExpr, o.right |> calculateExpr
        
    //    match leftVal, rightVal with
    //    | Number n1, Number n2 -> 
    //        match o.op with
    //        | Add           -> Number (n1 + n2)
    //        | Subtract      -> Number (n1 - n2)
    //        | Multiply      -> Number (n1 * n2)
    //        | Divide        -> Number (n1 / n2)
    //        | MoreThan      -> Boolean (n1 > n2)
    //        | LessThan      -> Boolean (n1 < n2)
    //        | Equals        -> Boolean (n1 = n2)
    //        | NotEqual      -> Boolean (n1 <> n2)
    //    | Boolean b1, Boolean b2 ->
    //        match o.op with
    //        | MoreThan      -> Boolean (b1 > b2)
    //        | LessThan      -> Boolean (b1 < b2)
    //        | LogicalOr     -> Boolean (b1 || b2)
    //        | LogicalAnd    -> Boolean (b1 && b2)
    //        | Equals        -> Boolean (b1 = b2)
    //        | NotEqual      -> Boolean (b1 <> b2)
    //    | String b1, String b2 -> String (b1 + b2)

    //and calculateExpr expr : Value =
    //    match expr with
    //    | Value v -> v
    //    | Identifier i -> identifierValue i
    //    | Expression o -> operationValue o                
    //    | FunctionCall c -> functionCallValue c
    
    //let handleNewVarAssignment (assignment: NewVarAssignment) =
    //    vars.Add(
    //        {
    //        identifier = assignment.identifier
    //        ``type`` = assignment.vartype
    //        value = (assignment.value |> calculateExpr) 
    //    })

    //let handleVarAssignment (assignment: ExistingVarAssignment) =
    //    let varIdx = vars.FindIndex(fun x -> x.identifier = assignment.identifier)
    //    vars.[varIdx] <- { 
    //        identifier = vars.[varIdx].identifier
    //        ``type`` = vars.[varIdx].``type``
    //        value = (assignment.value |> calculateExpr)
    //    }

    //let getValueOfCondition (boolExpr: Expression) =
    //    match boolExpr |> calculateExpr with
    //    | Boolean cond -> cond

    //let handleIf (ifCond: If) =
    //    let cond = getValueOfCondition ifCond.condition
    //    if cond = true then analyzeBlock ifCond.trueBranch vars functions
    //    //Scope(ifCond.trueBranch, new List<Var>(vars), new List<Function>(functions)).run |> ignore
    //    else analyzeBlock ifCond.falseBranch vars functions
    //    //Scope(ifCond.falseBranch, new List<Var>(vars), new List<Function>(functions)).run |> ignore

    //let handleWhile (whileLoop: While) =
    //    let xx = vars
    //    let mutable cond = getValueOfCondition whileLoop.condition
    //    let varsBeforeCount = vars.Count
    //    let funcBeforeCount = functions.Count

    //    while (cond) do 
    //        Scope(whileLoop.body, vars, functions).run |> ignore
    //        cond <- getValueOfCondition whileLoop.condition

    //    if vars.Count <> 0 then vars.RemoveRange(varsBeforeCount - 1, vars.Count - varsBeforeCount) 
    //    if functions.Count <> 0 then functions.RemoveRange(funcBeforeCount - 1, functions.Count - varsBeforeCount) 
            
    //let handlePrintLine (p: PrintLine) =
    //    match p with
    //    | PrintLine.Message msg -> printfn "%s" msg
    //    | PrintLine.Variable ident -> printfn "%A" (vars.Find(fun x -> x.identifier = ident).value)

    //let handlePrint (p: Print) =
    //    match p with
    //    | Print.Message msg -> printf "%s" msg
    //    | Print.Variable ident -> printf "%A" (vars.Find(fun x -> x.identifier = ident).value)

    //let handleFunctionDeclaration (f: Function) =
    //    functions.Add(f)

    //let executeStatement statement =
    //    match statement with 
    //    | NewVarAssignment x -> handleNewVarAssignment x
    //    | ExistingVarAssignment x -> handleVarAssignment x
    //    | If x -> handleIf x |> ignore
    //    | While x -> handleWhile x
    //    | Print x -> handlePrint x
    //    | PrintLine x -> handlePrintLine x
    //    | Function x -> handleFunctionDeclaration x

    //member this.run: Value option =
    //    statements
    //    |> List.iter executeStatement

    //    let v = vars
    //    let f = functions
    //    vars |> List.ofSeq |> List.tryFind (fun x -> x.identifier = "RETURN") |> Option.map (fun x -> x.value)

    let combineCurrentWithParent (parentScopeMap: Map<'a, 'b>) (currentScopeMap: Map<'a, 'b>): Map<'a, 'b> =
        currentScopeMap |> Map.fold (fun acc name value -> acc |> Map.add name value) parentScopeMap

    member this.childScope statements childScopeVars childScopeFuncs =
        let newHigherScopeVars = combineCurrentWithParent higherScopeVars vars
        let newHigherScopeFuncs = combineCurrentWithParent higherScopeFuncs funcs
        Scope (statements, childScopeVars, childScopeFuncs, newHigherScopeVars, newHigherScopeFuncs)

    new ast = Scope(ast, Map.empty, Map.empty, Map.empty, Map.empty)

let interpret (ast: Statement list) =
    let scope = Scope (ast)
    //scope.run |> ignore
    ignore
