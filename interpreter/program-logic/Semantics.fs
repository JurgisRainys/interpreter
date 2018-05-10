module Semantics

open AST
open System.Collections.Generic
open System.Linq
open Either

type Var = {
    vartype: Vartype
    identifier: Identifier
}

type Fun = {
    ``type``: Vartype
    identifier: Identifier 
    args: FunArg list
}

type SemanticError = string

// realiai vartype option turetu grazint, bet tingiu apsikraut, nes belenkiek papildomo matchinimo reikes.
type AnalysisResult = Either<SemanticError, Vartype> 

type Analysis (ast: Statement list, vars: List<Var>, functions: List<Fun>, nestLevel: int) =
    let isNumOperator op =
        match op with
        | Add | Subtract | Divide | Multiply -> true
        | _ -> false

    let isBoolOperator op =
        match op with
        | MoreThan | LessThan | LogicalAnd 
        | LogicalOr | Equals | NotEqual -> true
        | _ -> false

    let isStringOperator op = if (op = Append) then true else false

    let operatorType op = 
        if isNumOperator op then Int
        else if isStringOperator op then Str
        else Bool

    let valueType v =
        match v with
        | String _ -> Str
        | Number _ -> Int
        | Boolean _ -> Bool

    let mergeAllErrors (resultList: AnalysisResult list): AnalysisResult =
        resultList 
        |> List.choose (fun x -> 
            match x with
            | Left err -> Some err
            | _ -> None
        )
        |> fun errorList -> 
            if errorList.Length = 0 then Right Int
            else Left (errorList |> List.fold (fun acc error -> acc + "/n" + error) "")

    let rec funcCallType (expr: FunctionCall) : AnalysisResult =
        if (functions.Select(fun x -> x.identifier).Contains(expr.identifier)) then
            let { Fun.args = args; ``type`` = funType } = functions.Find(fun x -> x.identifier = expr.identifier)   
            if args.Length <> expr.arguments.Length then 
                Left("Function was supplied wrong number of arguments. Function name: " + expr.identifier)
            else 
                let results =
                    args
                    |> List.map (fun x -> x.vartype)
                    |> List.zip(expr.arguments)
                    |> List.map (fun (suppliedArg, expectedType) ->
                        match expressionType suppliedArg with
                        | Left _ as err -> err
                        | Right ``type`` ->
                            if ``type`` = expectedType then Right ``type``
                            else Left ("Argument supplied to function has wrong type. Func name: " + expr.identifier 
                            + "; expected type: " + expectedType.ToString() + "; found: " + ``type``.ToString())
                    )
                    |> mergeAllErrors
                match results with
                | Left _ as err -> err
                | Right _ -> Right funType
        else Left("Usage of undefined function. Function name: " + expr.identifier.ToString())

    and identifierType i =
        let ident = vars.FirstOrDefault(fun x -> x.identifier = i)
        if obj.ReferenceEquals(ident, null) then 
            Left ("No such identifier declared previously. Identifier: " + i + "\n") 
        else Right (ident.vartype)

    and operationType o =
        match (expressionType(o.left), expressionType(o.right), operatorType(o.op)) with
        | Right type1, Right type2, oType when type1 = type2 && type1 = oType -> Right (type1)
        | (Left _ as err), _, _ | _, (Left _ as err), _-> err
        | Right type1, Right type2, oType when type1 = type2 && oType = Bool -> Right (oType) // kai boolai komparinami
        | Right type1, Right type2, _ -> 
            Left ("Operator cannot be applied to given expressions. Expression types: (" 
            + type1.ToString() + ", " + type2.ToString() + "); Operation: " + o.op.ToString())

    and expressionType expr : AnalysisResult =
        match expr with
        | Value v -> Right (valueType v)
        | Identifier i -> identifierType i
        | Expression o -> operationType o                
        | FunctionCall c -> funcCallType c

    let checkNewVarAssignment (newA: NewVarAssignment) = 
        match expressionType(newA.value) with
        | Left _ as err -> err
        | Right ``type`` when ``type`` = newA.vartype -> 
            if vars.Select(fun x -> x.identifier).Contains(newA.identifier) then Left ("Identifier already declared. Identifier: " + newA.identifier)
            else vars.Add({ vartype = newA.vartype; identifier = newA.identifier }); Right (``type``)
        | Right ``type`` -> Left ("Expression evaluates to different type than var declaration states. Vartype: " + newA.vartype.ToString() + "; Identifier: " + newA.identifier + "; Expression type: " + ``type``.ToString())

    let checkExistingVarAssignment (exsA: ExistingVarAssignment) =
        match expressionType(exsA.value) with
        | Left _ as err -> err
        | Right tyyy -> 
            if vars.Contains({ vartype = tyyy; identifier = exsA.identifier }) then Right (tyyy)
            else Left ("Identifier is either not declared or stores different type of value. Identifier: " + exsA.identifier)
        
    // nestlevelis padaro, kad graziai erroras formuojamas butu, kai nestinami blokai
    let analyzeBlock (ast: Statement list) (varsInScope: List<Var>) (functionsInScope: List<Fun>): AnalysisResult =
        //let results = Analysis(ast, varsInScope).run |> List.mapi (fun i res -> i + 1, res ) |> List.filter (fun (i, res) -> res |> Either.isLeft)
        let results = 
            Analysis(ast, new List<Var>(varsInScope), new List<Fun>(functionsInScope), nestLevel + 1).run 
            |> List.mapi (fun i res -> i + 1, res ) 
            |> List.filter (fun (i, res) -> res |> Either.isLeft)
        match results with
        | [] -> Right(Int) // Turetu grazinti `None` optiona
        | errors -> 
            let xx = Enumerable.Repeat (' ', (nestLevel + 1) * 2) |> List.ofSeq
            let indentation = xx  |> List.fold (fun acc _ -> acc + " ") "" // gaidiskai veikia
            errors |> List.fold (fun acc (i, err) -> match err with | Left msg -> acc + "\n" + indentation + "Statement " + i.ToString() + ": " + msg) "" |> Left

    let checkIf (ifCond: If) = 
        match expressionType ifCond.condition with
        | Left _ as err -> err
        | Right ``type`` when ``type`` <> Bool -> Left ("If condition should be a boolean type expression. This expression evaluates to: " + ``type``.ToString())
        | Right _ -> 
            let trueBrErrors = analyzeBlock ifCond.trueBranch vars functions
            let falseBrErrors = analyzeBlock ifCond.falseBranch vars functions

            match trueBrErrors, falseBrErrors with
            | Right _, Right _ -> Right (Int) // tai turetu grazint `None`, bet ne optionas
            | Right _, (Left _ as err) | (Left _ as err), Right _ -> err
            | Left err1, Left err2 -> 
                let indentation = 
                    Enumerable.Repeat (' ', nestLevel * 2) 
                    |> List.ofSeq 
                    |> List.fold (fun acc _ -> acc + " ") "" // gaidiskai veikia
                Left ("\n" + indentation + "true branch errors: " + err1 + "\n\n" + indentation + "false branch errors: " + err2 + "\n  ")
                
    let checkWhile (whileLoop: While) =
        match expressionType whileLoop.condition with
        | Left _ as err -> err
        | Right ``type`` when ``type`` <> Bool -> Left ("While condition should be a boolean type expression. This expression evaluates to: " + ``type``.ToString())
        | Right _ -> analyzeBlock whileLoop.body vars functions
                    
    let checkFunction (func: Function) =
        if functions.Select(fun x -> x.identifier).Contains(func.name) then
            Left("Trying to redeclare a function. Function with such name already exists. Name: " + func.name)
        else
            let uniqueArgsCount = func.args |> Seq.distinctBy (fun x -> x.identifier) |> Seq.length
            if (uniqueArgsCount <> func.args.Length) then 
                Left ("Function argument list contains variables with same names. Function: " + func.name)
            else
                let addTemporary = func.args |> List.map (fun arg -> { identifier = arg.identifier; vartype = arg.vartype })
                vars.AddRange(addTemporary)                         // gaidiskas workaroundas, bet jau atsibodo daryt
            
                let funcBodyAnalysisResult = analyzeBlock func.body vars functions

                match funcBodyAnalysisResult with
                | Left _ as err -> 
                    addTemporary |> List.iter (fun xx -> vars.Remove(xx) |> ignore)
                    err
                | Right _ ->
                    let returnType = func.toReturn |> expressionType    // funkciju priimami kintamieji negali kartoti vardu
                    addTemporary |> List.iter (fun xx -> vars.Remove(xx) |> ignore)
            
                    match returnType with
                    | Left _ as err -> err
                    | Right returnsExpressionWithType ->
                        if (func.``type`` <> returnsExpressionWithType) then
                            Left ("Function returns an expression, that doesn't match it's return type. Func type: " + func.``type``.ToString() 
                            + "; returns expression of type: " + returnsExpressionWithType.ToString())
                        else 
                            functions.Add({ identifier = func.name; ``type`` = func.``type``; args = func.args }) 
                            Right (func.``type``)

    let checkPrint (print: Print) = 
        match print with
        | Print.Message m -> Right Int
        | Print.Variable v -> 
            if (vars.Select(fun x -> x.identifier).Contains(v)) then Right (Int)
            else Left ("Trying to print undefined variable.")

    let checkPrintLine (print: PrintLine) = 
        match print with
        | PrintLine.Message m -> Right Int
        | PrintLine.Variable v -> 
            if (vars.Select(fun x -> x.identifier).Contains(v)) then Right (Int)
            else Left ("Trying to print undefined variable.")

    let checkSemanticValidity (statement: Statement) = 
        match statement with 
        | NewVarAssignment x -> checkNewVarAssignment x
        | ExistingVarAssignment x -> checkExistingVarAssignment x
        | If x -> checkIf x
        | While x -> checkWhile x
        | Print x -> checkPrint x
        | PrintLine x -> checkPrintLine x
        | Function x -> checkFunction x
        
    member this.ast = ast
    member this.vars = vars
    member this.nestLevel = nestLevel
    member this.run = ast |> List.map (fun statement -> checkSemanticValidity statement)

    new (ast) = Analysis(ast, new List<Var>(), new List<Fun>(), 1)

let analyze (ast: Statement list) =
    let res = Analysis(ast).run 

    res |> List.iteri (fun i res -> 
        match res with
        | Left err -> printfn "Statement %d: %A" (i + 1) err
        | Right _ ->  printfn "Statement %d: Semantically Correct" (i + 1)
    )
    if res |> List.exists (Either.isLeft) then None else Some ast
