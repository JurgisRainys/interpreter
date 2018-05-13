module Semantics

open AST
open System.Linq
open Either
open System.Collections.Generic

type Func = {
    ``type``: Vartype
    args: FunArg list
}

type TypeWithLevel = {
    vartype: Vartype
    nestLevel: int
}

type SemanticError = string

// realiai vartype option turetu grazint, bet tingiu apsikraut, nes belenkiek papildomo matchinimo reikes.
type AnalysisResult = Either<SemanticError, Vartype> 

let dictionaryToMap (dictionary: Dictionary<'key, 'value>) =
    dictionary |> Seq.map ``|KeyValue|`` |> Seq.toList |> Map.ofList

let mapToDictionary (map: Map<'key, 'value>) =
    let acc = new Dictionary<'key, 'value>()
    map |> Map.iter (fun key value -> acc.Add(key, value))
    acc

type Analysis (ast: Statement list, 
                vars: Dictionary<Identifier, TypeWithLevel>, 
                funcs: Dictionary<Identifier, Func>,
                nestLevel: int) =

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
            else 
                let indentation = 
                    Enumerable.Repeat (' ', (nestLevel + 1) * 2) 
                    |> Seq.fold (fun acc x -> x.ToString() + acc) ""
                Left (errorList |> List.fold (fun acc error -> acc + "\n" + indentation + error) "")

    let getFunc name =
        if funcs.ContainsKey(name) then Some funcs.[name] else None

    let getVar identifier =
        if vars.ContainsKey(identifier) then Some vars.[identifier] else None

    let combineCurrentWithParent (parentScopeDictionary: Dictionary<'a, 'b>) (currentScopeDictionary: Dictionary<'a, 'b>): Dictionary<'a, 'b> =
        let parentScopeMap = parentScopeDictionary |> dictionaryToMap
        let currentScopeMap = currentScopeDictionary |> dictionaryToMap
        currentScopeMap 
        |> Map.fold (fun acc name value -> acc |> Map.add name value) parentScopeMap
        |> mapToDictionary

    let funcsContain name = 
        match getFunc name with
        | None -> false
        | Some _ -> true

    let varsContain identifier = 
        match getVar identifier with
        | None -> false
        | Some _ -> true

    let rec funcCallType (expr: FunctionCall) : AnalysisResult =
        match getFunc expr.identifier with
        | None -> Left("Usage of undefined function. Function name: " + expr.identifier.ToString())
        | Some f ->
            let { Func.args = args; ``type`` = funType } = f
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

    and identifierType i =
        let valOpt = getVar i |> Option.map (fun typeWithLvl -> typeWithLvl.vartype)
        match valOpt with
        | Some value -> Right value
        | None -> Left ("No such identifier declared previously. Identifier: " + i + "\n")

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
            match getVar newA.identifier with
            | Some vartypeWithLevel when vartypeWithLevel.nestLevel = nestLevel-> 
                Left ("Identifier already declared. Identifier: " + newA.identifier)
            | _ -> 
                vars.[newA.identifier] <- { vartype = newA.vartype; nestLevel = nestLevel }
                Right (``type``)
        | Right ``type`` -> 
            Left ("Expression evaluates to different type than var declaration states. Vartype: " 
            + newA.vartype.ToString() + "; Identifier: " + newA.identifier + "; Expression type: " + ``type``.ToString())

    let checkExistingVarAssignment (exsA: ExistingVarAssignment) =
        match expressionType(exsA.value) with
        | Left _ as err -> err
        | Right ``type`` -> 
            //let x = vars |> Map.tryPick (fun ident ``type`` -> { vartype = ``type``; identifier = ident })
            match getVar exsA.identifier with
            | None -> Left ("Identifier wan not declared previously. Identifier: " + exsA.identifier)
            | Some typeWithNestLevel -> 
                if typeWithNestLevel.vartype = ``type`` then Right ``type``
                else Left ("Identifier stores different type of value. Identifier: " + exsA.identifier)
            //if varsContain exsA.identifier then
            //    if vars.[exsA.identifier] = ``type`` then Right ``type``
            //    else Left ("Identifier stores different type of value. Identifier: " + exsA.identifier)
            //else Left ("Identifier wan not declared previously. Identifier: " + exsA.identifier)
                
    let checkPrint (print: Print) = 
        match print with
        | Print.Message m -> Right Int
        | Print.Variable v -> 
            if varsContain v then Right (Int)
            else Left ("Trying to print undefined variable.")

    let checkPrintLine (print: PrintLine) = 
        match print with
        | PrintLine.Message m -> Right Int
        | PrintLine.Variable v -> 
            if varsContain v then Right (Int)
            else Left ("Trying to print undefined variable.")

    let elementsFromListOfTypeToDictionary (ast: 'a list) (fkey: 'atype -> 'key) (fval: 'atype -> 'value) : Dictionary<'key, 'value> =
        let acc = new Dictionary<'key, 'value>() 
        ast |> List.iter (fun x ->
            match box x with
            | :? 'atype as x -> 
                acc.[fkey x] <- fval x
            | _ -> ()
        )
        acc

    //elementsFromListOfTypeToDictionary ast (fun x -> x.name) (fun x -> { ``type`` = x.``type``; args = x.args })
    //elementsFromListOfTypeToDictionary ast (fun x -> x.identifier) (fun x -> x.vartype)
    let childScopeFuncs (ast: Statement list) = 
        let acc = new Dictionary<Identifier, Func>() 
        ast |> List.iter (fun x ->
            match x with
            | Function f -> acc.[f.name] <- { ``type`` = f.``type``; args = f.args }
            | _ -> ()
        )
        acc

    let childScopeVars (ast: Statement list) = 
        let acc = new Dictionary<Identifier, Vartype>() 
        ast |> List.iter (fun x ->
            match x with
            | NewVarAssignment a -> acc.[a.identifier] <- a.vartype
            | _ -> ()
        )
        acc

    let analyzeFuncOrBlock (ast: Statement list) varsInBlock funcsInBlock =
        let results =
            Analysis(ast, varsInBlock, funcsInBlock, nestLevel + 1).run
            |> List.mapi (fun i res -> i + 1, res ) 
            |> List.filter (fun (i, res) -> res |> Either.isLeft)

        match results with
        | [] -> Right(Int) // Turetu grazinti `None` optiona
        | errors -> 
            let xx = Enumerable.Repeat (' ', (nestLevel + 1) * 2) |> List.ofSeq
            let indentation = xx  |> List.fold (fun acc _ -> acc + " ") "" 
            errors |> List.fold (fun acc (i, err) ->
                match err with 
                | Left msg -> acc + "\n" + indentation + "Statement " + i.ToString() + ": " + msg
            ) "" |> Left

    let statementsUntilFuncDefinition funcName ast =
        //let mutable funcNotFound = true;
        ast 
        |> Seq.takeWhile (fun stat -> 
            match stat with
            | Function f when f.name = funcName -> false
            | _ -> true
        )
        |> Seq.toList

    let analyzeFunc (func: Function) =
        let funcArgs = 
            func.args 
            |> List.fold (fun acc arg -> 
                acc |> Map.add arg.identifier { vartype = arg.vartype; nestLevel = nestLevel }) Map.empty
            |> mapToDictionary
        funcs.[func.name] <- { ``type`` = func.``type``; args = func.args }


        let newHigherScopeVars = vars |> combineCurrentWithParent <| funcArgs
        let newHigherScopeFuncs = funcs
        let bodyWithReturnStatement = 
            List.append func.body [ NewVarAssignment { identifier = "RETURN"; vartype = func.``type``; value = func.toReturn } ]

        let r = analyzeFuncOrBlock bodyWithReturnStatement newHigherScopeVars newHigherScopeFuncs
        funcs.Remove func.name |> ignore // nes analyze func bloke idedam sita
        r

    // nestlevelis padaro, kad graziai erroras formuojamas butu, kai nestinami blokai
    let analyzeBlock (ast: Statement list) : AnalysisResult =
        analyzeFuncOrBlock ast vars funcs

    let checkFunction (func: Function) =
        if funcsContain func.name then
            Left("Trying to redeclare a function. Function with such name already exists. Name: " + func.name)
        else
            let uniqueArgsCount = func.args |> Seq.distinctBy (fun x -> x.identifier) |> Seq.length
            if (uniqueArgsCount <> func.args.Length) then 
                Left ("Function argument list contains variables with same names. Function: " + func.name)
            else
                let funcBodyAnalysisResult = analyzeFunc func

                match funcBodyAnalysisResult with
                | Left _ as err -> err
                | Right _ -> 
                    funcs.[func.name] <- { ``type`` = func.``type``; args = func.args }
                    Right (func.``type``)

    
    let checkIf (ifCond: If) = 
        match expressionType ifCond.condition with
        | Left _ as err -> err
        | Right ``type`` when ``type`` <> Bool -> Left ("If condition should be a boolean type expression. This expression evaluates to: " + ``type``.ToString())
        | Right _ -> 
            let trueBrErrors = analyzeBlock ifCond.trueBranch
            let falseBrErrors = analyzeBlock ifCond.falseBranch

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
        | Right _ -> analyzeBlock whileLoop.body

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

    new (ast) = Analysis(ast, 
                        new Dictionary<Identifier, TypeWithLevel>(), 
                        new Dictionary<Identifier, Func>(), 1)

let analyze (ast: Statement list) =
    let res = Analysis(ast).run 

    res |> List.iteri (fun i res -> 
        match res with
        | Left err -> printfn "Statement %d: %A" (i + 1) err
        | Right _ ->  printfn "Statement %d: Semantically Correct" (i + 1)
    )
    printfn ""
    if res |> List.exists (Either.isLeft) then None else Some ast
