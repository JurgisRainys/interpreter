module AST 

type Identifier = string

type Text =
    | Variable of Identifier
    | Message of string

type PrintType =
    | PrintSingle of Text
    | PrintLine of Text

type PrintFile = {
    printType: PrintType
    path: string
    overwrite: bool
}

type Print = 
    | PrintFile of PrintFile
    | PrintConsole of PrintType

type Vartype =
    | Int
    | Str
    | Bool   

type Value = 
    | String of string
    | Number of int
    | Boolean of bool

type Operator = 
    | Add           | Subtract
    | Multiply      | Divide
    | MoreThan      | LessThan
    | LogicalOr     | LogicalAnd
    | Equals        | NotEqual
    | Append
    
and Expression = 
    | Value of Value
    | Expression of Operation
    | Identifier of Identifier
    | FunctionCall of FunctionCall

and FunctionCall = {
    identifier: string
    arguments: Expression list
}

and Operation = {
    op: Operator
    left: Expression
    right: Expression
}

and NewVarAssignment = {
    vartype: Vartype
    identifier: Identifier
    value: Expression
}

and ExistingVarAssignment = {
    identifier: Identifier
    value: Expression
}

and FunArg = {
    vartype: Vartype
    identifier: Identifier
}

and While = {
    condition: Expression
    body: Block
}

and If = { 
    condition: Expression
    trueBranch: Block
    falseBranch: Block
}

and Function = {
    ``type``: Vartype
    name: Identifier
    args: FunArg list
    body: Block
    toReturn: Expression
}

and Block = Statement list

and Statement =
| NewVarAssignment of NewVarAssignment
| ExistingVarAssignment of ExistingVarAssignment
| If of If
| While of While
| Print of Print
| Function of Function