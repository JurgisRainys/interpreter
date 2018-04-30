namespace interpreter

module AST = 
    type Identifier = string

    type Print = 
        | Variable of Identifier
        | Message of string

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
    
    type Expression = 
        | Value of Value
        | Expression of Operation
        | Identifier of Identifier
        
    and Operation = {
        op: Operator
        left: Expression
        right: Expression
    }

    type NewVarAssignment = {
        vartype: Vartype
        identifier: Identifier
        value: Expression
    }

    type ExistingVarAssignment = {
        identifier: Identifier
        value: Expression
    }

    type FunArg = {
        vartype: Vartype
        identifier: Identifier
    }

    type While = {
        condition: Expression
        body: Block
    }

    and If = { 
        condition: Expression
        trueBranch: Block
        falseBranch: Block
    }


    and Function = {
        name: Identifier
        args: FunArg list
        body: Block
    }

    and Block = Statement list

    and Statement =
    | NewVarAssignment of NewVarAssignment
    | ExistingVarAssignment of ExistingVarAssignment
    | Block of Block
    | If of If
    | While of While
    | Print of Print
    | Function of Function