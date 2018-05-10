// Signature file for parser generated by fsyacc
module Parser
type token = 
  | EOF
  | ASSIGNMENT
  | SEMICOLON
  | RIGHT_PARENS
  | LEFT_PARENS
  | CLOSE_BLOCK
  | OPEN_BLOCK
  | COMMA
  | MULTIPLY
  | DIVIDE
  | SUBTRACT
  | APPEND
  | ADD
  | MORE_THAN
  | LESS_THAN
  | LOGICAL_AND
  | LOGICAL_OR
  | EQUALS
  | NOT_EQUAL
  | PRINT
  | RETURN
  | FUN
  | WHILE
  | IF
  | ELSE
  | THEN
  | BOOLEAN of (bool)
  | NUMBER of (int)
  | STRING of (string)
  | IDENTIFIER of (string)
  | BOOL
  | STR
  | INT
type tokenId = 
    | TOKEN_EOF
    | TOKEN_ASSIGNMENT
    | TOKEN_SEMICOLON
    | TOKEN_RIGHT_PARENS
    | TOKEN_LEFT_PARENS
    | TOKEN_CLOSE_BLOCK
    | TOKEN_OPEN_BLOCK
    | TOKEN_COMMA
    | TOKEN_MULTIPLY
    | TOKEN_DIVIDE
    | TOKEN_SUBTRACT
    | TOKEN_APPEND
    | TOKEN_ADD
    | TOKEN_MORE_THAN
    | TOKEN_LESS_THAN
    | TOKEN_LOGICAL_AND
    | TOKEN_LOGICAL_OR
    | TOKEN_EQUALS
    | TOKEN_NOT_EQUAL
    | TOKEN_PRINT
    | TOKEN_RETURN
    | TOKEN_FUN
    | TOKEN_WHILE
    | TOKEN_IF
    | TOKEN_ELSE
    | TOKEN_THEN
    | TOKEN_BOOLEAN
    | TOKEN_NUMBER
    | TOKEN_STRING
    | TOKEN_IDENTIFIER
    | TOKEN_BOOL
    | TOKEN_STR
    | TOKEN_INT
    | TOKEN_end_of_input
    | TOKEN_error
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_prog
    | NONTERM_statements
    | NONTERM_statement
    | NONTERM_newVarAssignment
    | NONTERM_existingVarAssignment
    | NONTERM_ifCond
    | NONTERM_whileLoop
    | NONTERM_printLine
    | NONTERM_funArg
    | NONTERM_funArgs
    | NONTERM_func
    | NONTERM_returnS
    | NONTERM_vartype
    | NONTERM_expression
    | NONTERM_expressionInParens
    | NONTERM_value
    | NONTERM_operator
    | NONTERM_operation
    | NONTERM_functionArguments
    | NONTERM_functionCall
/// This function maps tokens to integer indexes
val tagOfToken: token -> int

/// This function maps integer indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val start : (Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> (Statement list) 
