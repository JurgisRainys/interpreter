namespace interpreter

type AST = 
    | NewAssignment of (Vartype * string * string)
    | Vart of Vartype

type Vartype =
    | INT
    | STR
    | BOOL   

(*
    <FsYacc Include="lexer-parser-rules/Parser.fsy">
      <OtherFlags>--module Parser</OtherFlags>
    </FsYacc>
    <FsLex Include="lexer-parser-rules/Lexer.fsl">
      <OtherFlags>--unicode</OtherFlags>
    </FsLex>
*)