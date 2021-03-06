val from_lexbuf : Lexing.lexbuf -> (Ast.parsed_ast, ErrKind.t) result

val from_string : string -> (Ast.parsed_ast, ErrKind.t) result

val compile : string -> (unit, ErrKind.t) result
