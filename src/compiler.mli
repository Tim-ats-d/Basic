val parse : Lexing.lexbuf -> (Ast.parsed_ast, ErrKind.t) result

val from_lexbuf : Lexing.lexbuf -> Ast.parsed_ast

val from_string : string -> Ast.parsed_ast

val compile : string -> (Ast.stmt option array, ErrKind.t) result
