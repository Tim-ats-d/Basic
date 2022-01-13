let from_lexbuf lexbuf =
  try Result.ok @@ Parser.prgrm Lexer.read lexbuf with
  | Lexer.Syntax_error -> ErrKind.(to_result SyntaxError)
  | ErrKind.Parse_error err -> ErrKind.to_result err
  | Parser.Error -> ErrKind.(to_result SyntaxError)

let from_string str =
  let lexbuf = Lexing.from_string str in
  from_lexbuf lexbuf

let compile prgrm =
  match from_string prgrm with
  | Ok ast -> Eval.eval ast
  | Error err -> ErrKind.to_result err
