let from_lexbuf lexbuf =
  try Result.ok @@ Parser.prgrm Lexer.read lexbuf with
  | Lexer.Syntax_error -> ErrKind.(to_result SyntaxError)
  | ErrKind.Parse_error err -> ErrKind.to_result err
  | Parser.Error -> ErrKind.(to_result SyntaxError)

let from_string str =
  let lexbuf = Lexing.from_string str in
  from_lexbuf lexbuf

let compile prgrm =
  let limits =
    Limitations.{
      lines_nb = 99999;
      constants_and_labels = 175;
      data = 300;
      printed_labels_length = 600;
      for_stmt = 26;
      goto_and_if_then = 80;
      list_and_tables = 1500;
    }
  in
  match from_string prgrm with
  | Ok ast -> Eval.eval ast limits
  | Error err -> ErrKind.to_result err
