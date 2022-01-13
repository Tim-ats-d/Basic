{
  open Parser

  exception Syntax_error
}

let ascii_char = [' '-'~']
let digit = ['0'-'9']
let space = ' ' | '\t'

let var = ['A'-'Z'] digit?
let int = '-'? digit*
let float = '-'? (digit*)? '.' digit+

let rem = "REM" [^ '\n']* ('\n')

rule read = parse
  | eof         { EOF }
  | space       { read lexbuf }
  | '\n'        { Lexing.new_line lexbuf; NEWLINE }
  | rem         { Lexing.new_line lexbuf; read lexbuf }
  | "LET"       { LET }
  | "READ"      { READ }
  | "DATA"      { DATA }
  | "PRINT"     { PRINT }
  | "GO"        { GO }
  | "TO"        { TO }
  | "IF"        { IF }
  | "THEN"      { THEN }
  | "FOR"       { FOR }
  | "STEP"      { STEP }
  | "NEXT"      { NEXT }
  | "END"       { END }
  | "STOP"      { STOP }
  | "DEF"       { DEF }
  | "FN"        { FN }
  | "GOSUB"     { GOSUB }
  | "RETURN"    { RETURN }
  | "DIM"       { DIM }
  | '='         { EQ }
  | "<>"        { NEQ }
  | '<'         { LT }
  | "<="        { LTE }
  | '>'         { GT }
  | ">="        { GTE }
  | '+'         { PLUS }
  | '-'         { MINUS }
  | '/'         { DIV }
  | '^'         { POWER }
  | '('         { LPARENT }
  | ')'         { RPARENT }
  | ','         { COMMA }
  | digit* as d { INT (int_of_string d) }
  | float as f  { FLOAT (float_of_string f)}
  | var as v    { VAR v }
  | '"'         { read_string (Buffer.create 17) lexbuf }
  | _           { raise Syntax_error }

and read_string buf = parse
  | '"'             { LABEL (Buffer.contents buf) }
  | _               { raise Syntax_error }
  | ascii_char as c { Buffer.add_char buf c; read_string buf lexbuf }
