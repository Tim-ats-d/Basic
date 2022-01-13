type t =
  | IllegalLineNumber
  | EndIsNotLast
  | NoEndInstruction
  | UndefinedNumber
  | SyntaxError

exception Parse_error of t

val to_result : t -> ('a, t) result

val show : t -> string
