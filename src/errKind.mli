type t =
  | IllegalLineNumber
  | EndIsNotLast
  | NoEndInstruction
  | UndefinedNumber
  | ProgramTooLong
  | TooMuchData
  | TooMuchLabels
  | SyntaxError

exception Parse_error of t

val to_result : t -> ('a, t) result

val show : t -> string
