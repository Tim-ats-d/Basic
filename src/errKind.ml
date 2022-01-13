type t =
  | IllegalLineNumber
  | EndIsNotLast
  | NoEndInstruction
  | UndefinedNumber
  | SyntaxError

exception Parse_error of t

let to_result = Result.error

let show = function
  | IllegalLineNumber -> "ILLEGAL LINE NUMBER"
  | EndIsNotLast -> "END IS NOT LAST"
  | NoEndInstruction -> "NO END INSTRUCTION"
  | UndefinedNumber -> "UNDEFINED NUMBER"
  | SyntaxError -> "SYNTAX ERROR"
