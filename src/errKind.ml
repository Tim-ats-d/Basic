type t =
  | IllegalConstant
  | IllegalLineNumber
  | EndIsNotLast
  | NoEndInstruction
  | NoData
  | UndefinedNumber
  | ProgramTooLong
  | TooMuchData
  | TooMuchLabels
  | SyntaxError

exception Parse_error of t

let to_result = Result.error

let show = function
  | IllegalConstant -> "ILLEGAL CONSTANT"
  | IllegalLineNumber -> "ILLEGAL LINE NUMBER"
  | EndIsNotLast -> "END IS NOT LAST"
  | NoEndInstruction -> "NO END INSTRUCTION"
  | NoData -> "NO DATA"
  | UndefinedNumber -> "UNDEFINED NUMBER"
  | ProgramTooLong -> "PROGRAM TOO LONG"
  | TooMuchData -> "TOO MUCH DATA"
  | TooMuchLabels -> "TOO MUCH LABELS"
  | SyntaxError -> "SYNTAX ERROR"
