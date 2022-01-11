type t = IllegalLineNumber | EndIsNotLast | NoEndInstruction | UndefinedNumber

let to_result = Result.error

let to_string = function
  | IllegalLineNumber -> "ILLEGAL LINE NUMBER"
  | EndIsNotLast -> "END IS NOT LAST"
  | NoEndInstruction -> "NO END INSTRUCTION"
  | UndefinedNumber -> "UNDEFINED NUMBER"
