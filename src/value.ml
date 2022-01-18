type t = VInt of int | VFloat of float | VLabel of string
[@@deriving eq, ord, show]

let to_string = function
  | VInt i -> Int.to_string i
  | VFloat f -> Float.to_string f
  | VLabel l -> l

let int i = VInt i

let float f = VFloat f

let label l = VLabel l
