type t = stmt array

and stmt =
  | Let of { name : string; expr : expr }
  | Read of string list
  | Data of number list
  | Print of expr list
  | GoTo of int
  | IfThen of { left : expr; op : comp_op; right : expr; line : int }
  | For of { var : string; start : expr; stop : expr; step : expr option }
  | Next of string
  | Stop
  | End
[@@deriving show]

and expr =
  | Number of number
  | Label of string
  | Ident of string
  | LabelConcat of { label : string; expr : expr }
  | BoolOp of { op : bool_op; values : expr list }
  | BinOp of { left : expr; op : op; right : expr }
  | UnaryOp of { op : unary_op; operand : expr }

and number = NInt of int | NFloat of float

and comp_op =
  | Equal
  | NotEqual
  | LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual

and op = Plus | Minus | Times | Div | Power

and bool_op = And | Or

and unary_op = Invert | Not

type parsed_ast = (stmt option array * number array, ErrKind.t) result
