type t = stmt array

and stmt =
  | Let of { name : string; value : expr }
  | Read of string list
  | Data of int list
  | Print of expr list
  | GoTo of int
  | IfThen of { x : expr; op : comp_op; y : expr; line_nb : int }
  | For of { var : string; start : expr; stop : expr; step : expr option }
  | Next of string
  | Stop
  | End
[@@deriving show]

and expr =
  | Int of int
  | Float of float
  | String of string
  | Ident of string
  | BoolOp of { op : bool_op; values : expr list }
  | BinOp of { left : expr; op : op; right : expr }
  | UnaryOp of { op : unary_op; operand : expr }

and comp_op =
  | LessThan
  | LessThanOrEqual
  | Equal
  | GreaterThanOrEqual
  | GreaterThan
  | NotEqual

and op = Plus | Minus | Times | DivideBy | Power

and bool_op = And | Or

and unary_op = Invert | Not
