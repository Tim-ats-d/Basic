module U = Utils

type t = { body : stmt option array; data : number U.Stack.t }

and stmt =
  | Let of { name : string; expr : expr }
  | Read of expr list
  | Data
  | Print of { items : expr list; end_newline : bool }
  | GoTo of int
  | IfThen of { left : expr; op : comp_op; right : expr; line : int }
  | For of { var : string; start : expr; stop : expr; step : expr option }
  | Next of string
  | Def of { name : string; param : string; body : expr }
  | GoSub of int
  | Return
  | Dim of { name : string; ilist : int list }
  | Stop
  | End
[@@deriving show]

and expr =
  | Int of int
  | Float of float
  | Label of U.Buffer.t
  | Name of string
  | LabelConcat of { label : U.Buffer.t; expr : expr }
  | SubScript of { name : string; args : expr list }
  | BoolOp of { op : bool_op; values : expr list }
  | BinOp of { left : expr; op : op; right : expr }
  | UnaryOp of { op : unary_op; operand : expr }
  | FunCall of { name : string; arg : expr }

and number = sign * [ `Int of int | `Float of float ]

and sign = Pos | Neg

and comp_op =
  | Equal
  | NotEqual
  | LessThan
  | LessThanOrEqual
  | GreaterThan
  | GreaterThanOrEqual

and op = Plus | Minus | Times | Div | Power

and bool_op = And | Or

and unary_op = UPlus | Invert | Not

type parsed_ast = {
  body : (int * stmt) list;
  data : number U.Stack.t;
  max_line : int;
}
[@@deriving show]
