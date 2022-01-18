module R = Result
module U = Utils

module Env : sig
  type t

  val empty : t

  val set : t -> string -> Value.t -> t

  val find : t -> string -> Value.t option
end = struct
  module NameMap = Map.Make (String)

  type t = Value.t NameMap.t

  let empty = NameMap.empty

  let set t name value = NameMap.add name value t

  let find t name = NameMap.find_opt name t
end

let preprocess prgrm =
  let parr = Array.make (prgrm.Ast.max_line + 1) None in
  let _ =
    let module IntSet = Set.Make (Int) in
    List.fold_left
      (fun lines (n, stmt) ->
        if IntSet.mem n lines then lines
        else
          let () = Array.set parr n @@ Some stmt in
          IntSet.add n lines)
      IntSet.empty
    @@ List.sort (fun (x, _) (y, _) -> Int.compare x y) prgrm.body
  in
  parr

type ctx = { data : Ast.number U.Stack.t; var_env : Env.t }

type state = Finish | Continue of ctx | Jump of int * ctx

let rec eval past _limits =
  let prgrm = preprocess past in
  let init_ctx = { data = past.data; var_env = Env.empty } in
  eval_prgrm prgrm init_ctx 0

and eval_prgrm prgrm ctx i =
  let stmt = U.Array.get_opt prgrm i in
  Option.fold
    ~none:ErrKind.(to_result UndefinedNumber)
    ~some:(function
      | None -> eval_prgrm prgrm ctx @@ succ i
      | Some stmt -> Result.bind (eval_stmt ctx stmt) @@ advance prgrm i)
    stmt

and advance prgrm i = function
  | Finish -> Ok ()
  | Continue ctx -> eval_prgrm prgrm ctx @@ succ i
  | Jump (i, ctx) -> eval_prgrm prgrm ctx i

and eval_stmt ctx =
  let open Ast in
  function
  | Let { name; expr } ->
      R.bind (eval_expr ctx expr) (fun v ->
          let var_env = Env.set ctx.var_env name v in
          R.ok @@ Continue { ctx with var_env })
  | Read elist -> eval_read ctx elist
  | Data -> R.ok @@ Continue ctx
  | Print { items; _ } -> eval_print ctx items
  | GoTo i -> R.ok @@ Jump (i, ctx)
  | End -> Ok Finish
  | _ -> failwith "todo"

and eval_expr ctx = function
  | Int i -> R.ok @@ Value.VInt i
  | Float f -> R.ok @@ Value.VFloat f
  | Label b -> R.ok @@ Value.VLabel (Buffer.contents b)
  | Name n ->
      let vopt = Env.find ctx.var_env n in
      Option.fold ~none:ErrKind.(to_result IllegalVariable) ~some:R.ok vopt
  | _ -> failwith "todo"

and eval_number =
  let open Ast in
  function
  | Pos, `Int i -> Value.int @@ +i
  | Pos, `Float f -> Value.float @@ +.f
  | Neg, `Int i -> Value.int @@ -i
  | Neg, `Float f -> Value.float @@ Float.neg f

and eval_read ctx elist =
  let bind_var ctx_opt expr =
    Option.bind ctx_opt (fun ctx ->
        let dopt, rest = U.Stack.pop ctx.data in
        match dopt with
        | None -> None
        | Some nb ->
            let name =
              match expr with Ast.Name name -> name | _ -> assert false
            in
            let var_env = Env.set ctx.var_env name @@ eval_number nb in
            Some { data = rest; var_env })
  in
  let ctx' = List.fold_left bind_var (Some ctx) elist in
  Option.fold ctx' ~none:(Ok Finish) ~some:(fun c -> R.ok @@ Continue c)

and eval_print ctx items =
  List.fold_left
    (fun r item ->
      R.bind r (fun state ->
          R.bind (eval_expr ctx item) (fun v ->
              print_endline @@ Value.to_string v;
              Ok state)))
    (R.ok @@ Continue ctx) items
