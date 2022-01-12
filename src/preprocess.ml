module LineMap = Map.Make (Int)
module IntSet = Set.Make (Int)

type ctx = { end_encounter : bool; lines : IntSet.t }

let initial = Ok { end_encounter = false; lines = IntSet.empty }

let pp parsed_ast =
  let prgrm = Array.make 100000 None in
  let pp_stmt (n, stmt) ctx =
    if n <= 0 || n > 99999 then ErrKind.(to_result IllegalLineNumber)
    else if IntSet.mem n ctx.lines then Ok ctx
    else
      let lines = IntSet.add n ctx.lines in
      if ctx.end_encounter then ErrKind.(to_result EndIsNotLast)
      else
        let () = Array.set prgrm n @@ Some stmt in
        match stmt with
        | Ast.End -> Ok { end_encounter = true; lines }
        | _ -> Ok { ctx with lines }
  in
  let res =
    Array.fold_left
      (fun ctx line -> Result.bind ctx @@ pp_stmt line)
      initial parsed_ast
  in
  match res with
  | Ok { end_encounter; _ } ->
      if end_encounter then Ok prgrm else ErrKind.(to_result NoEndInstruction)
  | Error _ as err -> err
