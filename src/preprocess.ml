module IntSet = Set.Make (Int)

type ctx = { end_encounter : bool; data : Ast.number list; lines : IntSet.t }

let initial = Ok { end_encounter = false; data = []; lines = IntSet.empty }

let pp parsed_ast line_nb =
  let prgrm = Array.make line_nb None in
  let pp_stmt (n, stmt) ctx =
    if IntSet.mem n ctx.lines then Ok ctx
    else
      let lines = IntSet.add n ctx.lines in
      if ctx.end_encounter then ErrKind.(to_result EndIsNotLast)
      else
        let () = Array.set prgrm n @@ Some stmt in
        match stmt with
        | Ast.End -> Ok { ctx with end_encounter = true; lines }
        | Ast.Data ilist ->
            let data = ilist @ ctx.data in
            Ok { ctx with data; lines }
        | _ -> Ok { ctx with lines }
  in
  let res =
    List.fold_left
      (fun ctx line -> Result.bind ctx @@ pp_stmt line)
      initial parsed_ast
  in
  match res with
  | Ok { end_encounter; data; _ } ->
      if end_encounter then Ok (prgrm, Array.of_list data)
      else ErrKind.(to_result NoEndInstruction)
  | Error _ as err -> err
