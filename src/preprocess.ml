module LineMap = Map.Make (Int)

type ctx = {
  acc : Ast.stmt list;
  cline : int;
  end_encounter : bool;
  lines : int LineMap.t;
}

let initial =
  Ok { acc = []; cline = 0; end_encounter = false; lines = LineMap.empty }

let preprocess parsed_ast =
  let pp_stmt (n, stmt) ctx =
    if n <= 0 || n > 99999 then ErrKind.(to_result IllegalLineNumber)
    else if LineMap.mem n ctx.lines then Ok ctx
    else
      let lines = LineMap.add n ctx.cline ctx.lines in
      let cline = ctx.cline + 1 in
      let acc = stmt :: ctx.acc in
      if ctx.end_encounter then ErrKind.(to_result EndIsNotLast)
      else
        match stmt with
        | Ast.End -> Ok { ctx with end_encounter = true; acc; lines }
        | GoTo g ->
            let acc =
              match LineMap.find_opt g ctx.lines with
              | Some nb -> Ast.GoTo nb :: acc
              | None -> acc
            in
            Ok { ctx with acc; cline; lines }
        | _ -> Ok { ctx with cline; acc; lines }
  in
  let sorted_ast =
    List.sort (fun (x, _) (y, _) -> Int.compare x y) parsed_ast
  in
  let res =
    List.fold_left
      (fun ctx elem -> Result.bind ctx @@ pp_stmt elem)
      initial sorted_ast
  in
  match res with
  | Ok { acc; end_encounter; _ } ->
      if end_encounter then Result.ok @@ Array.of_list acc
      else ErrKind.(to_result NoEndInstruction)
  | Error _ as err -> err
