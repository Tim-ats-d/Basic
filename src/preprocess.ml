module IntSet = Set.Make (Int)

type ctx = {
  end_encounter : bool;
  data : Ast.number list;
  lines : IntSet.t;
  limit : Limitations.t;
}

let initial =
  Ok
    {
      end_encounter = false;
      data = [];
      lines = IntSet.empty;
      limit = Limitations.null;
    }

let count_label elist =
  List.fold_left
    (fun (x, y) expr ->
      let x' =
        x
        +
        match expr with
        | Ast.Label buf -> Buffer.length buf
        | LabelConcat { label; _ } -> Buffer.length label
        | _ -> 0
      in
      let y' =
        y + match expr with Ast.Label _ | LabelConcat _ -> 1 | _ -> 0
      in
      (x', y'))
    (0, 0) elist

let pp { Ast.body; max_line } limits =
  if max_line > limits.Limitations.lines_nb then
    ErrKind.(to_result ProgramTooLong)
  else
    let prgrm = Array.make (max_line + 1) None in
    let pp_stmt (n, stmt) ctx =
      if IntSet.mem n ctx.lines then Ok ctx
      else
        let lines = IntSet.add n ctx.lines in
        if ctx.end_encounter then ErrKind.(to_result EndIsNotLast)
        else if ctx.limit.constants_and_labels > limits.constants_and_labels
        then ErrKind.(to_result ProgramTooLong)
        else if ctx.limit.printed_labels_length > limits.printed_labels_length
        then ErrKind.(to_result TooMuchLabels)
        else
          let () = Array.set prgrm n @@ Some stmt in
          match stmt with
          | Ast.End -> Ok { ctx with end_encounter = true; lines }
          | Data dlist ->
              let data = dlist @ ctx.data in
              Ok { ctx with data; lines }
          | Print pitems ->
              let labels_length, const_label = count_label pitems in
              let limit =
                Limitations.(
                  update_pll (update_cl ctx.limit const_label) labels_length)
              in
              Ok { ctx with limit; lines }
          | _ -> Ok { ctx with lines }
    in
    let res =
      List.fold_left
        (fun ctx line -> Result.bind ctx @@ pp_stmt line)
        initial body
    in
    match res with
    | Ok { end_encounter; data; _ } ->
        let data = Array.of_list data in
        if Array.length data > limits.data then ErrKind.(to_result TooMuchData)
        else if end_encounter then Ok { Ast.body = prgrm; data }
        else ErrKind.(to_result NoEndInstruction)
    | Error _ as err -> err
