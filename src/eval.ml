module IntSet = Set.Make (Int)

let shadow_unused_line prgrm =
  let parr = Array.make (prgrm.Ast.max_line + 1) None in
  let _ =
    List.fold_left
      (fun lines (n, stmt) ->
        if IntSet.mem n lines then lines
        else (
          Array.set parr n @@ Some stmt;
          IntSet.add n lines))
      IntSet.empty prgrm.body
  in
  parr

let eval parsed_ast _limits =
  let body = shadow_unused_line parsed_ast in
  Ok Ast.{ body; data = parsed_ast.data }
