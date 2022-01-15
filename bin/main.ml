open Basic

let prgrm =
  {basic|10 LET M = 0
20 LET X = 1
30 LET Y = 1
40 IF X > M THEN 100
50 PRINT X
60 LET X = X + Y
80 PRINT Y
90 END
|basic}

let () =
  match Compiler.compile prgrm with
  | Ok prgrm ->
      (* Array.iter
         (function Some s -> print_endline @@ Ast.show_stmt s | None -> ())
         prgrm.body; *)
      Array.iter (fun n -> print_endline @@ Ast.show_number n) prgrm.data
  | Error err -> print_endline @@ ErrKind.show err
