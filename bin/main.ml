open Basic

let prgrm =
  {basic|10 DATA 5
20 LET X = 1
30 LET Y = 1
40 IF X > M THEN 100
50 PRINT X
60 LET X = X + Y
80 PRINT Y
110 END
|basic}

let () =
  match Compiler.compile prgrm with
  | Ok arr ->
      Array.iter
        (function Some s -> print_endline @@ Ast.show_stmt s | None -> ())
        arr.body
  | Error err -> print_endline @@ ErrKind.show err
