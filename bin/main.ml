(*
   10 LET MAX = 5000
   20 LET X = 1
   30 LET Y = 1
   40 IF X > MAX THEN GOTO 100
   50 PRINT X
   60 X = X + Y
   70 IF (Y > MAX) GOTO 100
   80 PRINT Y
   90 Y = X + Y
   100 GOTO 30
   110 END
*)

open Basic

let prgrm =
  Ast.[ (10, Print [ String "ok" ]); (20, Print [ String "error" ]); (30, End) ]
(* Ast.(List.init 99998 (fun i -> (i + 1, Print [])) @ [ (99999, End) ]) *)

let () =
  match Preprocess.preprocess prgrm with
  | Ok arr -> Array.iter (fun s -> print_endline @@ Ast.show_stmt s) arr
  | Error err -> print_endline @@ ErrKind.to_string err
