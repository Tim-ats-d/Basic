open Basic

(* let prgrm =
     {basic|10 LET M = 0
   20 LET X = 1
   30 LET Y = 1
   40 IF X > M THEN 90
   50 PRINT X
   60 LET X = X + Y
   80 PRINT Y
   90 END
   |basic} *)

let prgrm = {basic|10 GOTO 3
20 PRINT "OUPS"
30 PRINT "OK"
90 END
|basic}

let () =
  match Compiler.compile prgrm with
  | Ok () -> print_endline "Sucess"
  | Error err -> print_endline @@ ErrKind.show err
