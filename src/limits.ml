type t = {
  max_lines_nb : int;
  constants_and_labels : int;
  data : int;
  printed_labels_length : int;
  for_stmt : int;
  goto_and_if_then : int;
  list_and_tables : int;
}

let standard =
  {
    max_lines_nb = 99999;
    constants_and_labels = 175;
    data = 300;
    printed_labels_length = 600;
    for_stmt = 26;
    goto_and_if_then = 80;
    list_and_tables = 1500;
  }
