type t = {
  lines_nb : int;
  constants_and_labels : int;
  data : int;
  printed_labels_length : int;
  for_stmt : int;
  goto_and_if_then : int;
  list_and_tables : int;
}

let null =
  {
    lines_nb = 0;
    constants_and_labels = 0;
    data = 0;
    printed_labels_length = 0;
    for_stmt = 0;
    goto_and_if_then = 0;
    list_and_tables = 0;
  }

let incr_line t = { t with lines_nb = t.lines_nb + 1 }

let update_cl t n =
  { t with constants_and_labels = t.constants_and_labels + n }

let incr_data t = { t with data = t.data + 1 }

let update_pll t n =
  { t with printed_labels_length = t.printed_labels_length + n }

let incr_for t = { t with for_stmt = t.for_stmt + 1 }

let incr_goto_if_then t = { t with goto_and_if_then = t.goto_and_if_then + 1 }

let incr_list_tables t = { t with list_and_tables = t.list_and_tables + 1 }
