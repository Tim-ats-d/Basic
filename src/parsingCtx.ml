type t = {
  mutable end_encounter : bool;
  mutable data_encounter : bool;
  mutable read_encounter : bool;
  mutable max_line : int;
  mutable constant_label : int;
  mutable line_nb : int;
  mutable printed_length : int;
  mutable data : Ast.number list;
  mutable ndata : int;
}

let create () =
  {
    end_encounter = false;
    data_encounter = false;
    read_encounter = false;
    max_line = 0;
    constant_label = 0;
    line_nb = 0;
    printed_length = 0;
    data = [];
    ndata = 0;
  }

let incr_line_nb t =
  t.line_nb <- t.line_nb + 1;
  if t.line_nb > Limits.standard.max_lines_nb then
    raise ErrKind.(Parse_error ProgramTooLong)

let incr_constant_label t =
  t.constant_label <- t.constant_label + 1;
  if t.constant_label > Limits.standard.constants_and_labels then
    raise ErrKind.(Parse_error ProgramTooLong)

let set_end_encounter t =
  if t.end_encounter then raise ErrKind.(Parse_error EndIsNotLast)
  else t.end_encounter <- true

let set_data_encounter t = t.data_encounter <- true

let set_read_encounter t = t.read_encounter <- true

let update_data t d =
  t.ndata <- t.ndata + List.length d;
  if t.ndata > Limits.standard.data then raise ErrKind.(Parse_error TooMuchData)
  else t.data <- d @ t.data

let update_max_line t n =
  if n < 0 || n > 99999 then raise ErrKind.(Parse_error IllegalLineNumber)
  else if n > t.max_line then t.max_line <- n

let update_printed_length t n =
  t.printed_length <- t.printed_length + n;
  if t.printed_length > Limits.standard.printed_labels_length then
    raise ErrKind.(Parse_error TooMuchLabels)

let raise_no_end t =
  if not t.end_encounter then raise ErrKind.(Parse_error NoEndInstruction)

let raise_end_not_last t =
  if t.end_encounter then raise ErrKind.(Parse_error EndIsNotLast)

let raise_illegal_consti n =
  if n > 999999999 then raise ErrKind.(Parse_error IllegalConstant)

let raise_illegal_constf n =
  if String.length @@ string_of_float n > 9 then raise ErrKind.(Parse_error IllegalConstant)

let raise_no_data t =
  if t.read_encounter && not t.data_encounter then
    raise ErrKind.(Parse_error NoData)
