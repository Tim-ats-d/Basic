module Array = struct
  include Array

  let get_opt arr n =
    try Option.some @@ Array.get arr n with Invalid_argument _ -> None
end

module Buffer = struct
  include Buffer

  let pp fmt t = Format.pp_print_string fmt @@ Buffer.contents t
end

module Stack = struct
  type 'a t = Empty | Cons of 'a * 'a t [@@deriving show]

  let empty = Empty

  let cons x xs = Cons (x, xs)

  let pop t =
    match t with Empty -> (None, Empty) | Cons (x, xs) -> (Some x, xs)

  let push t x = cons x t

  let of_list l = List.fold_right (fun a b -> cons a b) l empty
end
