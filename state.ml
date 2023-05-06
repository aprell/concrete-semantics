type state = name -> int

and name = string

let empty = fun _ -> 0

let bind n v s =
  fun x -> if x = n then v else s x

let assign ns_vs =
  List.fold_left
    (fun s (n, v) -> bind n v s)
    empty
    ns_vs
