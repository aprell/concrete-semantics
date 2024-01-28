let assert_property ?name p xs =
  try assert (List.for_all p xs) with
  Assert_failure _ ->
    Option.iter (Printf.eprintf ">>> \027[4m%s\027[0m failed\n") name;
    failwith "assert_property"

let assert_equal ?name lhs rhs =
  try assert (lhs = rhs) with
  Assert_failure _ ->
    Option.iter (Printf.eprintf ">>> \027[4m%s\027[0m failed\n") name;
    failwith "assert_equal"

let equivalent f g xs =
  List.for_all (fun x -> f x = g x) xs

let rec until b f x =
  if not (b x) then until b f (f x) else x

let rec while' b f x =
  if b x then while' b f (f x) else x

let printf ?(indent = 0) =
  print_string (String.make indent ' ');
  Printf.printf

let sprintf ?(indent = 0) =
  let indent = String.make indent ' ' in
  Printf.ksprintf (( ^ ) indent)
