let assert_property ?name p xs =
  try assert (List.for_all p xs) with
  Assert_failure _ ->
    Option.iter (Printf.eprintf ">>> \027[4m%s\027[0m failed\n") name;
    failwith "assert_property"

let equivalent f g xs =
  List.for_all (fun x -> f x = g x) xs
