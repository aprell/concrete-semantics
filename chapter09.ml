open Imp.Ast.Typed
open Imp.Parse
open Imp.Type

let test_well_typed () =
  let c1 = Seq (Assign ("x", Var "y"), Assign ("y", Add (Var "x", Var "y"))) in
  let c2 = Seq (Assign ("x", Var "y"), Assign ("z", Add (Var "x", Var "y"))) in
  let env = assign [("x", Int); ("y", Int); ("z", Real)] in
  assert (is_well_typed_command c1 env);
  assert (not (is_well_typed_command c2 env))

let test_fib_well_typed () =
  let fib = Imp.(parse Examples.fib |> Ast.convert_command) in
  let env = assign (List.map (fun x -> (x, Int)) ["f"; "n"; "i"; "t0"; "t1"; "t2"]) in
  assert (is_well_typed_command fib env);
  assert (not (is_well_typed_command fib (bind "t2" Real env)))

let () =
  test_well_typed ();
  test_fib_well_typed ();
