open Imp
open Ast.Typed
open Eval.Typed
open State
open Utils

let test_well_typed () =
  let c1 = Seq (Assign ("x", Var "y"), Assign ("y", Add (Var "x", Var "y"))) in
  let c2 = Seq (Assign ("x", Var "y"), Assign ("z", Add (Var "x", Var "y"))) in
  let g = Type.(assign [("x", Int); ("y", Int); ("z", Real)]) in
  assert Type.(g |- c1);
  assert Type.(not (g |- c2))

let test_fib_well_typed () =
  let fib, g = Examples.Typed.(fib, fib_type_env) in
  assert Type.(g |- fib);
  assert Type.(not (bind "t2" Real g |- fib))

let test_sum_well_typed () =
  let sum, g = Examples.Typed.(sum, sum_type_env) in
  assert Type.(g |- sum);
  assert Type.(not (bind "n" Real g |- sum))

let type_of (v : Typed.value) : Type.t =
  match v with
  | Int _ -> Type.Int
  | Real _ -> Type.Real

let ( |- ) (g : Type.env) (s : Typed.state) (xs : name list) : bool =
  List.for_all (fun x -> type_of (s x) = g x) xs

let preserve_type_of_aexpr (e : aexpr) (s : Typed.state) (g : Type.env) (xs : name list) : bool =
  match aeval_opt e s, Type.of_aexpr_opt g e with
  | Some v, Some t when (g |- s) xs -> type_of v = t
  | _ -> true

let progress_for_aexpr (e : aexpr) (s : Typed.state) (g : Type.env) (xs : name list) : bool =
  if Option.is_some (Type.of_aexpr_opt g e) && (g |- s) xs then
    Option.is_some (aeval_opt e s)
  else true

let progress_for_bexpr (e : bexpr) (s : Typed.state) (g : Type.env) (xs : name list) : bool =
  if Type.(g |~ e) && (g |- s) xs then
    Option.is_some (beval e s)
  else true

let preserve_type_of_command (c : command) (s : Typed.state) (g : Type.env) : bool =
  let (c', _) = ceval (c, s) in
  if Type.(g |- c) then Type.(g |- c') else true

let preserve_type_of_state (c : command) (s : Typed.state) (g : Type.env) (xs : name list) : bool =
  let (_, s') = ceval (c, s) in
  if Type.(g |- c) && (g |- s) xs then (g |- s') xs else true

let progress_for_command (c : command) (s : Typed.state) (g : Type.env) (xs : name list) : bool =
  if Type.(g |- c) && (g |- s) xs && c <> Skip then
    let _ = ceval (c, s) in true
  else true

let well_typed_programs_do_not_get_stuck
  (c : command) (s : Typed.state) (g : Type.env) (xs : name list) : bool =
  let rec steps ((c, s) as cs) =
    assert (preserve_type_of_command c s g);
    assert (preserve_type_of_state c s g xs);
    assert (progress_for_command c s g xs);
    let (c', _) as cs' = ceval cs in
    if c' <> Skip then steps cs' else cs'
  in
  let _ = steps (c, s) in true

let test_well_typed_programs_do_not_get_stuck () =
  let c1, s1 = Examples.Typed.fib, Typed.(assign [("n", Int 10)]) in
  let c2, s2 = Examples.Typed.sum, Typed.(assign [("n", Int 100)]) in
  let g1 = Examples.Typed.fib_type_env in
  let g2 = Examples.Typed.sum_type_env in
  let p = fun (c, s, g) -> well_typed_programs_do_not_get_stuck c s g ["n"] in
  [(c1, s1, g1); (c1, s1, Type.(bind "f" Real g1)); (c2, s2, g2); (c2, s2, Type.(bind "s" Real g2))]
  |> assert_property p ~name:"Well-typed programs do not get stuck"

let exercise_9_2 (e : aexpr) (s : Typed.state) (g : Type.env) (xs : name list) : bool =
  if (g |- s) xs then Type.of_aexpr g e = type_of (aeval e s) else true

let test_exercise_9_2 () =
  let e = [Add (Add (Var "x", Real 3.14), Add (Var "y", Int 2))] in
  let s = Typed.(assign [("x", Int 0); ("y", Real 0.)]) in
  let g = Type.(assign [("x", Int); ("y", Real)]) in
  let p = fun e -> exercise_9_2 e s g ["x"; "y"] in
  assert_property p e ~name:"Exercise 9.2"

let () =
  test_well_typed ();
  test_fib_well_typed ();
  test_sum_well_typed ();
  test_well_typed_programs_do_not_get_stuck ();
  test_exercise_9_2 ();
