open Imp
open Ast
open Eval
open Parse
open State
open Utils

let rec big_step (c : command) (s : state) : state =
  match c with
  | Assign (x, e) -> bind x (aeval e s) s
  | Seq (c1, c2) -> big_step c1 s |> big_step c2
  | If (e, c1, _) when beval e s -> big_step c1 s
  | If (_, _, c2) (* when not (beval e s) *) -> big_step c2 s
  | While (e, c) when beval e s -> big_step c s |> big_step (While (e, c))
  | While (_, _) (* when not (beval e s) *) -> s
  | Skip -> s

let test_big_step () =
  let fib n = big_step Examples.fib (assign [("n", n)]) "f" in
  [0; 1; 1; 2; 3; 5; 8; 13; 21; 34; 55]
  |> List.iteri (fun i n -> assert (fib i == n));
  let sum n = big_step Examples.sum (assign [("n", n)]) "s" in
  [0; 1; 3; 6; 10; 15; 21; 28; 36; 45; 55]
  |> List.iteri (fun i n -> assert (sum i == n))

(* Semantic equivalence *)
let equiv_command (c : command) (c' : command) (s : state) (xs : name list) : bool =
  equivalent (big_step c s) (big_step c' s) xs

let test_equiv_command () =
  (* Lemma 7.3 *)
  let c = "while i < 10 { x := x + y; i := i + 1 }" in
  let c' = "if i < 10 { x := x + y; i := i + 1; " ^ c ^ "}" in
  let p = fun s -> equiv_command (parse c) (parse c') s ["i"; "x"; "y"] in
  [ assign [("i",  0); ("x", 0); ("y", 0)];
    assign [("i",  2); ("x", 1); ("y", 0)];
    assign [("i",  5); ("x", 2); ("y", 3)];
    assign [("i", 10); ("x", 3); ("y", 4)];
  ] |> assert_property p ~name:"equiv_command"

type config = command * state

let final ((c, _) : config) : bool = c = Skip

let rec small_step ((c, s) : config) : config =
  match c with
  | Assign (x, e) -> (Skip, bind x (aeval e s) s)
  | Seq (c1, c2) when c1 = Skip -> (c2, s)
  | Seq (c1, c2) -> let (c1', s') = small_step (c1, s) in (Seq (c1', c2), s')
  | If (e, c1, _) when beval e s -> (c1, s)
  | If (_, _, c2) (* when not (beval e s) *) -> (c2, s)
  | While (e, c) -> (If (e, Seq (c, While (e, c)), Skip), s)
  | Skip -> (c, s)

let rec small_steps (cs : config) : state =
  let (_, s') as cs' = small_step cs in
  if not (final cs') then small_steps cs' else s'

let test_small_step () =
  let fib n = small_steps (Examples.fib, assign [("n", n)]) "f" in
  [0; 1; 1; 2; 3; 5; 8; 13; 21; 34; 55]
  |> List.iteri (fun i n -> assert (fib i == n));
  let sum n = small_steps (Examples.sum, assign [("n", n)]) "s" in
  [0; 1; 3; 6; 10; 15; 21; 28; 36; 45; 55]
  |> List.iteri (fun i n -> assert (sum i == n))

(* Equivalence of big-step and small-step semantics *)
let equiv_semantics ((c, s) : config) (xs : name list) : bool =
  equivalent (big_step c s) (small_steps (c, s)) xs

let test_equiv_semantics () =
  let c = "while i < 10 { x := x + y; i := i + 1 }" in
  let p = fun s -> equiv_semantics (parse c, s) ["i"; "x"; "y"] in
  [ assign [("i",  0); ("x", 0); ("y", 0)];
    assign [("i",  2); ("x", 1); ("y", 0)];
    assign [("i",  5); ("x", 2); ("y", 3)];
    assign [("i", 10); ("x", 3); ("y", 4)];
  ] |> assert_property p ~name:"equiv_semantics"

let () =
  test_big_step ();
  test_equiv_command ();
  test_small_step ();
  test_equiv_semantics ();
