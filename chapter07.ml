open Imp.Ast
open Imp.Parse
open State
open Utils

let rec aeval (e : aexpr) (s : state) : int =
  match e with
  | Int n -> n
  | Var x -> s x
  | Add (e1, e2) -> aeval e1 s + aeval e2 s

let rec beval (e : bexpr) (s : state) : bool =
  match e with
  | Bool b -> b
  | Not e -> not (beval e s)
  | And (e1, e2) -> beval e1 s && beval e2 s
  | Less (e1, e2) -> aeval e1 s < aeval e2 s

let rec big_step (c : command) (s : state) : state =
  match c with
  | Assign (x, e) -> bind x (aeval e s) s
  | Seq (c1, c2) -> big_step c1 s |> big_step c2
  | If (e, c1, _) when beval e s -> big_step c1 s
  | If (_, _, c2) (* when not (beval e s) *) -> big_step c2 s
  | While (e, c) when beval e s -> big_step c s |> big_step (While (e, c))
  | While (_, _) (* when not (beval e s) *) -> s
  | Skip -> s

let test_fib_big_step () =
  let fib_ast = parse Imp.Examples.fib in
  let fib n =
    let s = assign [("n", n)] in
    big_step fib_ast s "f"
  in
  [0; 1; 1; 2; 3; 5; 8; 13; 21; 34; 55]
  |> List.iteri (fun i n -> assert (fib i == n))

let equiv_state_ (s : state) (s' : state) (xs : name list) : bool =
  List.for_all (fun x -> s x = s' x) xs

let equiv_state (s : state) (s' : state) : bool =
  equiv_state_ s s' ["a"; "b"; "c"; "i"; "x"; "y"; "z"]

(* Semantic equivalence *)
let equiv_command (c : command) (c' : command) (s : state) : bool =
  equiv_state (big_step c s) (big_step c' s)

let test_equiv_command () =
  (* Lemma 7.3 *)
  let c = "while i < 10 { x := x + y; i := i + 1 }" in
  let c' = "if i < 10 { x := x + y; i := i + 1; " ^ c ^ "}" in
  [ assign [("i",  0)];
    assign [("i",  2); ("x", 1)];
    assign [("i",  5); ("x", 2); ("y", 3)];
    assign [("i", 10); ("x", 3); ("y", 4)];
  ] |> assert_property (equiv_command (parse c) (parse c')) ~name:"equiv_command"

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

let test_fib_small_step () =
  let fib_ast = parse Imp.Examples.fib in
  let fib n =
    let s = assign [("n", n)] in
    small_steps (fib_ast, s) "f"
  in
  [0; 1; 1; 2; 3; 5; 8; 13; 21; 34; 55]
  |> List.iteri (fun i n -> assert (fib i == n))

(* Equivalence of big-step and small-step semantics *)
let equiv_semantics ((c, s) : config) : bool =
  equiv_state (big_step c s) (small_steps (c, s))

let test_equiv_semantics () =
  let c = parse "while i < 10 { x := x + y; i := i + 1 }" in
  let s = [ assign [("i",  0)];
            assign [("i",  2); ("x", 1)];
            assign [("i",  5); ("x", 2); ("y", 3)];
            assign [("i", 10); ("x", 3); ("y", 4)]; ]
  in
  let p = fun s -> equiv_semantics (c, s) in
  assert_property p s ~name:"equiv_semantics"

let () =
  test_fib_big_step ();
  test_equiv_command ();
  test_fib_small_step ();
  test_equiv_semantics ();