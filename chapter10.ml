open Imp.Ast
open Imp.Eval
open Imp.Parse
open State
open Utils

module Vars = Set.Make (struct
  type t = name
  let compare = Stdlib.compare
end)

let rec vars_of_aexpr (e : aexpr) : Vars.t =
  match e with
  | Int _ -> Vars.empty
  | Var n -> Vars.singleton n
  | Add (e1, e2) -> Vars.union (vars_of_aexpr e1) (vars_of_aexpr e2)

let rec vars_of_bexpr (e : bexpr) : Vars.t =
  match e with
  | Bool _ -> Vars.empty
  | Not e -> vars_of_bexpr e
  | And (e1, e2) -> Vars.union (vars_of_bexpr e1) (vars_of_bexpr e2)
  | Less (e1, e2) -> Vars.union (vars_of_aexpr e1) (vars_of_aexpr e2)

let rec vars_of_command (c : command) : Vars.t =
  match c with
  | Assign (x, e) -> Vars.add x (vars_of_aexpr e)
  | Seq (c1, c2) -> Vars.union (vars_of_command c1) (vars_of_command c2)
  | If (e, c1, c2) -> Vars.(union (vars_of_bexpr e) (union (vars_of_command c1) (vars_of_command c2)))
  | While (e, c) -> Vars.union (vars_of_bexpr e) (vars_of_command c)
  | Skip -> Vars.empty

let rec def_init (xs : Vars.t) (c : command) : Vars.t * bool =
  let aok e = Vars.subset (vars_of_aexpr e) xs in
  let bok e = Vars.subset (vars_of_bexpr e) xs in
  match c with
  | Assign (x, e) when aok e ->
    (Vars.add x xs, true)
  | Assign _ (* when not (aok e) *) ->
    (xs, false)
  | Seq (c1, c2) ->
    let (xs1, ok1) = def_init xs c1 in
    let (xs2, ok2) = def_init xs1 c2 in
    (xs2, ok1 && ok2)
  | If (e, c1, c2) when bok e ->
    let (xs1, ok1) = def_init xs c1 in
    let (xs2, ok2) = def_init xs c2 in
    (Vars.inter xs1 xs2, ok1 && ok2)
  | If _ (* when not (bok e) *) ->
    (xs, false)
  | While (e, c) when bok e ->
    let (_, ok) = def_init xs c in
    (xs, ok)
  | While _ (* when not (bok e) *) ->
    (xs, false)
  | Skip -> (xs, true)

let is_def_init (c : command) : bool =
  snd (def_init Vars.empty c)

let test_def_init () =
  [parse "x := 1; y := 2; z := x + y"]
  |> assert_property is_def_init ~name:"Definitely initialized"

let dom (s : state) (xs : name list) : Vars.t =
  List.fold_left (fun xs x ->
      match s x with
      | _ -> Vars.add x xs
      | exception Failure _ -> xs
    ) Vars.empty xs

(* Lemma 10.1 *)
let initialized_aexpr (e : aexpr) (s : state) (xs : name list) : bool =
  let dom s = dom s xs in
  if Vars.subset (vars_of_aexpr e) (dom s) then
    Option.is_some (aeval_opt e s)
  else true

(* Lemma 10.2 *)
let initialized_bexpr (e : bexpr) (s : state) (xs : name list) : bool =
  let dom s = dom s xs in
  if Vars.subset (vars_of_bexpr e) (dom s) then
    Option.is_some (beval_opt e s)
  else true

(* Initialization-sensitive small-step semantics *)
let rec small_step ((c, s) : command * state) : command * state =
  match c with
  | Assign (x, e) ->
    begin match aeval_opt e s with
    | Some i -> (Skip, bind x i s)
    | None -> failwith "stuck"
    end
  | Seq (Skip, c2) -> (c2, s)
  | Seq (c1, c2) ->
    let (c1', s') = small_step (c1, s) in
    (Seq (c1', c2), s')
  | If (e, c1, c2) ->
    begin match beval_opt e s with
    | Some true -> (c1, s)
    | Some false -> (c2, s)
    | None -> failwith "stuck"
    end
  | While (e, c) ->
    (If (e, Seq (c, While (e, c)), Skip), s)
  | Skip -> (c, s)

(* Lemma 10.3 *)
let lemma_10_3 (xs : Vars.t) (c : command) : bool =
  let (xs', ok) = def_init xs c in
  if ok then Vars.subset xs xs' else true

(* Initialization-sensitive big-step semantics *)
let rec big_step (c : command) (s : state option) : state option =
  match s with
  | Some s ->
    begin match c with
    | Assign (x, e) ->
      begin match aeval_opt e s with
      | Some i -> Some (bind x i s)
      | None -> None
      end
    | Seq (c1, c2) -> big_step c1 (Some s) |> big_step c2
    | If (e, c1, c2) ->
      begin match beval_opt e s with
      | Some true -> big_step c1 (Some s)
      | Some false -> big_step c2 (Some s)
      | None -> None
      end 
    | While (e, c) ->
      begin match beval_opt e s with
      | Some true -> big_step c (Some s) |> big_step (While (e, c))
      | Some false -> Some s
      | None -> None
      end
    | Skip -> Some s
    end
  (* Error propagation *)
  | None -> None

let well_init_programs_do_not_go_wrong (c : command) (s : state) (xs : name list) : bool =
  let dom s = dom s xs in
  let (_, ok) = def_init (dom s) c in
  if ok then big_step c (Some s) |> Option.is_some else true

let test_well_init_programs_do_not_go_wrong () =
  let c = List.map parse [
    "x := 1; y := 2; z := x + y";
    "if i < 10 { x := 2 } else { x := y }";
    "while i < 10 { n := i + -1; i := i + 1 }";
  ]
  in
  let s = assign [("i", 0)] in
  let xs = ["i"; "n"; "x"; "y"; "z"] in
  let p = fun c -> well_init_programs_do_not_go_wrong c s xs in
  assert_property p c ~name:"Well-initialized programs do not go wrong"
  
(* Exercise 10.1 *)
let rec ivars (c : command) : Vars.t =
  match c with
  | Assign (x, _) -> Vars.singleton x
  | Seq (c1, c2) -> Vars.union (ivars c1) (ivars c2)
  | If (_, c1, c2) -> Vars.inter (ivars c1) (ivars c2)
  | While _ | Skip -> Vars.empty

let ivars_subset_all_vars (c : command) : bool =
  Vars.subset (ivars c) (vars_of_command c)

let test_ivars_subset_all_vars () =
  [Imp.Examples.fib; Imp.Examples.sum]
  |> assert_property ivars_subset_all_vars ~name:"ivars_subset_all_vars"

(* Exercise 10.1 *)
let rec ok (xs : Vars.t) (c : command) : bool =
  let aok e = Vars.subset (vars_of_aexpr e) xs in
  let bok e = Vars.subset (vars_of_bexpr e) xs in
  let cok ?(xs = xs) c = ok (Vars.union xs (ivars c)) c in
  match c with
  | Assign (_, e) -> aok e
  | Seq (c1, c2) -> cok c1 && cok c2 ~xs:(Vars.union xs (ivars c1))
  | If (e, c1, c2) -> bok e && cok c1 && cok c2
  | While (e, c) -> bok e && cok c
  | Skip -> true

let test_ok () =
  [Imp.Examples.fib; Imp.Examples.sum]
  |> assert_property (ok (Vars.singleton "n")) ~name:"Only initialized variables are read"

let exercise_10_1_a (xs : Vars.t) (c : command) : bool =
  let (xs', ok') = def_init xs c in
  if ok' then
    Vars.equal xs' (Vars.union xs (ivars c)) && ok xs c
  else true

let exercise_10_1_b (xs : Vars.t) (c : command) : bool =
  if ok xs c then
    let (xs', ok') = def_init xs c in
    ok' && Vars.equal xs' (Vars.union xs (ivars c))
  else true

let exercise_10_1 (xs : Vars.t) (c : command) : bool =
  exercise_10_1_a xs c && exercise_10_1_b xs c

let test_exercise_10_1 () =
  [parse "x := 1; y := 2; z := x + y"]
  |> assert_property (exercise_10_1 Vars.empty) ~name:"Exercise 10.1"

let () =
  test_def_init ();
  test_well_init_programs_do_not_go_wrong ();
  test_ivars_subset_all_vars ();
  test_ok ();
  test_exercise_10_1 ();
