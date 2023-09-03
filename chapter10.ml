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
  List.map parse [
    "x := 1; y := 2; z := x + y";
    "i := 0; while i < 10 { n := i + -1; i := i + 1 }";
  ] |> assert_property is_def_init ~name:"Definitely initialized"

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
  let p = fun c ->
    let s = assign [("i", 0)] in
    let xs = ["i"; "n"; "x"; "y"; "z"] in
    well_init_programs_do_not_go_wrong c s xs
  in
  List.map parse [
    "x := 1; y := 2; z := x + y";
    "if i < 10 { x := 2 } else { x := y }";
    "while i < 10 { n := i + -1; i := i + 1 }";
  ] |> assert_property p ~name:"Well-initialized programs do not go wrong"
  
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

let test_only_ivars_are_read () =
  [Imp.Examples.fib; Imp.Examples.sum]
  |> assert_property (ok (Vars.singleton "n")) ~name:"Only initialized variables are read"

let exercise_10_1 (xs : Vars.t) (c : command) : bool =
  let (xs', ok') = def_init xs c in
  let eq = Vars.equal xs' (Vars.union xs (ivars c)) in
  if ok' then eq && ok xs c else true &&
  if ok xs c then ok' && eq else true

let test_exercise_10_1 () =
  List.map parse [
    "x := 1; y := 2; z := x + y";
    "if i < 10 { x := 2 } else { x := y }";
    "while i < 10 { n := i + -1; i := i + 1 }";
  ] |> assert_property (exercise_10_1 Vars.empty) ~name:"Exercise 10.1"

type const = name -> int option

let lookup (x : name) (t : const) : int option =
  try t x with Failure _ -> None

let rec afold (e : aexpr) (t : const) : aexpr =
  match e with
  | Int _ -> e
  | Var x ->
    begin match lookup x t with
    | Some n -> Int n
    | None -> e
    end
  | Add (e1, e2) ->
    begin match afold e1 t, afold e2 t with
    | Int n, Int m -> Int (n + m)
    (* Exercise 10.2 *)
    | e', Int 0 | Int 0, e' -> e'
    | e1', e2' -> Add (e1', e2')
    end

let approx (t : const) (s : state) (xs : name list) : bool =
  List.for_all (fun x -> Option.fold (lookup x t) ~some:(( = ) (s x)) ~none:true) xs

let afold_correct (e : aexpr) (t : const) (s : state) (xs : name list) : bool =
  if approx t s xs then
    aeval (afold e t) s = aeval e s
  else true

let test_afold_correct () =
  let e = Add (Add (Var "x", Int 2), Var "y") in
  let s = assign [("x", 1); ("y", 3)] in
  let t = [
    assign [("x", None); ("y", None)];
    assign [("x", Some 1); ("y", None)];
    assign [("x", Some 1); ("y", Some 3)] ]
  in
  let p = fun t -> afold_correct e t s ["x"; "y"] in
  assert_property p t ~name:"afold_correct"

(* Exercise 10.4 *)
let rec bfold (e : bexpr) (t : const) : bexpr =
  match e with
  | Bool _ -> e
  | Not e ->
    begin match bfold e t with
    | Bool true -> Bool false
    | Bool false -> Bool true
    | e' -> Not e'
    end
  | And (e1, e2) ->
    begin match bfold e1 t, bfold e2 t with
    | Bool true, Bool true -> Bool true
    | Bool _, Bool _ -> Bool false
    | e1', e2' -> And (e1', e2')
    end
  | Less (e1, e2) ->
    begin match afold e1 t, afold e2 t with
    | Int n, Int m when n < m -> Bool true
    | Int _, Int _ (* when n >= m *) -> Bool false
    | e1', e2' -> Less (e1', e2')
    end

let rec lvars (c : command) : Vars.t =
  match c with
  | Assign (x, _) -> Vars.singleton x
  | Seq (c1, c2) -> Vars.union (lvars c1) (lvars c2)
  | If (_, c1, c2) -> Vars.union (lvars c1) (lvars c2)
  | While (_, c) -> lvars c
  | Skip -> Vars.empty

let merge (t1 : const) (t2 : const) : const =
  fun x -> if lookup x t1 = lookup x t2 then lookup x t1 else None

let restrict (t : const) (xs : Vars.t) ?(complement = false) : const =
  if not complement
  then fun x -> if Vars.mem x xs then lookup x t else None
  else fun x -> if Vars.mem x xs then None else lookup x t

let rec defs (c : command) (t : const) : const =
  match c with
  | Assign (x, e) ->
    begin match afold e t with
    | Int n -> bind x (Some n) t
    | _ -> bind x None t
    end
  | Seq (c1, c2) -> defs c1 t |> defs c2
  | If (_, c1, c2) -> merge (defs c1 t) (defs c2 t)
  | While (_, c) -> restrict ~complement:true t (lvars c)
  | Skip -> t

let rec fold (c : command) (t : const) : command =
  match c with
  | Assign (x, e) -> Assign (x, afold e t)
  | Seq (c1, c2) -> Seq (fold c1 t, fold c2 (defs c1 t))
  | If (e, c1, c2) -> If (e, fold c1 t, fold c2 t)
  | While (e, c) -> While (e, fold c (restrict ~complement:true t (lvars c)))
  | Skip -> Skip

let test_fold () =
  let c = parse "x := 42 + -5; y := x + x" in
  let c' = fold c empty in
  assert_equal (unparse c') "x := 37; y := 74" ~name:"test_fold"

let fold_correct (c : command) (t : const) (s : state) (xs : name list) : bool =
  if approx t s xs then
    equivalent (ceval (fold c t) s) (ceval c s) xs
  else true

let test_fold_correct () =
  let c = parse "x := 42 + -5; y := x + x" in
  let p = fun c -> fold_correct c empty empty [] in
  assert_property p [c] ~name:"fold_correct"

(* Exercise 10.4 *)
let rec fold_dce (c : command) (t : const) : command =
  match c with
  | Assign (x, e) -> Assign (x, afold e t)
  | Seq (c1, c2) -> Seq (fold_dce c1 t, fold_dce c2 (defs c1 t))
  | If (e, c1, c2) ->
    begin match bfold e t with
    | Bool true -> fold_dce c1 t
    | Bool false -> fold_dce c2 t
    | e' -> If (e', fold_dce c1 t, fold_dce c2 t)
    end
  | While (e, c) ->
    begin match bfold e t with
    | Bool false -> Skip
    | e' -> While (e', fold_dce c (restrict ~complement:true t (lvars c)))
    end
  | Skip -> Skip

let test_fold_dce () =
  let c = parse "x := 42 + -5; y := x + x; if x < y { z := x + y }" in
  let c' = fold_dce c empty in
  assert_equal (unparse c') "x := 37; y := 74; z := 111" ~name:"test_fold_dce"

let fold_dce_correct (c : command) (t : const) (s : state) (xs : name list) : bool =
  if approx t s xs then
    equivalent (ceval (fold_dce c t) s) (ceval c s) xs
  else true

let test_fold_dce_correct () =
  let c = parse "x := 42 + -5; y := x + x; if x < y { z := x + y }" in
  let p = fun c -> fold_dce_correct c empty empty [] in
  assert_property p [c] ~name:"fold_dce_correct"

let () =
  test_def_init ();
  test_well_init_programs_do_not_go_wrong ();
  test_ivars_subset_all_vars ();
  test_only_ivars_are_read ();
  test_exercise_10_1 ();
  test_afold_correct ();
  test_fold ();
  test_fold_correct ();
  test_fold_dce ();
  test_fold_dce_correct ();
