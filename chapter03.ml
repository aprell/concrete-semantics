open State
open Utils

type aexpr =
  | Int of int
  | Var of name
  | Add of aexpr * aexpr
  | Let of name * aexpr * aexpr

let rec aeval (e : aexpr) (s : state) : int =
  match e with
  | Int n -> n
  | Var x -> s x
  | Add (e1, e2) -> aeval e1 s + aeval e2 s
  | Let (x, e1, e2) -> aeval e2 (bind x (aeval e1 s) s)

let rec asimplify (e : aexpr) : aexpr =
  match e with
  | Add (e, Int 0)
  | Add (Int 0, e) -> asimplify e
  | Add (Int n1, Int n2) -> Int (n1 + n2)
  | Add (e1, e2) -> Add (asimplify e1, asimplify e2)
  | Let (x, e1, e2) -> Let (x, asimplify e1, asimplify e2)
  | _ -> e

let asimplify_correct (e : aexpr) (s : state) : bool =
  aeval (asimplify e) s = aeval e s

let test_asimplify_correct () =
  let e = [Add (Add (Int 1, Int 2), Var "x")] in
  let s = assign [("x", 1); ("y", 2); ("x", 3)] in
  let p = Fun.flip asimplify_correct s in
  assert_property p e ~name:"asimplify_correct"

let rec optimal (e : aexpr) : bool =
  match e with
  | Add (_, Int 0)
  | Add (Int 0, _)
  | Add (Int _, Int _) -> false
  | Add (e1, e2)
  | Let (_, e1, e2) -> optimal e1 && optimal e2
  | _ -> true

let exercise_3_1 (e : aexpr) : bool =
  optimal (asimplify e) = true

let test_exercise_3_1 () =
  [Add (Add (Int 1, Int 2), Var "x")]
  |> assert_property exercise_3_1 ~name:"Exercise 3.1"

let rec subst (x : name) (a : aexpr) (e : aexpr) : aexpr =
  match e with
  | Int _ -> e
  | Var y -> if x = y then a else e
  | Add (e1, e2) -> Add (subst x a e1, subst x a e2)
  | Let (y, e1, e2) -> Let (y, subst x a e1, if x <> y then subst x a e2 else e2)

(* Substitution lemma *)
let exercise_3_3 (x : name) (a : aexpr) (e : aexpr) (s : state) : bool =
  aeval (subst x a e) s = aeval e (bind x (aeval a s) s)

let test_exercise_3_3 () =
  let e = [
    Let ("x", Int 1,
    Let ("y", Int 2,
    Add (Add (Var "x", Int 2), Var "y"))) ]
  in
  let p = Fun.flip (exercise_3_3 "y" (Int 5)) (assign []) in
  assert_property p e ~name:"Exercise 3.3"

let rec inline (e : aexpr) : aexpr =
  match e with
  | Add (e1, e2) -> Add (inline e1, inline e2)
  | Let (x, e1, e2) -> inline (subst x (inline e1) e2)
  | _ -> e

let exercise_3_6 (e : aexpr) (s : state) : bool =
  aeval (inline e) s = aeval e s

let test_exercise_3_6 () =
  let e = [
    Let ("x", Int 1,
    Let ("y", Int 2,
    Add (Add (Var "x", Int 2), Var "y"))) ]
  in
  let p = Fun.flip exercise_3_6 (assign []) in
  assert_property p e ~name:"Exercise 3.6"

type bexpr =
  | Bool of bool
  | Not of bexpr
  | And of bexpr * bexpr
  | Less of aexpr * aexpr

let rec beval (e : bexpr) (s : state) : bool =
  match e with
  | Bool b -> b
  | Not e -> not (beval e s)
  | And (e1, e2) -> beval e1 s && beval e2 s
  | Less (e1, e2) -> aeval e1 s < aeval e2 s

let rec bsimplify (e : bexpr) : bexpr =
  match e with
  | Not (Bool b) -> Bool (not b)
  | And (Bool true, b)
  | And (b, Bool true) -> bsimplify b
  | And (Bool false, _)
  | And (_, Bool false) -> Bool false
  | And (e1, e2) -> And (bsimplify e1, bsimplify e2)
  | Less (Int n1, Int n2) -> Bool (n1 < n2)
  | Less (e1, e2) -> Less (asimplify e1, asimplify e2)
  | _ -> e

let bsimplify_correct (e : bexpr) (s : state) : bool =
  beval (bsimplify e) s = beval e s

let test_bsimplify_correct () =
  let e = [And (Not (Bool true), Not (Less (Int 1, Var "x")))] in
  let s = assign [("x", 2)] in
  let p = Fun.flip bsimplify_correct s in
  assert_property p e ~name:"bsimplify_correct"

type stack_instr =
  | Load_imm_s of int
  | Load_var_s of name
  | Add_s

type stack = int list

let exec_s_instr (i : stack_instr) (s : state) (t : stack) : stack =
  match i with
  | Load_imm_s n -> n :: t
  | Load_var_s x -> s x :: t
  | Add_s -> (
    match t with
    | n1 :: n2 :: rest -> n1 + n2 :: rest
    | _ -> failwith "exec_instr"
  )

let rec exec_s (is : stack_instr list) (s : state) (t : stack) : stack =
  match is with
  | i :: is' -> exec_s_instr i s t |> exec_s is' s
  | [] -> t

let rec compile_s (e : aexpr) : stack_instr list =
  match e with
  | Int n -> [Load_imm_s n]
  | Var x -> [Load_var_s x]
  | Add (e1, e2) -> compile_s e1 @ compile_s e2 @ [Add_s]
  | Let _ -> compile_s (inline e)

let compile_s_exec_s_correct (e : aexpr) (s : state) (t : stack) : bool =
  exec_s (compile_s e) s t = aeval e s :: t

let test_compile_s_exec_s_correct () =
  let e = [
    Let ("x", Int 1,
    Let ("y", Int 2,
    Add (Add (Var "x", Int 2), Var "y"))) ]
  in
  let p = fun e -> compile_s_exec_s_correct e (assign []) [] in
  assert_property p e ~name:"compile_s_exec_s_correct"

(* Exercise 3.11 *)
type reg_instr =
  | Load_imm_r of int * reg
  | Load_var_r of name * reg
  | Add_r of reg * reg

and reg = int

type reg_state = reg -> int

let exec_r_instr (i : reg_instr) (s : state) (rs : reg_state) : reg_state =
  match i with
  | Load_imm_r (n, r) -> bind r n rs
  | Load_var_r (x, r) -> bind r (s x) rs
  | Add_r (r1, r2) -> bind r1 (rs r1 + rs r2) rs

let rec exec_r (is : reg_instr list) (s : state) (rs : reg_state) : reg_state =
  match is with
  | i :: is' -> exec_r_instr i s rs |> exec_r is' s
  | [] -> rs

let rec compile_r (e : aexpr) (r : reg) : reg_instr list =
  match e with
  | Int n -> [Load_imm_r (n, r)]
  | Var x -> [Load_var_r (x, r)]
  | Add (e1, e2) -> compile_r e1 r @ compile_r e2 (r + 1) @ [Add_r (r, r + 1)]
  | Let _ -> compile_r (inline e) r

let compile_r_exec_r_correct (e : aexpr) (s : state) (rs : reg_state) : bool =
  exec_r (compile_r e 0) s rs 0 = aeval e s

let test_compile_r_exec_r_correct () =
  let e = [
    Let ("x", Int 1,
    Let ("y", Int 2,
    Add (Add (Var "x", Int 2), Var "y"))) ]
  in
  let p = fun e -> compile_r_exec_r_correct e (assign []) (assign []) in
  assert_property p e ~name:"compile_r_exec_r_correct"

let () =
  test_asimplify_correct ();
  test_bsimplify_correct ();
  test_exercise_3_1 ();
  test_exercise_3_3 ();
  test_exercise_3_6 ();
  test_compile_s_exec_s_correct ();
  test_compile_r_exec_r_correct ();
