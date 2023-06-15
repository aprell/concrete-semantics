open Imp.Ast
open Imp.Eval
open Imp.Parse
open State
open Utils

type instr =
  | Load_imm of int
  | Load_var of name
  | Store_var of name 
  | Add
  | Jump of addr
  | Jump_lt of addr
  | Jump_ge of addr

and addr = int

type config = pc * state * stack

and pc = int

and stack = int list

let hd = List.hd

let tl = List.tl

let hd2 xs = hd (tl xs)

let tl2 xs = tl (tl xs)

let iexec (i : instr) ((pc, s, t) : config) : config =
  match i with
  | Load_imm n -> (pc + 1, s, n :: t)
  | Load_var x -> (pc + 1, s, s x :: t)
  | Store_var x -> (pc + 1, bind x (hd t) s, tl t)
  | Add -> (pc + 1, s, hd2 t + hd t :: tl2 t)
  | Jump n -> (pc + 1 + n, s, t)
  | Jump_lt n when hd2 t < hd t -> (pc + 1 + n, s, tl2 t)
  | Jump_lt _ (* when hd2 t >= hd t *) -> (pc + 1, s, tl2 t)
  | Jump_ge n when hd2 t >= hd t -> (pc + 1 + n, s, tl2 t)
  | Jump_ge _ (* when hd2 t < hd t *) -> (pc + 1, s, tl2 t)

let exec1 (is : instr list) ((pc, _, _) as cfg : config) : config =
  assert (0 <= pc && pc < List.length is);
  iexec (List.nth is pc) cfg

let rec exec (is : instr list) ((pc, _, _) as cfg : config) : config =
  if pc < List.length is then exec is (exec1 is cfg) else cfg

let test_exec () =
  let p = [Load_var "y"; Store_var "x"] in
  let s = assign [("x", 3); ("y", 4)] in
  let (pc, s, t) = exec p (0, s, []) in
  assert ((pc, List.map s ["x"; "y"], t) = (2, [4; 4], []))

let rec acomp (e : aexpr) : instr list =
  match e with
  | Int n -> [Load_imm n]
  | Var x -> [Load_var x]
  | Add (e1, e2) -> acomp e1 @ acomp e2 @ [Add]

let acomp_correct (e : aexpr) (s : state) (t : stack) : bool =
  let is = acomp e in
  let (pc', _, t') = exec is (0, s, t) in
  pc' = List.length is && t' = aeval e s :: t

let test_acomp_correct () =
  let e = [Imp.Ast.(Add (Add (Var "x", Int 2), Var "y"))] in
  let s = assign [("x", 1); ("y", 2)] in
  let p = fun e -> acomp_correct e s [] in
  assert_property p e ~name:"acomp_correct"

let rec bcomp (e : bexpr) (flag : bool) (offset : int) : instr list =
  match e with
  | Bool b when b = flag -> [Jump offset]
  | Bool _ (* when b <> flag *) -> []
  | Not e -> bcomp e (not flag) offset
  | And (e1, e2) when flag ->
    let ce2 = bcomp e2 flag offset in
    let ce1 = bcomp e1 false (List.length ce2) in
    ce1 @ ce2
  | And (e1, e2) (* when not flag *) ->
    let ce2 = bcomp e2 flag offset in
    let ce1 = bcomp e1 false (List.length ce2 + offset) in
    ce1 @ ce2
  | Less (e1, e2) when flag ->
    acomp e1 @ acomp e2 @ [Jump_lt offset]
  | Less (e1, e2) (* when not flag *) ->
    acomp e1 @ acomp e2 @ [Jump_ge offset]

let bcomp_correct (e : bexpr) (s : state) (t : stack) : bool =
  let is = bcomp e true 3 in (* offset >= 0 *)
  let (pc', _, _) = exec is (0, s, t) in
  pc' = List.length is + if beval e s then 3 else 0

let test_bcomp_correct () =
  let e = [Imp.Ast.(And (Less (Var "x", Var "y"), Bool true))] in
  let s = assign [("x", 1); ("y", 2)] in
  let p = fun e -> bcomp_correct e s [] in
  assert_property p e ~name:"bcomp_correct"

let rec ccomp (c : command) : instr list =
  match c with
  | Assign (x, e) -> acomp e @ [Store_var x]
  | Seq (c1, c2) -> ccomp c1 @ ccomp c2
  | If (e, c1, Skip) ->
    (* Exercise 8.1 *)
    let cc1 = ccomp c1 in
    let ce = bcomp e false (List.length cc1 + 1) in
    ce @ cc1
  | If (e, c1, c2) ->
    let cc1 = ccomp c1 in
    let cc2 = ccomp c2 in
    let ce = bcomp e false (List.length cc1 + 1) in
    ce @ cc1 @ [Jump (List.length cc2)] @ cc2
  | While (e, c) ->
    let cc = ccomp c in
    let ce = bcomp e false (List.length cc + 1) in
    ce @ cc @ [Jump (-List.(length ce + length cc + 1))]
  | Skip -> []

let equiv_state_ (s : state) (s' : state) (xs : name list) : bool =
  List.for_all (fun x -> s x = s' x) xs

let equiv_state (s : state) (s' : state) : bool =
  equiv_state_ s s' ["a"; "b"; "c"; "i"; "x"; "y"; "z"]

let ccomp_correct (c : command) (s : state) (t : stack) : bool =
  let is = ccomp c in
  let (pc', s', _) = exec is (0, s, t) in
  pc' = List.length is && equiv_state s' (ceval c s)

let test_ccomp_correct () =
  let c = [parse "while i < 10 { x := x + y; i := i + 1 }"] in
  let s = assign [("i",  0); ("x", 3); ("y", 4)] in
  let p = fun c -> ccomp_correct c s [] in
  assert_property p c ~name:"ccomp_correct"

let () =
  test_exec ();
  test_acomp_correct ();
  test_bcomp_correct ();
  test_ccomp_correct ();
