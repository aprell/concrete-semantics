open Imp.Ast
open Imp.Eval
open Imp.Parse
open State
open Utils

type state = (name * int) list

module States = Set.Make (struct
  type t = state
  let compare = Stdlib.compare
end)

module Ints = Set.Make (struct
  type t = int
  let compare = Stdlib.compare
end)

let string_of_ints ints =
  Ints.elements ints
  |> List.map string_of_int
  |> String.concat ", "
  |> Printf.sprintf "{%s}"

let rec asize (c : command) : int =
  match c with
  | Assign _ -> 1
  | Seq (c1, c2) -> asize c1 + asize c2
  | If (_, c1, c2) -> asize c1 + asize c2 + 3
  | While (_, c) -> asize c + 3
  | Skip -> 1

let shift (f : int -> 'a) (n : int) : int -> 'a =
  fun p -> f (p + n)

let rec annotate (f : int -> 'a) (c : command) : 'a Annotated.command =
  match c with
  | Assign (x, e) -> Annotated.Assign (x, e, f 0)
  | Seq (c1, c2) ->
    let c1' = annotate f c1 in
    let c2' = annotate (shift f (asize c1)) c2 in
    Annotated.Seq (c1', c2')
  | If (e, c1, c2) ->
    let p1 = f 0 in
    let c1' = annotate (shift f 1) c1 in
    let p2 = f (asize c1 + 1) in
    let c2' = annotate (shift f (asize c1 + 2)) c2 in
    let q = f (asize c1 + asize c2 + 2) in
    Annotated.If (e, p1, c1', p2, c2', q)
  | While (e, c) ->
    let i = f 0 in
    let p = f 1 in
    let c' = annotate (shift f 2) c in
    let q = f (asize c + 2) in
    Annotated.While (i, e, p, c', q)
  | Skip -> Annotated.Skip (f 0)

let rec strip (c : 'a Annotated.command) : command =
  match c with
  | Annotated.Assign (x, e, _) -> Assign (x, e)
  | Annotated.Seq (c1, c2) -> Seq (strip c1, strip c2)
  | Annotated.If (e, _, c1, _, c2, _) -> If (e, strip c1, strip c2)
  | Annotated.While (_, e, _, c, _) -> While (e, strip c)
  | Annotated.Skip _ -> Skip

let rec annotations (c : 'a Annotated.command) : 'a list =
  match c with
  | Annotated.Assign (_, _, p) -> [p]
  | Annotated.Seq (c1, c2) -> annotations c1 @ annotations c2
  | Annotated.If (_, p1, c1, p2, c2, q) -> p1 :: annotations c1 @ p2 :: annotations c2 @ [q]
  | Annotated.While (i, _, p, c, q) -> i :: p :: annotations c @ [q]
  | Annotated.Skip p -> [p]

let annotation (c : 'a Annotated.command) (n : int) : 'a =
  List.nth (annotations c) n

let post_annotation (c : 'a Annotated.command) : 'a =
  List.(hd (rev (annotations c)))

let rec aeval (e : aexpr) (s : state) : int =
  match e with
  | Int n -> n
  | Var x -> List.assoc x s
  | Add (e1, e2) -> aeval e1 s + aeval e2 s

let rec beval (e : bexpr) (s : state) : bool =
  match e with
  | Bool b -> b
  | Not e -> not (beval e s)
  | And (e1, e2) -> beval e1 s && beval e2 s
  | Less (e1, e2) -> aeval e1 s < aeval e2 s

let rec step (s : States.t) (c : States.t Annotated.command) : States.t Annotated.command =
  match c with
  | Annotated.Assign (x, e, _) ->
    let p' = States.map (fun t -> (x, (aeval e t)) :: t) s in
    Annotated.Assign (x, e, p')
  | Annotated.Seq (c1, c2) ->
    Annotated.Seq (step s c1, step (post_annotation c1) c2)
  | Annotated.If (e, p1, c1, p2, c2, _) ->
    let p1', p2' = States.partition (fun t -> beval e t) s in
    let q' = States.union (post_annotation c1) (post_annotation c2) in
    Annotated.If (e, p1', step p1 c1, p2', step p2 c2, q')
  | Annotated.While (i, e, p, c, _) ->
    let i' = States.union s (post_annotation c) in
    let p', q' = States.partition (fun t -> beval e t) i in
    Annotated.While (i', e, p', step p c, q')
  | Annotated.Skip _ ->
    Annotated.Skip s

let lfp f =
  until (fun x ->
      let sx = annotations x in
      let sy = annotations (f x) in
      List.for_all2 States.equal sx sy
    ) f

let collecting_semantics (c : command) (s : States.t) : States.t Annotated.command =
  annotate (function _ -> States.empty) c |> lfp (step s)

let values_of (x : name) (s : States.t) : Ints.t =
  States.fold (fun t acc -> Ints.add (List.assoc x t) acc) s Ints.empty

let show (xs : name list) (s : States.t) : string =
  List.map (fun x -> Printf.sprintf "%s := %s" x (values_of x s |> string_of_ints)) xs
  |> String.concat ", "
  |> Printf.sprintf "{%s}"

let all_pairs xs ys =
  List.concat_map (fun x -> List.map (fun y -> [x; y]) ys) xs

let test_collecting_semantics_1 () =
  let c = parse "x := x + 1; x := x + y" in
  let s = States.of_list (all_pairs [("x", 2); ("x", 7)] [("y", 3); ("y", 4)]) in
  let cs = Printf.sprintf "\n{%s}\n%s\n"
    (show ["x"; "y"] s)
    (Annotated.pp_command (show ["x"; "y"]) (collecting_semantics c s))
  in
  assert (cs = {|
{{x := {2, 7}, y := {3, 4}}}
x := x + 1 {{x := {3, 8}, y := {3, 4}}};
x := x + y {{x := {6, 7, 11, 12}, y := {3, 4}}}
|})

let test_collecting_semantics_2 () =
  let c = parse "if x < 5 { x := x + 1 } else { x := x + 2 }" in
  let s = States.of_list [[("x", 2)]; [("x", 7)]] in
  let cs = Printf.sprintf "\n{%s}\n%s\n"
    (show ["x"] s)
    (Annotated.pp_command (show ["x"]) (collecting_semantics c s))
  in
  assert (cs = {|
{{x := {2, 7}}}
if x < 5 {
  {{x := {2}}}
  x := x + 1 {{x := {3}}}
} else {
  {{x := {7}}}
  x := x + 2 {{x := {9}}}
}
{{x := {3, 9}}}
|})

let test_collecting_semantics_3 () =
  let c = parse "x := 0; while x < 3 { x := x + 2 }" in
  let s = States.singleton [("x", 0)] in
  let cs = Printf.sprintf "\n{%s}\n%s\n"
    (show ["x"] s)
    (Annotated.pp_command (show ["x"]) (collecting_semantics c s))
  in
  assert (cs = {|
{{x := {0}}}
x := 0 {{x := {0}}};
{{x := {0, 2, 4}}}
while x < 3 {
  {{x := {0, 2}}}
  x := x + 2 {{x := {2, 4}}}
}
{{x := {4}}}
|})

(* Limited to variables x and y *)
let defunc (s : name -> int) : state =
  (try [("x", s "x")] with _ -> []) @
  (try [("y", s "y")] with _ -> [])

let func (s : state) : name -> int =
  List.fold_right (fun (n, v) s -> bind n v s) s empty

let lemma_13_8 (c : command) (s : name -> int) : bool =
  let t = ceval c s in
  let cs = collecting_semantics c (States.singleton (defunc s)) in
  States.exists (fun s ->
      try List.for_all2 ( = ) (defunc t) s with _ -> true
    ) (post_annotation cs) = true

let test_lemma_13_8 () =
  let c = parse "x := 0; while x < 3 { x := x + 2 }" in
  let s = assign [] in
  let p = fun c -> lemma_13_8 c s in
  assert_property p [c] ~name:"Lemma 13.8"

type 'av st = (name * 'av) list

module Vars = Set.Make (struct
  type t = name
  let compare = Stdlib.compare
end)

type parity = None | Even | Odd | Either

let string_of_parity = function
  | None -> "None"
  | Even -> "Even"
  | Odd -> "Odd"
  | Either -> "Either"

let assoc x xs =
  List.assoc_opt x xs
  |> Option.value ~default:None

let order_parity a b = a = None || a = b || b = Either

let order_parity' a b =
  let names = Vars.of_list (fst (List.split a) @ fst (List.split b)) in
  Vars.fold (fun x s ->
      let av, av' = assoc x a, assoc x b in
      (x, order_parity av av') :: s
    ) names []
  |> List.for_all snd

let join_parity a b =
  match a, b with
  | None, a
  | a, None -> a
  | _ when a = b -> a
  | _ -> Either

let join_parity' a b =
  let names = Vars.of_list (fst (List.split a) @ fst (List.split b)) in
  Vars.fold (fun x s ->
      let av, av' = assoc x a, assoc x b in
      (x, join_parity av av') :: s
    ) names []

(* To be generalized later *)

let rec aeval_parity (e : aexpr) (s : parity st) : parity =
  match e with
  | Int n -> num_parity n
  | Var x -> List.assoc x s
  | Add (e1, e2) -> plus_parity (aeval_parity e1 s) (aeval_parity e2 s)

and num_parity n = if n mod 2 = 0 then Even else Odd

and plus_parity a b =
  match a, b with
  | Even, Even
  | Odd, Odd -> Even
  | Even, Odd
  | Odd, Even -> Odd
  | Either, _
  | _, Either -> Either
  | _ -> assert false

let asem_parity x e s =
  match List.assoc_opt x s with
  | Some _ -> (x, aeval_parity e s) :: s
  | None -> (x, None) :: s

let bsem_parity _e s = s

let rec step_parity
  (s : parity st)
  (c : parity st Annotated.command)
     : parity st Annotated.command =
  match c with
  | Annotated.Assign (x, e, _) ->
    Annotated.Assign (x, e, asem_parity x e s)
  | Annotated.Seq (c1, c2) ->
    Annotated.Seq (step_parity s c1, step_parity (post_annotation c1) c2)
  | Annotated.If (e, p1, c1, p2, c2, _) ->
    let p1', p2' = bsem_parity e s, bsem_parity (Not e) s in 
    let q' = join_parity' (post_annotation c1) (post_annotation c2) in
    Annotated.If (e, p1', step_parity p1 c1, p2', step_parity p2 c2, q')
  | Annotated.While (i, e, p, c, _) ->
    let i' = join_parity' s (post_annotation c) in
    let p', q' = bsem_parity e i, bsem_parity (Not e) i in
    Annotated.While (i', e, p', step_parity p c, q')
  | Annotated.Skip _ ->
    Annotated.Skip s

let pfp order f s =
  until (fun x -> order (f x) x) f s

let bot (c : command) =
  annotate (function _ -> []) c

let abs_interp_parity (c : command) (s : parity st) =
  pfp (fun x y ->
      let sx = annotations x in
      let sy = annotations y in
      List.for_all2 order_parity' sx sy
    ) (step_parity s) (bot c)

let show_parity (xs : name list) (s : parity st) : string =
  List.map (fun x -> Printf.sprintf "%s := %s" x (List.assoc x s |> string_of_parity)) xs
  |> String.concat ", "
  |> Printf.sprintf "%s"

let test_abs_interp_parity () =
  let c = parse "x := 3; while x < 10 { x := x + 2 }" in
  let s = [("x", Either)] in
  let ai = Printf.sprintf "\n{%s}\n%s\n"
    (show_parity ["x"] s)
    (Annotated.pp_command (show_parity ["x"]) (abs_interp_parity c s))
  in
  assert (ai = {|
{x := Either}
x := 3 {x := Odd};
{x := Odd}
while x < 10 {
  {x := Odd}
  x := x + 2 {x := Odd}
}
{x := Odd}
|})

let () =
  test_collecting_semantics_1 ();
  test_collecting_semantics_2 ();
  test_collecting_semantics_3 ();
  test_lemma_13_8 ();
  test_abs_interp_parity ();
