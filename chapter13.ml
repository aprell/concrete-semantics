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

module type Abstract_domain = sig
  (* Type of abstract values *)
  type t
  (* Least element *)
  val bot : t
  (* Greatest element *)
  val top : t
  val to_string : t -> string
  val show : name list -> t st -> string
  (* <= *)
  val order : t st -> t st -> bool
  (* Least upper bound of x and y *)
  val join : t st -> t st -> t st
  (* Abstract arithmetic *)
  val aeval : aexpr -> t st -> t
end

module Parity = struct
  type t = None | Even | Odd | Either

  let bot = None

  let top = Either

  let to_string = function
    | None -> "None"
    | Even -> "Even"
    | Odd -> "Odd"
    | Either -> "Either"

  let show (xs : name list) (s : t st) : string =
    List.map (fun x -> Printf.sprintf "%s := %s" x (List.assoc x s |> to_string)) xs
    |> String.concat ", "
    |> Printf.sprintf "%s"

  let assoc x xs =
    List.assoc_opt x xs
    |> Option.value ~default:None

  let order' a b = a = None || a = b || b = Either

  let order a b =
    let names = Vars.of_list (fst (List.split a) @ fst (List.split b)) in
    Vars.fold (fun x s ->
        let av, av' = assoc x a, assoc x b in
        (x, order' av av') :: s
      ) names []
    |> List.for_all snd

  let join' a b =
    match a, b with
    | None, a
    | a, None -> a
    | _ when a = b -> a
    | _ -> Either

  let join a b =
    let names = Vars.of_list (fst (List.split a) @ fst (List.split b)) in
    Vars.fold (fun x s ->
        let av, av' = assoc x a, assoc x b in
        (x, join' av av') :: s
      ) names []

  let rec aeval (e : aexpr) (s : t st) : t =
    match e with
    | Int n when n mod 2 = 0 -> Even
    | Int _ (* when n mod 2 <> 0 *) -> Odd
    | Var x -> List.assoc x s
    | Add (e1, e2) -> plus (aeval e1 s) (aeval e2 s)

  and plus a b =
    match a, b with
    | Even, Even
    | Odd, Odd -> Even
    | Even, Odd
    | Odd, Even -> Odd
    | Either, _
    | _, Either -> Either
    | _ -> assert false
end

module Abstract_interpreter (Domain : Abstract_domain) = struct
  let rec step
    (s : Domain.t st)
    (c : Domain.t st Annotated.command)
       : Domain.t st Annotated.command =
    match c with
    | Annotated.Assign (x, e, _) ->
      Annotated.Assign (x, e, asem x e s)
    | Annotated.Seq (c1, c2) ->
      Annotated.Seq (step s c1, step (post_annotation c1) c2)
    | Annotated.If (e, p1, c1, p2, c2, _) ->
      let p1', p2' = bsem e s, bsem (Not e) s in
      let q' = Domain.join (post_annotation c1) (post_annotation c2) in
      Annotated.If (e, p1', step p1 c1, p2', step p2 c2, q')
    | Annotated.While (i, e, p, c, _) ->
      let i' = Domain.join s (post_annotation c) in
      let p', q' = bsem e i, bsem (Not e) i in
      Annotated.While (i', e, p', step p c, q')
    | Annotated.Skip _ ->
      Annotated.Skip s

  and asem x e s =
    match List.assoc_opt x s with
    | Some _ -> (x, Domain.aeval e s) :: s
    | None -> (x, Domain.bot) :: s

  and bsem _e s = s

  let pfp order f s =
    until (fun x -> order (f x) x) f s

  let bot (c : command) =
    annotate (function _ -> []) c

  let run (c : command) (s : Domain.t st) =
    pfp (fun x y ->
        let sx = annotations x in
        let sy = annotations y in
        List.for_all2 Domain.order sx sy
      ) (step s) (bot c)
end

module Parity_interpreter = Abstract_interpreter (Parity)

let test_parity_1 () =
  let c = parse "x := 3; while x < 10 { x := x + 2 }" in
  let s = [("x", Parity.top)] in
  let ai = Printf.sprintf "\n{%s}\n%s\n"
    (Parity.show ["x"] s)
    (Annotated.pp_command (Parity.show ["x"]) (Parity_interpreter.run c s))
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

(* Exercise 13.10 *)
let test_parity_2 () =
  let c = parse "x := 3; while x < 10 { x := x + 1 }" in
  let s = [("x", Parity.top)] in
  let ai = Printf.sprintf "\n{%s}\n%s\n"
    (Parity.show ["x"] s)
    (Annotated.pp_command (Parity.show ["x"]) (Parity_interpreter.run c s))
  in
  assert (ai = {|
{x := Either}
x := 3 {x := Odd};
{x := Either}
while x < 10 {
  {x := Either}
  x := x + 1 {x := Either}
}
{x := Either}
|})

let () =
  test_collecting_semantics_1 ();
  test_collecting_semantics_2 ();
  test_collecting_semantics_3 ();
  test_lemma_13_8 ();
  test_parity_1 ();
  test_parity_2 ();