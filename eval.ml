open Ast
open State

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

let rec ceval (c : command) (s : state) : state =
  match c with
  | Assign (x, e) -> bind x (aeval e s) s
  | Seq (c1, c2) -> ceval c1 s |> ceval c2
  | If (e, c1, _) when beval e s -> ceval c1 s
  | If (_, _, c2) (* when not (beval e s) *) -> ceval c2 s
  | While (e, c) when beval e s -> ceval c s |> ceval (While (e, c))
  | While (_, _) (* when not (beval e s) *) -> s
  | Skip -> s
