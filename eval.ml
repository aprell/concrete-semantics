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

let rec aeval_opt (e : aexpr) (s : state) : int option =
  match e with
  | Int n -> Some n
  | Var x -> (try Some (s x) with Failure _ -> None)
  | Add (e1, e2) ->
    begin match aeval_opt e1 s, aeval_opt e2 s with
    | Some n, Some m -> Some (n + m)
    | _ -> None
    end

let rec beval_opt (e : bexpr) (s : state) : bool option =
  match e with
  | Bool b -> Some b
  | Not e ->
    begin match beval_opt e s with
    | Some b -> Some (not b)
    | None -> None
    end
  | And (e1, e2) ->
    begin match beval_opt e1 s with
    | Some b1 ->
      begin match beval_opt e2 s with
      | Some b2 -> Some (b1 && b2)
      | None -> None
      end
    | None -> None
    end
  | Less (e1, e2) ->
    begin match aeval_opt e1 s, aeval_opt e2 s with
    | Some a1, Some a2 -> Some (a1 < a2)
    | _ -> None
    end

let rec ceval (c : command) (s : state) : state =
  match c with
  | Assign (x, e) -> bind x (aeval e s) s
  | Seq (c1, c2) -> ceval c1 s |> ceval c2
  | If (e, c1, _) when beval e s -> ceval c1 s
  | If (_, _, c2) (* when not (beval e s) *) -> ceval c2 s
  | While (e, c) when beval e s -> ceval c s |> ceval (While (e, c))
  | While (_, _) (* when not (beval e s) *) -> s
  | Skip -> s

module Typed = struct
  open Ast.Typed
  open State.Typed

  let rec aeval_opt (e : aexpr) (s : state) : value option =
    match e with
    | Int n -> Some (Int n)
    | Real n -> Some (Real n)
    | Var x -> (try Some (s x) with Failure _ -> None)
    | Add (e1, e2) ->
      begin match aeval_opt e1 s, aeval_opt e2 s with
      | Some (Int n), Some (Int m) -> Some (Int (n + m))
      | Some (Real n), Some (Real m) -> Some (Real (n +. m))
      | _ -> None
      end

  (* Exercise 9.2 *)
  let rec aeval (e : aexpr) (s : state) : value =
    match e with
    | Int n -> Int n
    | Real n -> Real n
    | Var x -> s x
    | Add (e1, e2) ->
      begin match aeval e1 s, aeval e2 s with
      | Int n, Int m -> Int (n + m)
      | Int n, Real m
      | Real m, Int n -> Real (float_of_int n +. m)
      | Real n, Real m -> Real (n +. m)
      end

  let rec beval (e : bexpr) (s : state) : bool option =
    match e with
    | Bool b -> Some b
    | Not e ->
      begin match beval e s with
      | Some b -> Some (not b)
      | None -> None
      end
    | And (e1, e2) ->
      begin match beval e1 s with
      | Some b1 ->
        begin match beval e2 s with
        | Some b2 -> Some (b1 && b2)
        | None -> None
        end
      | None -> None
      end
    | Less (e1, e2) ->
      begin match aeval_opt e1 s, aeval_opt e2 s with
      | Some a1, Some a2 -> Some (a1 < a2)
      | _ -> None
      end

  (* Small-step semantics *)
  let rec ceval ((c, s) : command * state) : command * state =
    match c with
    | Assign (x, e) ->
      begin match aeval_opt e s with
      | Some a -> (Skip, bind x a s)
      | None -> failwith "stuck"
      end
    | Seq (Skip, c2) -> (c2, s)
    | Seq (c1, c2) -> let (c1', s') = ceval (c1, s) in (Seq (c1', c2), s')
    | If (e, c1, c2) ->
      begin match beval e s with
      | Some true -> (c1, s)
      | Some false -> (c2, s)
      | None -> failwith "stuck"
      end
    | While (e, c) -> (If (e, Seq (c, While (e, c)), Skip), s)
    | Skip -> (c, s)

  let final ((c, _) : command * state) : bool = c = Skip

  let rec ceval_star (cs : command * state) : state =
    let (_, s') as cs' = ceval cs in
    if not (final cs') then ceval_star cs' else s'
end
