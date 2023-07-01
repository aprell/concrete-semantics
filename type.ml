(* Typed IMP *)

open Ast.Typed

type t = Int | Real

type env = name -> t

and name = string

let empty = fun x -> failwith ("unknown type: " ^ x ^ " : ?")

let bind n t env =
  fun x -> if x = n then t else env x

let assign ns_ts =
  List.fold_left
    (fun env (n, t) -> bind n t env)
    empty
    ns_ts

let assign_all t ns =
  assign (List.map (fun x -> (x, t)) ns)

let rec of_aexpr_opt (g : env) (e : aexpr) : t option =
  match e with
  | Int _ -> Some Int
  | Real _ -> Some Real
  | Var x -> Some (g x)
  | Add (e1, e2) ->
    begin match of_aexpr_opt g e1, of_aexpr_opt g e2 with
    | Some Int, Some Int -> Some Int
    | Some Real, Some Real -> Some Real
    | _ -> None
    end

(* Exercise 9.2 *)
let rec of_aexpr (g : env) (e : aexpr) : t =
  match e with
  | Int _ -> Int
  | Real _ -> Real
  | Var x -> g x
  | Add (e1, e2) ->
    begin match of_aexpr g e1, of_aexpr g e2 with
    | Int, Int -> Int
    | _ -> Real
    end

let rec ( |~ ) (g : env) (b : bexpr) : bool =
  match b with
  | Bool _ -> true
  | Not e -> g |~ e
  | And (e1, e2) -> g |~ e1 && g |~ e2
  | Less (e1, e2) ->
    begin match of_aexpr_opt g e1, of_aexpr_opt g e2 with
    | Some Int, Some Int
    | Some Real, Some Real -> true
    | _ -> false
    end

let rec ( |- ) (g : env) (c : command) : bool =
  match c with
  | Assign (x, e) -> Option.fold (of_aexpr_opt g e) ~some:(( = ) (g x)) ~none:false
  | Seq (c1, c2) -> g |- c1 && g |- c2
  | If (e, c1, c2) -> g |~ e && g |- c1 && g |- c2
  | While (e, c) -> g |~ e && g |- c
  | Skip -> true
