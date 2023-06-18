(* Typed IMP *)

open Ast.Typed

type t = Int | Real

type env = name -> t

and name = string

let empty = fun _ -> failwith "unknown type"

let bind n t env =
  fun x -> if x = n then t else env x

let assign ns_ts =
  List.fold_left
    (fun env (n, t) -> bind n t env)
    empty
    ns_ts

let rec of_aexpr_opt (e : aexpr) (env : env) : t option =
  match e with
  | Int _ -> Some Int
  | Real _ -> Some Real
  | Var x -> Some (env x)
  | Add (e1, e2) ->
    begin match of_aexpr_opt e1 env, of_aexpr_opt e2 env with
    | Some Int, Some Int -> Some Int
    | Some Real, Some Real -> Some Real
    | _ -> None
    end

let rec is_well_typed_bexpr (e : bexpr) (env : env) : bool =
  match e with
  | Bool _ -> true
  | Not e -> is_well_typed_bexpr e env
  | And (e1, e2) -> is_well_typed_bexpr e1 env && is_well_typed_bexpr e2 env
  | Less (e1, e2) ->
    begin match of_aexpr_opt e1 env, of_aexpr_opt e2 env with
    | Some Int, Some Int
    | Some Real, Some Real -> true
    | _ -> false
    end

let rec is_well_typed_command (c : command) (env : env) : bool =
  match c with
  | Assign (x, e) ->
    begin match of_aexpr_opt e env with
    | Some t -> t = env x
    | None -> false
    end
  | Seq (c1, c2) -> is_well_typed_command c1 env && is_well_typed_command c2 env
  | If (e, c1, c2) -> is_well_typed_bexpr e env && is_well_typed_command c1 env && is_well_typed_command c2 env
  | While (e, c) -> is_well_typed_bexpr e env && is_well_typed_command c env
  | Skip -> true
