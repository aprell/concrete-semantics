(* IMP abstract syntax *)

open State
open Utils

type aexpr =
  | Int of int
  | Var of name
  | Add of aexpr * aexpr

type bexpr =
  | Bool of bool
  | Not of bexpr
  | And of bexpr * bexpr
  | Less of aexpr * aexpr

type command =
  | Assign of name * aexpr
  | Seq of command * command
  | If of bexpr * command * command
  | While of bexpr * command
  | Skip

let rec pp_aexpr = function
  | Int n -> string_of_int n
  | Var x -> x
  | Add (e1, e2) -> pp_aexpr e1 ^ " + " ^ pp_aexpr e2

let rec pp_bexpr = function
  | Bool b -> string_of_bool b
  | Not e -> "!(" ^ pp_bexpr e ^ ")"
  | And (e1, e2) -> pp_bexpr e1 ^ " && " ^ pp_bexpr e2
  | Less (e1, e2) -> pp_aexpr e1 ^ " < " ^ pp_aexpr e2

let rec pp_command = function
  | Assign (x, e) -> x ^ " := " ^ pp_aexpr e
  | Seq (c1, c2) -> pp_command c1 ^ "; " ^ pp_command c2
  | If (e, c1, c2) -> "if " ^ pp_bexpr e ^ " { " ^ pp_command c1 ^ " } else { " ^ pp_command c2 ^ " }"
  | While (e, c) -> "while " ^ pp_bexpr e ^ " { " ^ pp_command c ^ " }"
  | Skip -> "skip"

module Typed = struct
  type aexpr =
    | Int of int
    | Real of float
    | Var of name
    | Add of aexpr * aexpr

  type bexpr =
    | Bool of bool
    | Not of bexpr
    | And of bexpr * bexpr
    | Less of aexpr * aexpr

  type command =
    | Assign of name * aexpr
    | Seq of command * command
    | If of bexpr * command * command
    | While of bexpr * command
    | Skip
end

let rec convert_aexpr (e : aexpr) : Typed.aexpr =
  match e with
  | Int i -> Typed.Int i
  | Var x -> Typed.Var x
  | Add (e1, e2) -> Typed.Add (convert_aexpr e1, convert_aexpr e2)

let rec convert_bexpr (e : bexpr) : Typed.bexpr =
  match e with
  | Bool b -> Typed.Bool b
  | Not e -> Typed.Not (convert_bexpr e)
  | And (e1, e2) -> Typed.And (convert_bexpr e1, convert_bexpr e2)
  | Less (e1, e2) -> Typed.Less (convert_aexpr e1, convert_aexpr e2)

let rec convert_command (c : command) : Typed.command =
  match c with
  | Assign (x, e) -> Typed.Assign (x, convert_aexpr e)
  | Seq (c1, c2) -> Typed.Seq (convert_command c1, convert_command c2)
  | If (e, c1, c2) -> Typed.If (convert_bexpr e, convert_command c1, convert_command c2)
  | While (e, c) -> Typed.While (convert_bexpr e, convert_command c)
  | Skip -> Typed.Skip

module Annotated = struct
  type 'a command =
    | Assign of name * aexpr * 'a
    | Seq of 'a command * 'a command
    (* if b { {P1} c1 } else { {P2} c2 } {Q} *)
    | If of bexpr * 'a * 'a command * 'a * 'a command * 'a
    (* {I} while b { {P} c } {Q} *)
    | While of 'a * bexpr * 'a * 'a command * 'a
    | Skip of 'a

  let pp_command' = pp_command

  let rec pp_command (show : 'a -> name) ?(indent = 0) = function
    | Assign (x, e, p) ->
      sprintf ~indent "%s {%s}" (pp_command' (Assign (x, e))) (show p)
    | Seq (c1, c2) ->
      pp_command ~indent show c1 ^ ";\n" ^ pp_command ~indent show c2
    | If (e, p1, c1, p2, c2, q) ->
      sprintf ~indent "if %s {\n" (pp_bexpr e) ^
      sprintf ~indent:(indent + 2) "{%s}\n" (show p1) ^
      sprintf ~indent "%s\n" (pp_command ~indent:(indent + 2) show c1) ^
      sprintf ~indent "} else {\n" ^
      sprintf ~indent:(indent + 2) "{%s}\n" (show p2) ^
      sprintf ~indent "%s\n" (pp_command ~indent:(indent + 2) show c2) ^
      sprintf ~indent "}\n{%s}" (show q)
    | While (i, e, p, c, q) ->
      sprintf ~indent "{%s}\nwhile %s {\n" (show i) (pp_bexpr e) ^
      sprintf ~indent:(indent + 2) "{%s}\n" (show p) ^
      sprintf ~indent "%s\n" (pp_command ~indent:(indent + 2) show c) ^
      sprintf ~indent "}\n{%s}" (show q)
    | Skip p ->
      sprintf ~indent "%s\n{%s}" (pp_command' Skip) (show p)
end
