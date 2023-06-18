(* IMP abstract syntax *)

open State

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

(* Typed IMP *)

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
