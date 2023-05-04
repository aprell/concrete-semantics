(* IMP abstract syntax *)

type aexpr =
  | Int of int
  | Var of name
  | Add of aexpr * aexpr

and name = string

type state = name -> int

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
