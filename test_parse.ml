open Imp.Ast
open Imp.Parse
open OUnit2

let peq s v =
  assert_equal v (parse s)

let test_parse_assignment _ =
  peq "a := 1"
      (Assign ("a", Int 1));
  peq "b := -2"
      (Assign ("b", Int (-2)));
  peq "c := a + b + 1"
      (Assign ("c", Add (Add (Var "a", Var "b"), Int 1)))

let test_parse_sequential _ =
  peq "a := 1; b := 2"
      (Seq (Assign ("a", Int 1), Assign ("b", Int 2)));
  peq "a := 1; b := 2; skip"
      (Seq (Seq (Assign ("a", Int 1), Assign ("b", Int 2)), Skip));
  peq "a := 1; if b < a { b := a }; c := a + b"
      (Seq (Seq (Assign ("a", Int 1), If (Less (Var "b", Var "a"), Assign ("b", Var "a"), Skip)),
            Assign ("c", Add (Var "a", Var "b"))))

let test_parse_conditional _ =
  peq "if true { skip }"
      (If (Bool true, Skip, Skip));
  peq "if !true { skip } else { skip }"
      (If (Not (Bool true), Skip, Skip));
  peq "if !false && n < 10 { skip } else { skip }"
      (If (And (Not (Bool false), Less (Var "n", Int 10)), Skip, Skip))

let test_parse_loop _ =
  peq "while true { skip }"
      (While (Bool true, Skip));
  peq "while n < 10 { skip }"
      (While (Less (Var "n", Int 10), Skip));
  peq "while !(n < 10) { x := 1; y := x }"
      (While (Not (Less (Var "n", Int 10)), Seq (Assign ("x", Int 1), Assign ("y", Var "x"))))

let suite =
  "Parser tests" >::: [
    "Assignments" >:: test_parse_assignment;
    "Sequentials" >:: test_parse_sequential;
    "Conditionals" >:: test_parse_conditional;
    "Loops" >:: test_parse_loop;
  ]

let () = run_test_tt_main suite
