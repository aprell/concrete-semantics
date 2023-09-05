open Imp.Ast
open Imp.Parse
open OUnit2

let parse_equal s c =
  assert_equal c (parse s)

let unparse_equal c s =
  assert_equal s (unparse c)

let test_parse_assignment _ =
  parse_equal "a := 1"
    (Assign ("a", Int 1));
  parse_equal "b := -2"
    (Assign ("b", Int (-2)));
  parse_equal "c := a + b + 1"
    (Assign ("c", Add (Add (Var "a", Var "b"), Int 1)))

let test_parse_sequential _ =
  parse_equal "a := 1; b := 2"
    (Seq (Assign ("a", Int 1), Assign ("b", Int 2)));
  parse_equal "a := 1; b := 2; skip"
    (Seq (Seq (Assign ("a", Int 1), Assign ("b", Int 2)), Skip));
  parse_equal "a := 1; if b < a { b := a }; c := a + b"
    (Seq (Seq (Assign ("a", Int 1), If (Less (Var "b", Var "a"), Assign ("b", Var "a"), Skip)),
          Assign ("c", Add (Var "a", Var "b"))))

let test_parse_conditional _ =
  parse_equal "if true { skip }"
    (If (Bool true, Skip, Skip));
  parse_equal "if !true { skip } else { skip }"
    (If (Not (Bool true), Skip, Skip));
  parse_equal "if !false && n < 10 { skip } else { skip }"
    (If (And (Not (Bool false), Less (Var "n", Int 10)), Skip, Skip))

let test_parse_loop _ =
  parse_equal "while true { skip }"
    (While (Bool true, Skip));
  parse_equal "while n < 10 { skip }"
    (While (Less (Var "n", Int 10), Skip));
  parse_equal "while !(n < 10) { x := 1; y := x }"
    (While (Not (Less (Var "n", Int 10)), Seq (Assign ("x", Int 1), Assign ("y", Var "x"))))

let test_unparse_assignment _ =
  unparse_equal (parse "a := 1")
    "a := 1";
  unparse_equal (parse "b := -2")
    "b := -2";
  unparse_equal (parse "c := a + b + 1")
    "c := a + b + 1"

let test_unparse_sequential _ =
  unparse_equal (parse "a := 1; b := 2")
    "a := 1; b := 2";
  unparse_equal (parse "a := 1; b := 2; skip")
    "a := 1; b := 2; skip";
  unparse_equal (parse "a := 1; if b < a { b := a }; c := a + b")
    "a := 1; if b < a { b := a } else { skip }; c := a + b"

let test_unparse_conditional _ =
  unparse_equal (parse "if true { skip }")
    "if true { skip } else { skip }";
  unparse_equal (parse "if !true { skip } else { skip }")
    "if !(true) { skip } else { skip }";
  unparse_equal (parse "if !false && n < 10 { skip } else { skip }")
    "if !(false) && n < 10 { skip } else { skip }"

let test_unparse_loop _ =
  unparse_equal (parse "while true { skip }")
    "while true { skip }";
  unparse_equal (parse "while n < 10 { skip }")
    "while n < 10 { skip }";
  unparse_equal (parse "while !(n < 10) { x := 1; y := x }")
    "while !(n < 10) { x := 1; y := x }"

let suite =
  test_list [
    "Parser tests" >::: [
      "Assignments" >:: test_parse_assignment;
      "Sequentials" >:: test_parse_sequential;
      "Conditionals" >:: test_parse_conditional;
      "Loops" >:: test_parse_loop;
    ];
    "Unparser tests" >::: [
      "Assignments" >:: test_unparse_assignment;
      "Sequentials" >:: test_unparse_sequential;
      "Conditionals" >:: test_unparse_conditional;
      "Loops" >:: test_unparse_loop;
    ]
  ]

let () = run_test_tt_main suite
