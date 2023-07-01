open Parse

let fib = parse {|
  t0 := 0;
  t1 := 1;
  if n < 2 {
    f := n
  } else {
    i := 2;
    while i < n + 1 {
      t2 := t0 + t1;
      t0 := t1;
      t1 := t2;
      i := i + 1
    };
    f := t1
  }
|}

let sum = parse {|
  s := 0;
  i := 1;
  while i < n + 1 {
    s := s + i;
    i := i + 1
  }
|}

module Typed = struct
  let fib = Ast.convert_command fib
  let fib_type_env = ["f"; "n"; "i"; "t0"; "t1"; "t2"] |> Type.(assign_all Int)
  let sum = Ast.convert_command sum
  let sum_type_env = ["s"; "n"; "i"] |> Type.(assign_all Int)
end
