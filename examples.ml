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
  let fib = Ast.convert_command fib,
            Type.(assign_all Int ["f"; "i"; "n"; "t0"; "t1"; "t2"])
  let sum = Ast.convert_command sum,
            Type.(assign_all Int ["i"; "n"; "s"])
end
