let fib = {|
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
