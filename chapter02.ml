open Utils

let conj (a : bool) (b : bool) : bool =
  match a, b with
  | true, true -> true
  | _, _ -> false

type nat = Z | S of nat

let rec add (a : nat) (b : nat) : nat =
  match a, b with
  | Z, _ -> b
  | S n, _ -> S (add n b)

let add_m_0 (m : nat) : bool =
  add m Z = m

let test_add_m_0 () =
  [Z; S Z; S (S Z); S (S (S Z)); S (S (S (S Z)))]
  |> assert_property add_m_0 ~name:"add_m_0"

type 'a list = Nil | Cons of 'a * 'a list

let cons (x : 'a) (xs : 'a list) : 'a list =
  Cons (x, xs)

let rec snoc (x : 'a) (xs: 'a list) : 'a list =
  match xs with
  | Nil -> Cons (x, Nil)
  | Cons (x', xs') -> Cons (x', snoc x xs')

let rec sum_list (nats : nat list) : nat =
  match nats with
  | Nil -> Z
  | Cons (n, ns) -> add n (sum_list ns)

let rec append (xs : 'a list) (ys : 'a list) : 'a list =
  match xs, ys with
  | Nil, _ -> ys
  | Cons (x, xs'), _ -> Cons (x, append xs' ys)

let rec reverse (xs : 'a list) : 'a list =
  match xs with
  | Nil -> Nil
  | Cons (x, xs') -> append (reverse xs') (Cons (x, Nil))

let rev_rev (xs : 'a list) : bool =
  xs |> reverse |> reverse = xs

let test_rev_rev () =
  [Nil; Cons (1, Nil); Cons (1, Cons (2, Nil)); Cons (1, Cons (2, Cons (3, Nil)))]
  |> assert_property rev_rev ~name:"rev_rev"

let rev_app (xs : 'a list) (ys : 'a list) : bool =
  reverse (append xs ys) = append (reverse ys) (reverse xs)

let test_rev_app () =
  let xs = [Nil; Cons (1, Nil); Cons (1, Cons (2, Nil)); Cons (1, Cons (2, Cons (3, Nil)))] in
  let assert_property = assert_property ~name:"rev_app" in
  assert_property (rev_app Nil) xs;
  assert_property (rev_app (Cons (1, Nil))) xs;
  assert_property (rev_app (Cons (1, Cons (2, Nil)))) xs;
  assert_property (rev_app (Cons (1, Cons (2, Cons (3, Nil))))) xs

type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

let rec sum_tree (nats : nat tree) : nat =
  match nats with
  | Leaf -> Z
  | Node (l, n, r) -> add n (add (sum_tree l) (sum_tree r))

let rec mirror (t : 'a tree) : 'a tree =
  match t with
  | Leaf -> Leaf
  | Node (l, n, r) -> Node (mirror r, n, mirror l)

let mirror_mirror (t : 'a tree) : bool =
  t |> mirror |> mirror = t

let test_mirror_mirror () =
  [ Leaf;
    Node (Leaf, 1, Leaf);
    Node (Node (Leaf, 2, Leaf), 1, Leaf);
    Node (Leaf, 1, Node (Leaf, 3, Leaf));
    Node (Node (Leaf, 2, Leaf), 1, Node (Leaf, 3, Leaf));
    Node (Node (Leaf, 2, Leaf), 1, Node (Node (Leaf, 4, Leaf), 3, Leaf));
  ] |> assert_property mirror_mirror ~name:"mirror_mirror"

let contents (t : 'a tree) : 'a list =
  let rec loop t lst =
    match t with
    | Leaf -> lst
    | Node (l, n, r) -> Cons (n, append (loop l lst) (loop r lst))
  in
  loop t Nil

let exercise_2_6 (t : nat tree) : bool =
  sum_tree t = sum_list (contents t)

let test_exercise_2_6 () =
  let _1 = S Z in
  let _2 = S (S Z) in
  let _3 = S (S (S Z)) in
  let _4 = S (S (S (S Z))) in
  [ Leaf;
    Node (Leaf, _1, Leaf);
    Node (Node (Leaf, _2, Leaf), _1, Leaf);
    Node (Leaf, _1, Node (Leaf, _3, Leaf));
    Node (Node (Leaf, _2, Leaf), _1, Node (Leaf, _3, Leaf));
    Node (Node (Leaf, _2, Leaf), _1, Node (Node (Leaf, _4, Leaf), _3, Leaf));
  ] |> assert_property exercise_2_6 ~name:"Exercise 2.6"

let preorder = contents

let postorder (t : 'a tree) : 'a list =
  let rec loop t lst =
    match t with
    | Leaf -> lst
    | Node (l, n, r) -> snoc n (append (loop l lst) (loop r lst))
  in
  loop t Nil

let exercise_2_7 (t : 'a tree) : bool =
  preorder (mirror t) = reverse (postorder t)

let test_exercise_2_7 () =
  let _1 = S Z in
  let _2 = S (S Z) in
  let _3 = S (S (S Z)) in
  let _4 = S (S (S (S Z))) in
  [ Leaf;
    Node (Leaf, _1, Leaf);
    Node (Node (Leaf, _2, Leaf), _1, Leaf);
    Node (Leaf, _1, Node (Leaf, _3, Leaf));
    Node (Node (Leaf, _2, Leaf), _1, Node (Leaf, _3, Leaf));
    Node (Node (Leaf, _2, Leaf), _1, Node (Node (Leaf, _4, Leaf), _3, Leaf));
  ] |> assert_property exercise_2_7 ~name:"Exercise 2.7"

let () =
  test_add_m_0 ();
  test_rev_rev ();
  test_rev_app ();
  test_mirror_mirror ();
  test_exercise_2_6 ();
  test_exercise_2_7 ();
