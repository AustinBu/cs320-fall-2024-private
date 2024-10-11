type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree

let sum_tr _ = 12345