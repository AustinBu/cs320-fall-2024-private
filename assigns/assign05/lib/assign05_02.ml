type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree

let sum_tr t =
  let rec loop t cont =
    match t with
    | Leaf -> cont 0
    | Node (x, l, r) ->
        loop l (fun sum_l -> loop r (fun sum_r -> cont (x + sum_l + sum_r)))
  in
  loop t (fun x -> x)
