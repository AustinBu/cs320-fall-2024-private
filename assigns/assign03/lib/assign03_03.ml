type tree = 
| Leaf of int
| Node of tree list

let rec height t =
  match t with
  | Leaf _ -> 0
  | Node cs ->
     let rec max_depth cs =
       match cs with
       | [] -> -1
       | c :: cs -> max (height c) (max_depth cs)
     in 1 + max_depth cs

let collapse h t =
  let rec replace count t1 =
    if count <= 0
      then t1
    else
        let rec move n f =
          match n with
          | [] -> []
          | head :: rest -> 
            match head with
            | Leaf le -> Leaf le :: move rest f
            | Node no -> 
              if f = true then
                move no false @ move rest f
              else
                no @ move rest f
          in
        let new_n = Node (move [t1] true)
      in
    replace (count - 1) new_n
  in
  if height t <= h
    then t
  else
    replace (height t - h) t