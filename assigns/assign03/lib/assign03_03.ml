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
  let rec replace count t1 h =
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
            match no with
            | [] -> [Node []] @ move rest f
            | _ ->
            if f = true then
              move no false @ move rest f
            else
              no @ move rest f
        in
        let rec go_down tre h = 
          match tre with
          | [] -> []
          | head :: rest ->
            match head with
            | Leaf lea -> Leaf lea :: go_down rest h
            | Node nod ->
              if h = 0
                then [Node (move nod true)] @ go_down rest h
              else
                [Node (go_down nod (h - 1))] @ go_down rest h
          in
        let new_n = match go_down [t1] (h - 1) with
          | [] -> Node []
          | head :: _ -> head
    in
  replace (count - 1) new_n h
  in
  if height t <= h
    then t
  else
    replace (height t - h) t h
