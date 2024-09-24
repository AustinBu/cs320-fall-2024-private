let gen_fib l k =
  let rec loop l sum iter =
    if iter <= 0
      then sum
    else
      match l with
      | [] -> sum
      | head :: rest -> 
        let x = sum + sum - head in
        loop (rest @ [x]) x (iter - 1)
  in
  let rec init l k iter sum =
    match l with
    | [] -> sum, k - iter
    | head :: rest -> 
      if iter = k
        then head, k - iter
      else
        init rest k (iter + 1) (sum + head)
  in
  let (x, y) = init l k 0 0
  in
  if k = 0 then x
  else loop (l @ [x]) x y
    
