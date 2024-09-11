let is_prime n =
  let rec loop k =
    if n < 2 then false
    else if n = 2 then true
    else if n mod k = 0 then false
    else if Assign00_01.sqrt n <= k then true
      else loop (k + 1)
  in
  loop 2