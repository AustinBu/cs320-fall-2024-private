let sqrt n =
  let rec loop k =
    if k * k >= n then k
    else loop (k + 1)
  in
  loop 0
