let nth s i =
  let n = Assign01_02.nth_prime i
  in let rec loop s n =
    if s mod n <> 0 then 0
    else 1 + loop (s/n) n
  in loop s n