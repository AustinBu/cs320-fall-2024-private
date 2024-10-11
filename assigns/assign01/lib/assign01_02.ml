let nth_prime n2 =
  let rec loop i k2 =
    let is_prime n1 =
      let rec loop k1 =
        if n1 < 2 then false
        else if n1 = 2 then true
        else if n1 mod k1 = 0 then false
        else
          let sqrt n =
            let rec loop1 k =
              if k * k >= n then k
              else loop1 (k + 1)
            in
            loop1 0
          in if sqrt n1 <= k1 then true
          else loop (k1 + 1)
      in
      loop 2
    in if is_prime i 
      then 
        if k2 = 0
          then i 
        else loop (i + 1) (k2 - 1)
    else loop (i + 1) k2
  in loop 2 n2