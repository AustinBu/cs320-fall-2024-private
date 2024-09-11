let to_string i =
  if i < 2 then "[]"
  else
    let rec loop i str n =
      let num = Assign01_03.nth i n
      in if num > 0
        then 
          let x = i / Assign01_01.pow (Assign01_02.nth_prime n) num
          in 
          if x = 1
            then str ^ string_of_int num ^ "]"
          else 
            let new_str = str ^ string_of_int num ^ "; "
            in loop x new_str (n + 1)
      else loop i (str ^ "0; ") (n + 1)
  in loop i "[" 0