open Assign00_01

let is_prime n =
  let rec loop k =
    if n < 2 then false
    else if k > sqrt n then true
    else if n mod k = 0 then false
    else loop (k + 1)
  in
  loop 2