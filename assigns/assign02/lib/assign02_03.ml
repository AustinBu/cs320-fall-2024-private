type dir = 
| North
| South
| East
| West

type path = dir list

let dist dirs =
  let rec loop dirs x y =
    let process direction rest x y = 
      match direction with
      | North -> loop rest x (y + 1)
      | South -> loop rest x (y - 1)
      | East -> loop rest (x + 1) y
      | West -> loop rest (x - 1) y
    in
    match dirs with
    | [] -> x, y
    | head :: rest -> process head rest x y
  in
  let x, y = loop dirs 0 0 in
  let xf = float_of_int (x * x) in
  let yf = float_of_int (y * y) in 
  sqrt (xf +. yf) 