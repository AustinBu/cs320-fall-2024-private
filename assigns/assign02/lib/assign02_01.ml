type piece = 
| X
| O
type pos = 
| Piece of piece
| Blank
type row_index = 
| Top
| Middle
| Bottom
type col_index = 
| Left
| Middle
| Right

let get_pos board pos_index =
  let (row_str, column_str) = pos_index in
  match board with
  | ((a1, a2, a3), (b1, b2, b3), (c1, c2, c3)) ->
    let row = match row_str with
      | Top -> (a1, a2, a3)
      | Middle -> (b1, b2, b3)
      | Bottom -> (c1, c2, c3)
    in
    let (x, y, z) = row in
    match column_str with
    | Left ->  x
    | Middle -> y
    | Right -> z

let winner board =
  match board with
  | ((a1, a2, a3), (b1, b2, b3), (c1, c2, c3)) ->
    let list a = match a with
      | 1 -> (a1, a2, a3)
      | 2 -> (b1, b2, b3)
      | 3 -> (c1, c2, c3)
      | 4 -> (a1, b1, c1)
      | 5 -> (a2, b2, c2)
      | 6 -> (a3, b3, c3)
      | 7 -> (a1, b2, c3)
      | 8 -> (a3, b2, c1) 
      | _ -> failwith "Invalid index"
    in
    let check l =
      let (x, y, z) = l in
      if x = y && y = z then true
      else false
    in
    let rec loop size =
      if size = 0 then
        false
      else if
        let l = list size in
        check l then
          true
      else
        loop (size - 1)
    in loop 8

