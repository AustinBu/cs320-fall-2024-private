type matrix = {
  entries : float list list;
  rows : int;
  cols : int;
}

let mk_matrix entries shape =
  let rec create_matrix columns l =
    if List.length l <= columns then
      [l]
    else
      let create_rows columns l =
        let rec loop new_l columns l =
          match (columns, l) with
          | (0, temp) -> (List.rev new_l, temp)
          | (_, [] ) -> (List.rev new_l, [])
          | (_, head :: rest) -> loop (head :: new_l) (columns - 1) rest
        in
        loop [] columns l
      in
      let (row, back) = create_rows columns l in
      row :: create_matrix columns back
  in
  let (rows, columns) = shape in
  {
    entries = 
    if List.length entries = 0 then [] 
    else
      create_matrix columns entries;
    rows = rows;
    cols = columns;
  }
