let mk_unique_keys alst =
  let rec loop_add lst name num =
    match lst with
    | [] -> num
    | head :: rest ->
      let (x, y) = head in
      if x = name
        then loop_add rest name (num + y)
      else
        loop_add rest name num
  in
  let rec loop lst result =
    match lst with
    | [] -> result
    | head :: rest -> 
      let (x, y) = head in
      match List.assoc_opt x result with
      | None -> loop rest ((fst(head), loop_add rest x y) :: result)
      | Some _ -> loop rest result
  in
  loop alst []