let group l =
  let check_valid alis =
    match alis with
    | [] -> true
    | head :: _ ->
      if head = 0 then false
      else
        let rec loop blis sign flip =
          match blis with
          | [] -> true
          | head :: rest ->
            if head = 0
              then if flip then false
              else loop rest sign true
            else if (flip) 
              then if ((head > 0) = sign) then false
              else loop rest (not sign) false
            else if head < 0 = sign then false
            else loop rest sign false
          in
          loop alis (head > 0) false
        in
  let rec inner_loop in_lis =
    match in_lis with
    | [] -> []
    | head :: rest ->
      if head = 0
        then []
      else
        head :: inner_loop rest
      in
    let rec outer_loop input_lis =
      match input_lis with
      | [] -> []
      | head :: rest ->
        if head = 0
          then [inner_loop rest] @ outer_loop rest
        else
          outer_loop rest
      in
    match check_valid l with
    | false -> None
    | true -> Some ([inner_loop l] @ outer_loop l)
