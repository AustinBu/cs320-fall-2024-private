(* basic if test *)
  let _ =
    let b = true in
    let e1 x = x + 1 in
    let e2 x = x - 1 in
    let f = if b then e1 else e2 in
    assert (f 3 = 4)