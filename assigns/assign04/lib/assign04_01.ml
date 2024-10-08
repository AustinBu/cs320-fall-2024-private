let last_function_standing funcs start pred =
  let rec resolve funcs start pred =
    let l = (fst funcs) (fst start), (snd funcs) (snd start)
    in 
    if pred (fst l) && pred (snd l)
      then (fst funcs), true
    else if pred (fst l) then (snd funcs), false
    else if pred (snd l) then (fst funcs), false
    else resolve funcs l pred
  in
  match funcs with
  | [] -> None
  | head :: _ ->
  let rec iterate funcs start pred func max same =
    match funcs with
    | [] -> func, same
    | head :: tail -> 
      let rec loop func s p count max =
        let s = func s in 
        if count > 1000 then 1001
        else if p s then count
        else loop func s p (count + 1) max
      in let c = loop head start pred 0 max
      in if c > max
        then iterate tail start pred head c false
      else if c = max
        then if c = 1001
          then let x, y = resolve (head, func) (start, start) pred in
          if y
            then iterate tail start pred x max true
          else iterate tail start pred x max false
        else iterate tail start pred func max true
      else
        iterate tail start pred func max same
    in let x, y = iterate funcs start pred head (-1) false
    in if y = true
      then None
    else Some x
