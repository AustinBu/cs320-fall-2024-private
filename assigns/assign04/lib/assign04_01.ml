let last_function_standing funcs start pred =
  match funcs with
  | [] -> None
  | head :: _ ->
  let rec iterate funcs start pred func max same =
    match funcs with
    | [] -> func, same
    | head :: tail -> 
      let rec loop func s p count seen =
        let s = func s in
        if List.mem s seen then
          max_int
        else if count > 1000 then 1001
        else if p s then count
        else loop func s p (count + 1) (s :: seen)
      in let c = loop head start pred 0 []
      in if c > max
        then iterate tail start pred head c false
      else if c = max
        then iterate tail start pred func max true
      else
        iterate tail start pred func max false
    in let x, y = iterate funcs start pred head (-1) false
    in if y = true
      then None
    else Some x
