open Utils

let rec type_of (e : expr) : ty option =
  match e with
  | Num _ -> Some TInt
  | Add (x, y) ->
    (match type_of x, type_of y with
    | Some TInt, Some TInt -> Some TInt
    | _ -> None)
  | Lt (x, y) ->
    (match type_of x, type_of y with
    | Some TInt, Some TInt -> Some TBool
    | _ -> None)
  | Ite (x, y, z) ->
    (match type_of x, type_of y, type_of z with
    | Some TBool, Some t1, Some t2 -> 
      if t1 = t2 then Some t1 else None
    | _ -> None)