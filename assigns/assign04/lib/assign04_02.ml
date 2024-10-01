type expr = 
| True
| False
| Num of int
| Or of expr * expr
| Add of expr * expr
| IfThenElse of expr * expr * expr

type ty = 
| Int
| Bool

(* let type_of e = e
  let rec bool_loop e =
    match e with
    | True -> true
    | False -> false
    | Or (x, y) -> 
      match (x, y) with
        | (True, _) | (_, True) -> true
        | (False, False) -> false
        | (_, _) -> bool_loop x || bool_loop y
    | _ -> false
    in
    let rec primitive_loop e =
    match e with
    | True -> Some Bool
    | False -> Some Bool
    | Num x -> Some Int
    | Or (x, y) -> 
      match (x, y) with
        | (True, True) | (False, True) | (True, False) | (False, False) -> Some Bool
        |(_, _) -> None
    | Add (x, y) ->
      match (x, y) with
      | (Num _, Num _) -> Some Int
      | (_, _) -> None
    | IfThenElse (x, y, z) ->
      if bool_loop x
        then primitive_loop y
      else
        primitive_loop z
    in primitive_loop e *)
  
let type_of e = 
  match e with
  | _ -> None