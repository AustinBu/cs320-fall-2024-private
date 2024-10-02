open Assign04_02

type value = 
| VNum of int
| VBool of bool

let rec bool_loop e =
  match e with
  | True -> true
  | False -> false
  | IfThenElse (x, y, z) -> 
      if bool_loop x then bool_loop y
        else bool_loop z
  | Or (x, y) -> 
    (match (x, y) with
      | (True, False) | (False, True) | (True, True) -> true
      | (False, False) -> false
      | (_, _) -> bool_loop x || bool_loop y)
  | _ -> false

let rec num_loop e =
  match e with
  | Num x -> x
  | Add (x, y) -> (num_loop x) + (num_loop y)
  | IfThenElse (x, y, z) ->
    if bool_loop x then
      num_loop y
    else num_loop z
  | _ -> 0
  
let eval e = 
  let rec primitive_loop e =
    match e with
    | True -> VBool true
    | False -> VBool false
    | Num x -> VNum x
    | Or (x, y) ->
      (match (x, y) with
      | (True, _) | (_, True) -> VBool true
      | (False, False) -> VBool false
      | _ -> VBool false)
    | Add (x, y) -> VNum (num_loop x + num_loop y)
    | IfThenElse (x, y, z) ->
      if bool_loop x then primitive_loop y
      else primitive_loop z
    in
    primitive_loop e