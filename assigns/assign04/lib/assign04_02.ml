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

let rec bool_loop e =
  match e with
  | Num _ | Add (_, _) -> false, false
  | True -> true, true
  | False -> false, true
  | IfThenElse (x, y, z) -> 
    let a, b = bool_loop x in
    if b then 
      if a then bool_loop y
      else bool_loop z
    else false, false
  | Or (x, y) -> 
    (match (x, y) with
      | (True, False) | (False, True) | (True, True) -> true, true
      | (False, False) -> false, true
      | (_, _) -> 
        let a = bool_loop x in
        let b = bool_loop y in
        if snd a && snd b then
          fst a || fst b, true
        else false, false)

let rec num_loop e =
  match e with
  | Or (_, _) | True | False -> false
  | Num _ -> true
  | Add (x, y) -> num_loop x && num_loop y
  | IfThenElse (x, y, z) ->
    let a, b = bool_loop x in
    if b then
      if a
        then num_loop y
      else
        num_loop z
    else false
(* let type_of e = 
  match e with
  | _ -> None *)

let type_of e =
  let rec primitive_loop e =
    match e with
    | True -> Some Bool
    | False -> Some Bool
    | Num _ -> Some Int
    | Or (x, y) -> 
      (match (x, y) with
      | (True, True) | (False, True) | (True, False) | (False, False) -> Some Bool
      |(_, _) -> None)
    | Add (x, y) ->
      if num_loop x && num_loop y then Some Int
      else None
    | IfThenElse (x, y, z) ->
      let a, b = bool_loop x in
      if b then
        if a
          then primitive_loop y
        else
          primitive_loop z
      else None
    in
  primitive_loop e
 