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
  | True -> true
  | False -> true
  | IfThenElse (x, y, z) -> 
    let b = bool_loop x in
    if b then 
      let c = bool_loop y in
        let d = bool_loop z in
        if c = d then c
        else false
    else false
  | Or (x, y) -> 
    let a = bool_loop x in
    let b = bool_loop y in
    if a && b then
      true
    else false
  | _ -> false

let rec num_loop e =
  match e with
  | Num _ -> true
  | Add (x, y) -> num_loop x && num_loop y
  | IfThenElse (x, y, z) ->
    let b = bool_loop x in
    if b then
      let c = num_loop y in
      let d = num_loop z in
      if c = d then c
      else false
    else false
  | _ -> false

let type_of e =
  let rec primitive_loop e =
    match e with
    | True -> Some Bool
    | False -> Some Bool
    | Num _ -> Some Int
    | Or (x, y) -> 
      let a = bool_loop x in
      let b = bool_loop y in
      if a && b then Some Bool
      else None
    | Add (x, y) ->
      if num_loop x && num_loop y then Some Int
      else None
    | IfThenElse (x, y, z) ->
      let b = bool_loop x in
      if b then
        let c = primitive_loop y in
        let d = primitive_loop z in
        if c = d then c
        else None
      else None
    in
  primitive_loop e
 