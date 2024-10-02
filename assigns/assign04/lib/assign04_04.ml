type ident = string

type expr' = 
| True
| False
| Num of int
| Var of ident
| Let of ident * expr' * expr'
| Add of expr' * expr'
| Or of expr' * expr'
| IfThenElse of expr' * expr' * expr'

type ty' = 
| Int
| Bool

type context = (ident * ty') list

let rec gamma_loop gamma e =
  match gamma with
  | [] -> Bool, false
  | head :: tail ->
    (match head with
    | (x, y) -> 
      if x = e then y, true
      else gamma_loop tail e)
and bool_loop gamma e =
  match e with
  | True -> true
  | False -> true
  | IfThenElse (x, y, z) -> 
    let b = bool_loop gamma x in
    if b then 
      let c = bool_loop gamma y in
        let d = bool_loop gamma z in
        if c = d then c
        else false
    else false
  | Or (x, y) -> 
    let a = bool_loop gamma x in
    let b = bool_loop gamma y in
    if a && b then true
    else false
  | Var s ->
    let (a, b) = gamma_loop gamma s in
    if b then 
      if a = Bool then true
      else false
    else false
  | Let (s, x, y) ->
    let a = primitive_loop gamma x in
    (match a with 
    | None -> false
    | Some Int -> bool_loop ((s, Int) :: gamma) y
    | Some Bool -> bool_loop ((s, Bool) :: gamma) y)
  | _ -> false
and num_loop gamma e =
  match e with
  | Num _ -> true
  | Add (x, y) -> num_loop gamma x && num_loop gamma y
  | IfThenElse (x, y, z) ->
    let b = bool_loop gamma x in
    if b then
      let c = num_loop gamma y in
      let d = num_loop gamma z in
      if c = d then c
      else false
    else false
  | Var s ->
    let (a, b) = gamma_loop gamma s in
    if b then 
      if a = Int then true
      else false
    else false
  | Let (s, x, y) ->
    let a = primitive_loop gamma x in
    (match a with 
    | None -> false
    | Some Int -> num_loop ((s, Int) :: gamma) y
    | Some Bool -> num_loop ((s, Bool) :: gamma) y)
  | _ -> false
and primitive_loop gamma e =
  match e with
  | True -> Some Bool
  | False -> Some Bool
  | Num _ -> Some Int
  | Or (x, y) -> 
    let a = bool_loop gamma x in
    let b = bool_loop gamma y in
    if a && b then Some Bool
    else None
  | Add (x, y) ->
    if num_loop gamma x && num_loop gamma y then Some Int
    else None
  | IfThenElse (x, y, z) ->
    let b = bool_loop gamma x in
    if b then
      let c = primitive_loop gamma y in
      let d = primitive_loop gamma z in
      if c = d then c
      else None
    else None
  | Var s ->
    let (a, b) = gamma_loop gamma s in
    if b then 
      Some a
    else None
  | Let (s, x, y) ->
    if num_loop gamma x then primitive_loop ((s, Int) :: gamma) y
    else if bool_loop gamma x then primitive_loop ((s, Bool) :: gamma) y
    else None

      
let type_of' gamma e = 
  (* match gamma, e with
  | _, _ -> None *)
  primitive_loop gamma e
  
  
      
