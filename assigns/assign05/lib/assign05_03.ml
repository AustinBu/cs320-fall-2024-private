type ident = string

type ty = 
  | Unit
  | Arr of ty * ty

type expr = 
  | Var of ident
  | Fun of ident * ty * expr
  | App of expr * expr

type ctxt = (ident * ty) list

let rec loop gamma x =
  match gamma with
  | [] -> None
  | (y, t) :: rest -> if x = y then Some t else loop rest x

let rec type_of gamma e =
  match e with
  | Var x ->
      loop gamma x
  | Fun (x, y, e) ->
      let gamma' = (x, y) :: gamma in
      (match type_of gamma' e with
      | Some a -> Some (Arr (y, a))
      | None -> None)
  | App (e1, e2) ->
      (match type_of gamma e1, type_of gamma e2 with
      | Some (Arr (a, t)), Some b when a = b -> Some t
      | _ -> None)
