type ident = string

type ty = 
  | Unit
  | Arr of ty * ty

type expr = 
  | Var of ident
  | Fun of ident * ty * expr
  | App of expr * expr

type ctxt = (ident * ty) list

let rec type_of gamma e =
  let rec loop gamma x =
    match gamma with
    | [] -> None
    | head :: rest ->
      if x = (fst head) then Some (snd head)
      else loop rest x
    in
  match e with
  | Var x -> loop gamma x
  | Fun (a, b, c) ->
    let g = (a, b) :: gamma in
    (match type_of g e with
    | Some x -> Some (Arr (b, x))
    | None -> None)
  | App (a, b) ->
    (match type_of gamma a, type_of gamma b with
    | Some (Arr (x, y)), Some z -> 
      if x = z then Some y
      else None
    | _ -> None)
