open Utils

let rec eval (e : expr) : value =
  match e with
  | Num n -> VNum n
  | Add (x, y) ->
    (match eval x, eval y with
    | VNum a, VNum b -> VNum (a + b)
    | _ -> VNum 0)
  | Lt (x, y) ->
    (match eval x, eval y with
    | VNum a, VNum b -> VBool (a < b)
    | _ -> VBool false)
  | Ite (x, y, z) ->
    (match eval x with
    | VBool a -> if a then eval y else eval z
    | _ -> VNum 0)