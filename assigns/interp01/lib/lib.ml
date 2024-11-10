open Utils

type env = (string * value) list

let eval_value (v : value) : expr =
  match v with
  | VNum n -> Num n
  | VBool true -> True
  | VBool false -> False
  | VUnit -> Unit
  | VFun (x, body) -> Fun (x, body)

let rec subst v x e = 
  match e with
  | Var y -> if y = x then v else e
  | App (e1, e2) -> App (subst v x e1, subst v x e2)
  | Bop (op, e1, e2) -> Bop (op, subst v x e1, subst v x e2)
  | If (cond, e1, e2) -> If (subst v x cond, subst v x e1, subst v x e2)
  | Let (y, e1, e2) -> 
      if y = x then Let (y, subst v x e1, e2) 
      else Let (y, subst v x e1, subst v x e2)
  | Fun (y, body) -> 
      if y = x then e 
      else Fun (y, subst v x body)
  | _ -> e

let eval_bop op v1 v2 =
  match op, v1, v2 with
  | Add, VNum x, VNum y -> Ok (VNum (x + y))
  | Sub, VNum x, VNum y -> Ok (VNum (x - y))
  | Mul, VNum x, VNum y -> Ok (VNum (x * y))
  | Div, VNum x, VNum y -> if y = 0 then Error DivByZero else Ok (VNum (x / y))
  | Mod, VNum x, VNum y -> if y = 0 then Error DivByZero else Ok (VNum (x mod y))
  | Lt, VNum x, VNum y -> Ok (VBool (x < y))
  | Lte, VNum x, VNum y -> Ok (VBool (x <= y))
  | Gt, VNum x, VNum y -> Ok (VBool (x > y))
  | Gte, VNum x, VNum y -> Ok (VBool (x >= y))
  | Eq, VNum x, VNum y -> Ok (VBool (x = y))
  | Neq, VNum x, VNum y -> Ok (VBool (x <> y))
  | And, VBool x, VBool y -> Ok (VBool (x && y))
  | Or, VBool x, VBool y -> Ok (VBool (x || y))
  | _ -> Error (InvalidArgs op)

let rec eval (e : expr) (env : env) : (value, error) result =
  match e with
  | Num n -> Ok (VNum n)
  | True -> Ok (VBool true)
  | False -> Ok (VBool false)
  | Unit -> Ok VUnit
  | Var x -> 
      (match List.assoc_opt x env with
       | Some v -> Ok v
       | None -> Error (UnknownVar x))
  | Let (x, e1, e2) -> 
      (match eval e1 env with
       | Ok v1 -> eval e2 ((x, v1) :: env)
       | Error err -> Error err)
  | Fun (x, body) -> Ok (VFun (x, body))
  | App (e1, e2) ->
      (match eval e1 env with
       | Ok (VFun (x, body)) ->
           (match eval e2 env with
            | Ok v2 -> eval (subst (eval_value v2) x body) env
            | Error err -> Error err)
       | Ok _ -> Error InvalidApp
       | Error err -> Error err)
  | Bop (op, e1, e2) -> 
      (match eval e1 env with
       | Ok v1 -> 
           (match eval e2 env with
            | Ok v2 -> eval_bop op v1 v2
            | Error err -> Error err)
       | Error err -> Error err)
  | If (cond, e1, e2) ->
      (match eval cond env with
       | Ok (VBool true) -> eval e1 env
       | Ok (VBool false) -> eval e2 env
       | Ok _ -> Error InvalidIfCond
       | Error err -> Error err)

let interp s =
  match (My_parser.parse s) with
  | None -> Error ParseFail
  | Some p -> eval p []