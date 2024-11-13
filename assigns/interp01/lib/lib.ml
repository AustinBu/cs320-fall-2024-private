open Utils

type env = (string * value) list

let eval_value (v : value) : expr =
  match v with
  | VNum n -> Num n
  | VBool true -> True
  | VBool false -> False
  | VUnit -> Unit
  | VFun (x, body) -> Fun (x, body)

let rec rn old neu expr =
  match expr with
  | Var y -> if y = old then Var neu else expr
  | App (e1, e2) -> App (rn old neu e1, rn old neu e2)
  | Bop (b, e1, e2) -> Bop (b, rn old neu e1, rn old neu e2)
  | If (cond, bthen, belse) ->
      If (rn old neu cond, rn old neu bthen, rn old neu belse)
  | Let (y, e1, e2) ->
      if y = old then Let (y, rn old neu e1, e2)
      else Let (y, rn old neu e1, rn old neu e2)
  | Fun (y, body) ->
      if y = old then Fun (y, body)
      else Fun (y, rn old neu body)
  | _ -> expr

let subst (v : value) x e : expr = 
  let rec loop v x e (env : string list) = 
    match e with
    | Var y -> if y = x then eval_value v else e
    | App (e1, e2) -> App (loop v x e1 env, loop v x e2 env)
    | Bop (op, e1, e2) -> Bop (op, loop v x e1 env, loop v x e2 env)
    | If (cond, e1, e2) -> If (loop v x cond env, loop v x e1 env, loop v x e2 env)
    | Let (y, e1, e2) -> 
        if y = x then Let (y, loop v x e1 env, e2) 
        else if List.mem y env then let y' = gensym () in Let (y', loop v x e1 env, loop v x (rn y y' e2) (y' :: env)) 
        else Let (y, loop v x e1 env, loop v x e2 (y :: env))
    | Fun (y, body) -> 
        if y = x then e 
        else if List.mem y env then let y' = gensym () in Fun (y', loop v x (rn y y' body) (y' :: env)) 
        else Fun (y, loop v x body (y ::env)) 
    | _ -> e
  in loop v x e []

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
  | And, VBool x, VBool y -> 
    (match x with 
    | false -> Ok (VBool false)
    | _ -> Ok (VBool (x && y)))
  | And, VBool x, _ -> 
      (match x with 
      | false -> Ok (VBool false)
      | _ -> Error (InvalidArgs op))
  | Or, VBool x, VBool y -> 
    (match x with 
    | true -> Ok (VBool true)
    | _ -> Ok (VBool (x || y)))
  | Or, VBool x, _ -> 
    (match x with 
    | true -> Ok (VBool true)
    | _ -> Error (InvalidArgs op))
  | _ -> Error (InvalidArgs op)

let eval e =
  let rec loop e env depth =
    if depth > 1000 then Error InvalidApp else
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
        (match loop e1 env (depth + 1) with
        | Ok v1 -> loop (subst v1 x e2) ((x, v1) :: env) (depth + 1)
        | Error err -> Error err)
    | Fun (x, body) -> Ok (VFun (x, body))
    | App (e1, e2) ->
        (match loop e1 env (depth + 1) with
        | Ok (VFun (x, body)) ->
            (match loop e2 env (depth + 1) with
              | Ok v2 -> loop (subst (v2) x body) env (depth + 1)
              | Error err -> Error err)
        | Ok _ -> Error InvalidApp
        | Error err -> Error err)
    | Bop (op, e1, e2) -> 
        (match loop e1 env (depth + 1) with
        | Ok v1 -> 
            (match loop e2 env (depth + 1) with
              | Ok v2 -> eval_bop op v1 v2
              | Error err -> Error err)
        | Error err -> Error err)
    | If (cond, e1, e2) ->
        (match loop cond env (depth + 1) with
        | Ok (VBool true) -> loop e1 env (depth + 1)
        | Ok (VBool false) -> loop e2 env (depth + 1)
        | Ok _ -> Error InvalidIfCond
        | Error err -> Error err)
in loop e [] 0

let parse s = My_parser.parse s

let interp s =
  match (parse s) with
  | None -> Error ParseFail
  | Some p -> eval p