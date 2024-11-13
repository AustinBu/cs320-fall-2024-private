open Utils

type env = (string * value) list

let eval_value (v : value) : expr =
  match v with
  | VNum n -> Num n
  | VBool true -> True
  | VBool false -> False
  | VUnit -> Unit
  | VFun (x, body) -> Fun (x, body)

let rec fv expr =
  match expr with
  | Var x -> [x]
  | App (e1, e2) | Bop (_, e1, e2) -> 
      (fv e1) @ (fv e2)
  | If (cond, e1, e2) -> 
      (fv cond) @ (fv e1) @ (fv e2)
  | Let (x, e1, e2) -> 
      let free_e1 = fv e1 in
      let free_e2 = fv e2 in
      List.filter (fun v -> v <> x) (free_e1 @ free_e2)
  | Fun (x, body) -> 
      List.filter (fun v -> v <> x) (fv body)
  | _ -> []

let rec find_binding y expr =
  match expr with
  | Var x -> if x = y then Some (Var x) else None
  | App (e1, e2) | Bop (_, e1, e2) -> 
      (match find_binding y e1 with
      | Some _ as result -> result
      | None -> find_binding y e2)
  | If (cond, e1, e2) -> 
      (match find_binding y cond with
      | Some _ as result -> result
      | None -> 
          (match find_binding y e1 with
          | Some _ as result -> result
          | None -> find_binding y e2))
  | Let (x, e1, e2) -> 
      if x = y then Some (Var x)
      else
        let binding_in_e1 = find_binding y e1 in
        (match binding_in_e1 with
        | Some _ as result -> result
        | None -> find_binding y e2)
  | Fun (x, body) -> 
      if x = y then Some (Fun (x, body))
      else find_binding y body
  | _ -> None

let rec subst (v : value) x e : expr = 
    match e with
    | Var y -> if y = x then eval_value v else e
    | App (e1, e2) -> App (subst v x e1, subst v x e2)
    | Bop (op, e1, e2) -> Bop (op, subst v x e1, subst v x e2)
    | If (cond, e1, e2) -> If (subst v x cond, subst v x e1, subst v x e2)
    | Let (y, e1, e2) -> 
        if y = x then Let (y, subst v x e1, e2) 
        else if List.mem y (fv (eval_value v)) then
          (match find_binding y e2 with 
          | Some b -> let y' = gensym () in Let (y', subst v x e1, subst v x (subst (VFun (y, b)) y e2)) 
          | None -> Let (y, subst v x e1, subst v x e2))
        else Let (y, subst v x e1, subst v x e2)
    | Fun (y, body) -> 
        if y = x then e
        else if List.mem y (fv (eval_value v)) then
          (match find_binding y body with
          | Some b -> let y' = gensym () in Fun (y', subst v x (subst (VFun (y, b)) y body))
          | None -> Fun (y, subst v x body))
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