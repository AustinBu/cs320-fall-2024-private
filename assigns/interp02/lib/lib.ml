open Utils

exception AssertFail
exception DivByZero

let rec lookup env x =
  match env with
  | [] -> failwith ("Unbound variable: " ^ x)
  | (y, v) :: rest -> if x = y then v else lookup rest x

let extend env x v = (x, v) :: env

let eval_bop op v1 v2 =
  match (op, v1, v2) with
  | (Add, VNum n1, VNum n2) -> VNum (n1 + n2)
  | (Sub, VNum n1, VNum n2) -> VNum (n1 - n2)
  | (Mul, VNum n1, VNum n2) -> VNum (n1 * n2)
  | (Div, VNum _, VNum 0) -> raise DivByZero
  | (Div, VNum n1, VNum n2) -> VNum (n1 / n2)
  | (Mod, VNum _, VNum 0) -> raise DivByZero
  | (Mod, VNum n1, VNum n2) -> VNum (n1 mod n2)
  | (Lt, VNum n1, VNum n2) -> VBool (n1 < n2)
  | (Lte, VNum n1, VNum n2) -> VBool (n1 <= n2)
  | (Gt, VNum n1, VNum n2) -> VBool (n1 > n2)
  | (Gte, VNum n1, VNum n2) -> VBool (n1 >= n2)
  | (Eq, VNum n1, VNum n2) -> VBool (n1 = n2)
  | (Neq, VNum n1, VNum n2) -> VBool (n1 <> n2)
  | (And, VBool b1, VBool b2) -> VBool (b1 && b2)
  | (Or, VBool b1, VBool b2) -> VBool (b1 || b2)
  | _ -> failwith "Invalid binary operation"

let eval expr =
  let rec loop env expr =
    match expr with
    | Unit -> VUnit
    | True -> VBool true
    | False -> VBool false
    | Num n -> VNum n
    | Var x -> lookup env x
    | Bop (op, e1, e2) ->
        let v1 = loop env e1 in
        let v2 = loop env e2 in
        eval_bop op v1 v2
    | If (cond, e1, e2) -> (
        match loop env cond with
        | VBool true -> loop env e1
        | VBool false -> loop env e2
        | _ -> failwith "Condition must loopuate to a Boolean")
    (* | Fun (arg, _, body) -> VClos { name = None; arg; body; env = env } *)
    (* | App (f, arg) -> (
        match loop env f with
        | VClos { name = _; arg = param; body; env = closure_env } ->
            let arg_val = loop env arg in
            let env' = extend closure_env param arg_val in
            loop env' body
        | _ -> failwith "Application of a non-function") *)
    | Let { is_rec; name; ty; value; body } ->
        let rec_env =
          if is_rec then
            extend env name (loop env (Let { is_rec = false; name; ty; value; body = value }))
          else env
        in
        let value_val = loop rec_env value in
        loop (extend env name value_val) body
    | Assert e -> (
        match loop env e with
        | VBool true -> VUnit
        | VBool false -> raise AssertFail
        | _ -> failwith "Assertion must loopuate to a Boolean")
    | _ -> VUnit
  in loop [] expr


let type_of (e : expr) : (ty, error) result =
  let rec loop ctx e =
  match e with
  | Unit -> Ok UnitTy
  | True | False -> Ok BoolTy
  | Num _ -> Ok IntTy
  | Var x -> (
      match List.assoc_opt x ctx with
      | Some ty -> Ok ty
      | None -> Error (UnknownVar x))
  | Bop (op, e1, e2) -> (
      let type_op = function
        | Add | Sub | Mul | Div | Mod -> (IntTy, IntTy)
        | Lt | Lte | Gt | Gte -> (IntTy, BoolTy)
        | Eq | Neq -> (IntTy, BoolTy)
        | And | Or -> (BoolTy, BoolTy)
      in
      let (expected_arg, result_ty) = type_op op in
      match loop ctx e1 with
      | Error _ as err -> err
      | Ok t1 ->
          if t1 <> expected_arg then
            Error (OpTyErrL (op, expected_arg, t1))
          else
            match loop ctx e2 with
            | Error _ as err -> err
            | Ok t2 ->
                if t2 <> expected_arg then
                  Error (OpTyErrR (op, expected_arg, t2))
                else Ok result_ty)
  | If (cond, e1, e2) -> (
      match loop ctx cond with
      | Ok BoolTy -> (
          match loop ctx e1, loop ctx e2 with
          | Ok ty1, Ok ty2 ->
              if ty1 = ty2 then Ok ty1
              else Error (IfTyErr (ty1, ty2))
          | Error _ as err, _ -> err
          | _, (Error _ as err) -> err)
      | Ok ty -> Error (IfCondTyErr ty)
      | Error _ as err -> err)
  | Fun (arg, arg_ty, body) ->
      let ctx' = (arg, arg_ty) :: ctx in
      (match loop ctx' body with
      | Ok body_ty -> Ok (FunTy (arg_ty, body_ty))
      | Error _ as err -> err)
  | App (f, arg) -> (
      match loop ctx f with
      | Ok (FunTy (param_ty, ret_ty)) -> (
          match loop ctx arg with
          | Ok arg_ty ->
              if arg_ty = param_ty then Ok ret_ty
              else Error (FunArgTyErr (param_ty, arg_ty))
          | Error _ as err -> err)
      | Ok ty -> Error (FunAppTyErr ty)
      | Error _ as err -> err)
  | Let { is_rec; name; ty; value; body } ->
      let ctx' = if is_rec then (name, ty) :: ctx else ctx in
      let ctx'' = (name, ty) :: ctx in
      (match loop ctx' value with
      | Ok value_ty ->
          if value_ty = ty then loop ctx'' body
          else Error (LetTyErr (ty, value_ty))
      | Error _ as err -> err)
  | Assert e -> (
      match loop ctx e with
      | Ok BoolTy -> Ok UnitTy
      | Ok ty -> Error (AssertTyErr ty)
      | Error _ as err -> err)
  in loop [] e

  open Utils

let rec dexpr = function
  | SUnit -> Unit
  | STrue -> True
  | SFalse -> False
  | SNum n -> Num n
  | SVar x -> Var x
  | SFun { arg; args; body } ->
    let rec make_fun args body =
      match args with
      | [] -> body
      | (x, ty) :: xs -> Fun (x, ty, make_fun xs body)
    in
    Fun (fst arg, snd arg, make_fun args (dexpr body))
  | SApp (f, x) -> App (dexpr f, dexpr x)
  | SLet { is_rec; name; args; ty; value; body } ->
    let value_expr =
      match args with
      | [] -> dexpr value
      | _ ->
        let rec make_fun args body =
          match args with
          | [] -> body
          | (x, ty) :: xs -> Fun (x, ty, make_fun xs body)
        in
        make_fun args (dexpr value)
    in
    Let { is_rec; name; ty; value = value_expr; body = dexpr body }
  | SIf (cond, then_, else_) ->
    If (dexpr cond, dexpr then_, dexpr else_)
  | SBop (op, left, right) ->
    Bop (op, dexpr left, dexpr right)
  | SAssert e -> Assert (dexpr e)

let desugar prog =
  List.fold_right
    (fun toplet acc ->
      Let
        { is_rec = toplet.is_rec
        ; name = toplet.name
        ; ty = toplet.ty
        ; value = dexpr toplet.value
        ; body = acc
        })
    prog
    Unit

let parse s = My_parser.parse s

let interp s =
  match (parse s) with
  | None -> Error ParseErr
  | Some p -> Ok (eval (desugar p))