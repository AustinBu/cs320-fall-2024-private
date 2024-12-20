open Utils

exception AssertFail
exception DivByZero


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
  | (And, VBool false, _) -> VBool false
  | (And, VBool b1, VBool b2) -> VBool (b1 && b2)
  | (Or, VBool true, _) -> VBool true
  | (Or, VBool b1, VBool b2) -> VBool (b1 || b2)
  | _ -> failwith "3"

let eval expr =
  let rec loop env expr =
    match expr with
    | Unit -> VUnit
    | True -> VBool true
    | False -> VBool false
    | Num n -> VNum n
    | Var x -> 
      (* print_endline x;  *)
      Env.find x env
    | Bop (op, e1, e2) -> 
      (* print_endline "bop"; *)
        let v1 = loop env e1 in
        (try eval_bop op v1 VUnit
        with Failure _ ->
        let v2 = loop env e2 in
        eval_bop op v1 v2)
    | If (cond, e1, e2) -> 
      (* print_endline "if"; *)
      (match loop env cond with
        | VBool true -> loop env e1
        | VBool false -> loop env e2
        | _ -> failwith "4")
    | Fun (arg, _, body) -> 
      (* print_endline "fun"; *)
      VClos { name = None; arg; body; env = env}
    | App (f, arg) -> 
      (* print_endline "app"; *)
      (match loop env f with
        | VClos { name = None; arg = param; body; env = closure_env } ->
            let arg_val = loop env arg in
            let env' = Env.add param arg_val closure_env in
            loop env' body
        | VClos { name = Some fname; arg = param; body; env = closure_env } ->
          let arg_val = loop env arg in
          let env' = Env.add fname (VClos { name = Some fname; arg = param; body; env = closure_env }) closure_env in
          loop (Env.add param arg_val env') body
        | _ -> failwith "5")
    | Let { is_rec; name; ty=_; value; body } ->
      (* print_endline "let"; *)
      if is_rec then
        (match loop env value with
        | VClos { name=_; arg = param; body = nbody; env = closure_env } -> 
          let value_val = VClos {name=Some name; arg = param; body=nbody; env = closure_env} in
          loop (Env.add name value_val env) body
        | _ -> failwith "6")
      else
        let value_val = loop env value in
        loop (Env.add name value_val env) body
    | Assert e -> 
      (* print_endline "assert"; *)
      (match loop env e with
        | VBool true -> VUnit
        | _ -> raise AssertFail)
  in loop Env.empty expr


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

let rec dtype args ty =
  match args with
  | (_, ty') :: rest -> FunTy (ty', dtype rest ty)
  | [] -> ty

let rec dexpr sfexpr = 
  match sfexpr with
  | SUnit -> Unit
  | STrue -> True
  | SFalse -> False
  | SNum n -> Num n
  | SVar v -> Var v
  | SFun { arg = (x, ty); args = []; body } -> Fun (x, ty, dexpr body)
  | SFun { arg = (x, ty); args = (y, ty') :: args'; body } ->
      dexpr (SFun { arg = (x, ty); args = []; body = SFun { arg = (y, ty'); args = args'; body } })
  | SApp (f, arg) -> App (dexpr f, dexpr arg)
  | SLet { is_rec; name; args = []; ty; value; body } ->
      Let { is_rec; name; ty; value = dexpr value; body = dexpr body }
  | SLet { is_rec; name; args = (x, ty') :: args'; ty; value; body } ->
      dexpr
        (SLet
            { is_rec
            ; name
            ; args = []
            ; ty = dtype ((x, ty') :: args') ty
            ; value =
                SFun
                  { arg = (x, ty')
                  ; args = args'
                  ; body = value
                  }
            ; body
            })
  | SIf (cond, then_branch, else_branch) ->
      If (dexpr cond, dexpr then_branch, dexpr else_branch)
  | SBop (op, e1, e2) -> Bop (op, dexpr e1, dexpr e2)
  | SAssert e -> Assert (dexpr e)

let desugar prog =
  let rec dtoplet prog = 
    match prog with
    | [] -> Unit
    | { is_rec; name; args; ty; value } :: rest ->
      Let
      { is_rec
        ; name
        ; ty = dtype args ty
        ; value = 
        (match args with 
          | [] -> dexpr value
          | (x, ty') :: args' -> dexpr (SFun { arg = (x, ty'); args = args'; body = value })
        )
        ; body = dtoplet rest
      }
  in
  dtoplet prog

let parse s = My_parser.parse s

let interp s =
  match (parse s) with
  | None -> Error ParseErr
  | Some p -> 
    let newp = desugar p in
    match type_of newp with
    | Ok _ -> Ok (eval newp)
    | Error _ as err -> err

