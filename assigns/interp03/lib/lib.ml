open Utils
include My_parser

exception AssertFail
exception DivByZero
exception RecWithoutArg
exception CompareFunVals

let eval_expr _ _ = assert false

let ty_subst t x =
  let rec go = function
    | TUnit -> TUnit
    | TInt -> TInt
    | TFloat -> TFloat
    | TBool -> TBool
    | TVar y -> if x = y then t else TVar y
    | TList ty -> TList (go ty)
    | TOption ty -> TOption (go ty)
    | TPair (ty1, ty2) -> TPair (go ty1, go ty2)
    | TFun (ty1, ty2) -> TFun (go ty1, go ty2)
  in go
  
let ty_subst_c t x (t1, t2) = (ty_subst t x t1, ty_subst t x t2)
let ty_subst_cs t x = List.map (ty_subst_c t x)
  
let rec fvs = function
  | TUnit -> VarSet.empty
  | TInt -> VarSet.empty
  | TFloat -> VarSet.empty
  | TBool -> VarSet.empty
  | TVar x -> VarSet.of_list [x]
  | TList ty -> fvs ty
  | TOption ty -> fvs ty
  | TPair (ty1, ty2) -> VarSet.union (fvs ty1) (fvs ty2)
  | TFun (ty1, ty2) -> VarSet.union (fvs ty1) (fvs ty2)

let unify ty constraints : ty_scheme option =
  let rec go constraints = 
    match constraints with
    | [] -> None
    | [TVar "$_out", t] -> Some t (* optimization to not build a full solution *)
    | (TPair (t1, t2), TPair (t1', t2')) :: cs ->
      go ((t1, t1') :: (t2, t2') :: cs)
    | (TList t1, TList t2) :: cs | (TOption t1, TOption t2) :: cs->
      go ((t1, t2) :: cs)
    | (t1, t2) :: cs when t1 = t2 -> go cs
    | (TFun (t1, t2), TFun (t1', t2')) :: cs ->
        go ((t1, t1') :: (t2, t2') :: cs)
    | (TVar x, t) :: cs ->
        if VarSet.mem x (fvs t)
        then None
        else go (ty_subst_cs t x cs)
    | (t, TVar x) :: cs -> go ((TVar x, t) :: cs)
    | _ -> None
  in 
  let tys = go (constraints @ [TVar "$_out", ty]) in
  match tys with
  | None -> None
  | Some t' ->   
    let s = VarSet.to_list (fvs t') in
    Some (Forall (s, t'))

let match_type type1 type2 type3 type4 =
  if (type1 = type2) then
  let ot = (match type4 with
    | Some t -> t
    | None -> type2) in
  (match type3 with
    | Some t -> 
      if (t = type1) then ot
      else failwith "match_type"
    | None -> ot)
  else failwith "match_type"

let type_of' ctxt expr =
  let rec loop ( ctxt : stc_env ) expr =
    match expr with
    | Unit -> TUnit, []
    | True | False -> TBool, []
    | Int _ -> TInt, []
    | Float _ -> TFloat, []
    | Var x ->
      let bnd_vars, t = 
      (match Env.find x ctxt with
      | Forall (vars, t) -> vars, t) in
      let rec instantiate bnd_vars t =
        match bnd_vars with
        | [] -> t
        | x :: bnd_vars ->
          let b = TVar (gensym ()) in
          instantiate bnd_vars (ty_subst b x t)
      in
      instantiate bnd_vars t, []
    | ENone -> TOption (TVar (gensym ())), []
    | ESome e -> let t, c = loop ctxt e in 
      TOption (t), c
    | Nil -> TList (TVar (gensym ())), []
    | Bop (op, e1, e2) -> 
      let t1, c1 = loop ctxt e1 in
      let t2, c2 = loop ctxt e2 in
      (match op with
      | Cons ->  (match_type (TList t1) t2 None None, (t2, TList t1) :: c1 @ c2)
      | Add | Sub | Mul | Div | Mod -> (match_type t1 t2 (Some TInt) None, [(t1, TInt); (t2, TInt)] @ c1 @ c2)
      | AddF | SubF | MulF | DivF | PowF ->  (match_type t1 t2 (Some TFloat) None, [(t1, TFloat); (t2, TFloat)] @ c1 @ c2)
      | Lt | Lte | Gt | Gte | Eq | Neq ->  (match_type t1 t2 None (Some TBool), (t1, t2) :: c1 @ c2)
      | And | Or ->  (match_type t1 t2 (Some TBool) None, [(t1, TBool); (t2, TBool)] @ c1 @ c2)
      | Concat -> 
        let alpha = TList (TVar (gensym ())) in 
        (alpha, [(t1, alpha); (t2, alpha)] @ c1 @ c2)
      | Comma -> (TPair (t1, t2), c1 @ c2))
    | If (e1, e2, e3) -> 
      let t1, c1 = loop ctxt e1 in
      let t2, c2 = loop ctxt e2 in
      let t3, c3 = loop ctxt e3 in
      (match_type t2 t3 None None, [(t1, TBool); (t2, t3)] @ c1 @ c2 @ c3)
    | Assert False -> TVar (gensym ()), []
    | Assert e -> 
      let t, c = loop ctxt e in
      TUnit, (t, TBool) :: c
    | Annot (e, ty) -> 
      let t, c = loop ctxt e in
      ty, (ty, t) :: c
    | OptMatch { matched; some_name; some_case; none_case } -> 
      let tm, cm = loop ctxt matched in
      let alpha = TVar (gensym ()) in
      let ctxt_cons = Env.add some_name (Forall ([], alpha)) ctxt in
      let ts, cs = loop ctxt_cons some_case in
      let tn, cn = loop ctxt none_case in
      (tn, [(tm, TOption alpha); (ts, tn)] @ cm @ cs @ cn)
    | ListMatch { matched; hd_name; tl_name; cons_case; nil_case } -> 
      let tm, cm = loop ctxt matched in
      let alpha = TVar (gensym ()) in
      let ctxt_cons = Env.add hd_name (Forall ([], alpha)) (Env.add tl_name (Forall ([], TList alpha)) ctxt) in
      let tc, cc = loop ctxt_cons cons_case in
      let tn, cn = loop ctxt nil_case in
      (tn, [(tm, TList alpha); (tc, tn)] @ cm @ cc @ cn)
    | PairMatch { matched; fst_name; snd_name; case } ->
      let tm, cm = loop ctxt matched in
      let alpha = TVar (gensym ()) in
      let beta = TVar (gensym ()) in
      let ctxt_cons = Env.add fst_name (Forall ([], alpha)) (Env.add snd_name (Forall ([], beta)) ctxt) in
      let tc, cc = loop ctxt_cons case in
      (tc, (tm, TPair (alpha, beta)) :: cm @ cc)
    | Fun (x, ty_opt, e) ->
      let x_ty = match ty_opt with Some t -> t | None -> TVar (gensym ()) in
      let ctxt_body = Env.add x (Forall ([], x_ty)) ctxt in
      let t, c =  loop ctxt_body e in
      (TFun (x_ty, t), c)
    | App (e1, e2) ->
      let t1, c1 = loop ctxt e1 in
      let t2, c2 = loop ctxt e2 in
      let alpha = TVar (gensym ()) in
      (alpha, (t1, TFun (t2, alpha)) :: c1 @ c2)
    | Let { is_rec; name; value; body } ->
      if is_rec then
        let alpha = TVar (gensym ()) in
        let beta = TVar (gensym ()) in
        let ctxt' = Env.add name (Forall ([], TFun (alpha, beta))) ctxt in
        let tv, cv = loop ctxt' value in
        let ctxt'' = Env.add name (Forall ([], tv)) ctxt in
        let tb, cb = loop ctxt'' body in
        (tb, (tv, TFun (alpha, beta)) :: cv @ cb)
      else 
        let tv, cv = loop ctxt value in
        let tb, cb = loop (Env.add name (Forall ([], tv)) ctxt) body in
        (tb, cv @ cb)
  in loop ctxt expr

let type_of ( ctxt : stc_env ) expr =
  let t, c = type_of' ctxt expr in (* constraint-based inference *)
  unify t c (* unification *)
  
let type_check =
  let rec go ctxt = function
  | [] -> Some (Forall ([], TUnit))
  | {is_rec;name;value} :: ls ->
    match type_of ctxt (Let {is_rec;name;value; body = Var name}) with
    | Some ty -> (
      match ls with
      | [] -> Some ty
      | _ ->
        let ctxt = Env.add name ty ctxt in
        go ctxt ls
    )
    | None -> None
  in go Env.empty

let eval p =
  let rec nest = function
    | [] -> Unit
    | [{is_rec;name;value}] -> Let {is_rec;name;value;body = Var name}
    | {is_rec;name;value} :: ls -> Let {is_rec;name;value;body = nest ls}
  in eval_expr Env.empty (nest p)

  let interp input =
  match parse input with
  | Some prog -> (
    match type_check prog with
    | Some ty -> Ok (eval prog, ty)
    | None -> Error TypeError
  )
  | None -> Error ParseError
