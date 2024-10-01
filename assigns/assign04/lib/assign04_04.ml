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

let type_of' gamma e = 
  match gamma, e with
  | _ -> None
