type value = 
| VNum of int
| VBool of bool

let eval e = 
  match e with
  | _ ->  VNum 0