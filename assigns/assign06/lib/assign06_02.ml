open Utils

let rec pop_n stack n l =
  if not (n = 0) then
    match stack with
    | head :: tail -> pop_n tail (n - 1) (head :: l)
    | [] -> None
  else Some (l, stack)

let parse (t : tok list) : expr option =
  let rec loop toklist stack =
    match toklist with
    | [] -> 
      (match stack with
      | [e] -> Some e
      | _ -> None)
    | TNum n :: tail ->
      loop tail (Num n :: stack)
    | TAdd :: tail ->
      (match pop_n stack 2 [] with
      | Some (a, s) -> 
        (match a with
        | x :: y :: _ -> loop tail (Add (x, y) :: s)
        | _ -> None)
      | _ -> None)
    | TLt :: tail -> 
      (match pop_n stack 2 [] with
      | Some (a, s) -> 
        (match a with
        | x :: y :: _ -> loop tail (Lt (x, y) :: s)
        | _ -> None)
      | _ -> None)
    | TIte :: tail ->
      (match pop_n stack 3 [] with
      | Some (a, s) -> 
        (match a with
        | x :: y :: z :: _ -> loop tail (Ite (x, y, z) :: s)
        | _ -> None)
      | _ -> None)
  in loop t []

