module ListSet = struct
  type t = int list

  let empty = []

  let singleton x = [x]

  let rec mem x s =
    match s with
    | [] -> false
    | head :: tail ->
      if x = head then true
      else if x < head then false
      else mem x tail

  let rec card s =
    match s with
    | [] -> 0
    | _ :: tail -> 1 + card tail

  let rec union a b =
    match (a, b) with
    | [], x | x, [] -> x
    | h1 :: t1, h2 :: t2 ->
      if h1 = h2 then h1 :: union t1 t2
      else if h1 < h2 then h1 :: union t1 b
      else h2 :: union a t2
end

type set_info = {
  ind : int -> bool;
  mn : int;
  mx : int;
}

module FuncSet = struct
  type t = set_info

  let empty = {ind = (fun _ -> false); mn = 1; mx = 0}

  let singleton x = {ind = (fun y -> y = x); mn = x; mx = x}

  let mem x s = s.ind x

  let card s =
    let rec count c x =
      if x > s.mx then c
      else if s.ind x then count (c + 1) (x + 1)
      else count c (x + 1)
    in
    count 0 s.mn

  let union a b =
    {
      ind = (fun x -> a.ind x || b.ind x);
      mn = min a.mn b.mn;
      mx = max a.mx b.mx
    } 
end
