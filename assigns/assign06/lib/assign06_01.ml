open Utils

let lex (s : string) : tok list option = 
  let w = split s in
  let rec loop words c =
    match words with
    | [] -> Some (List.rev c)
    | wo :: rds -> 
      (match tok_of_string_opt wo with
      | Some t -> loop rds (t :: c)
      | None -> None)
  in
  loop w []