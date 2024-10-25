open Utils


let parse (tokens: tok list) : expr option =
let apply_bin_op op stack =
  match stack with
  | e2 :: e1 :: rest -> Some (op (e1, e2) :: rest)
  | _ -> None  
in
let apply_ite_op stack =
  match stack with
  | e3 :: e2 :: e1 :: rest -> Some (Ite (e1, e2, e3) :: rest)
  | _ -> None  
in
let rec parse_tokens stack = function
  | [] ->  
    (match stack with
    | [e] -> Some e
    | _ -> None)
  | TNum n :: rest -> parse_tokens (Num n :: stack) rest  
  | TAdd :: rest ->  
    (match apply_bin_op (fun (e1, e2) -> Add (e1, e2)) stack with
    | Some new_stack -> parse_tokens new_stack rest
    | None -> None)
  | TLt :: rest ->  
    (match apply_bin_op (fun (e1, e2) -> Lt (e1, e2)) stack with
    | Some new_stack -> parse_tokens new_stack rest
    | None -> None)
  | TIte :: rest ->  
    (match apply_ite_op stack with
    | Some new_stack -> parse_tokens new_stack rest
    | None -> None)
in
parse_tokens [] tokens  