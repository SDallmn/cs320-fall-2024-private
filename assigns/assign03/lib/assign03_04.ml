(* while  *)



let sign n =
  if n > 0 then 1
  else if n < 0 then -1
  else 0

let rec drop n lst =
  match lst with
  | [] -> []  (* If the list is empty, return an empty list *)
  | _ :: tl -> 
      if n <= 0 then lst  (* If n is 0 or negative, return the list as it is *)
      else drop (n - 1) tl  (* Recursively drop elements from the tail *)

(* Function to check if the list is valid *)

(* 
let is_valid_list lst =
  let rec check_sublists lst prev_sign =
    match lst with
    | [] -> true  (* End of the list *)
    | 0 :: tl -> check_sublists tl prev_sign  (* Skip zeros *)
    | hd :: tl when sign hd = 0 -> check_sublists tl prev_sign (* Skip if head is 0 *)
    | hd :: tl ->
        let current_sign = sign hd in
        if prev_sign = 0 || prev_sign <> current_sign then
          (* Extract the sublist of same-sign numbers *)
          let rec get_samesign_sublist lst curr_sign =
            match lst with
            | hd :: tl when sign hd = curr_sign -> hd :: get_samesign_sublist tl curr_sign
            | _ -> []
          in
          let sublist = get_samesign_sublist (hd :: tl) current_sign in
          let remaining = drop (List.length sublist) (hd :: tl) in
          check_sublists remaining current_sign
        else
          false  (* Signs didn't alternate *)
  in
  check_sublists lst 0  *)



let is_valid lst =
  let rec check_adjacent prev = function
    | [] -> true  (* End of the list, valid so far *)
    | 0 :: hd :: tl ->
        if sign hd = 0 then false  (* A zero must not be followed by another zero *)
        else if sign hd = prev then check_adjacent (sign hd)  tl  (* Opposite signs around zero *)
        else false  (* Not opposite signs *)
    | hd :: 0 :: tl ->
        if sign prev = 0 || sign hd = 0 then false  (* No non-zero integers adjacent to zero *)
        else if sign hd = prev then check_adjacent (sign hd)  tl  (* Same sign *)
        else false  (* Must be the same sign or zero *)
    | hd :: tl ->
        if sign prev = 0 then check_adjacent (sign hd)  tl  (* First element case *)
        else if sign hd = prev || sign hd = 0 then check_adjacent (sign hd)  tl
        else false  (* Must be the same sign or zero *)

  in
  match lst with
  | [] -> true  (* Empty list is valid *)
  | hd :: tl -> check_adjacent (sign hd)  tl;;

let split_by_zero lst =
  let rec aux current acc = function
    | [] -> 
        if current = [] then List.rev acc  (* If no current sublist, return accumulated result *)
        else List.rev (List.rev current :: acc)  (* Add the last sublist and return result *)
    | 0 :: tl -> 
        if current = [] then aux [] acc tl  (* Skip consecutive zeros *)
        else aux [] (List.rev current :: acc) tl  (* Add sublist to acc and start a new one *)
    | hd :: tl -> aux (hd :: current) acc tl  (* Add element to current sublist *)
  in
  aux [] [] lst  (* Initial empty current sublist and accumulator *)



let group l = 
  if is_valid l then
    Some (split_by_zero l)
  else
    None
