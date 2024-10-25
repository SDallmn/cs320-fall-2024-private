open Utils


type value =
 | VNum of int
 | VBool of bool



let rec eval (e: expr) : value =
  match e with
  | Num n -> VNum n  

  | Add (e1, e2) ->  
    (match eval e1, eval e2 with
      | VNum v1, VNum v2 -> VNum (v1 + v2)
      | _ -> failwith "Type error: expected two numbers for Add")

  | Lt (e1, e2) ->  
    (match eval e1, eval e2 with
      | VNum v1, VNum v2 -> VBool (v1 < v2)
      | _ -> failwith "Type error: expected two numbers for Lt")

  | Ite (e_cond, e_then, e_else) ->  
    (match eval e_cond with
      | VBool cond -> if cond then eval e_then else eval e_else
      | _ -> failwith "Type error: expected a boolean for the condition in Ite")