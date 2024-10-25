open Utils


type ty =
 | TInt
 | TBool


let rec type_of (e: expr) : ty option =
  match e with
  | Num _ -> Some TInt  
  
  | Add (e1, e2) ->  
    (match type_of e1, type_of e2 with
    | Some TInt, Some TInt -> Some TInt  
    | _ -> None)  

  | Lt (e1, e2) ->  
    (match type_of e1, type_of e2 with
    | Some TInt, Some TInt -> Some TBool  
    | _ -> None)  

  | Ite (e_cond, e_then, e_else) ->  
    (match type_of e_cond with
    | Some TBool ->  
      (match type_of e_then, type_of e_else with
      | Some t1, Some t2 when t1 = t2 -> Some t1 
      | _ -> None)  
    | _ -> None)  