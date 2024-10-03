
type expr = 
| True
| False
| Num of int
| Or of expr * expr
| Add of expr * expr
| IfThenElse of expr * expr * expr

type ty = 
| Int
| Bool


let rec type_of expr = 
  match expr with
  | True -> Some Bool        
  | False -> Some Bool      
  | Num _ -> Some Int      
  | Or(e1, e2) ->              
    (match type_of e1, type_of e2 with
     | Some Bool, Some Bool -> Some Bool
     | _ -> None)
  | Add(e1, e2) ->           
    (match type_of e1, type_of e2 with
     | Some Int, Some Int -> Some Int
     | _ -> None)
  | IfThenElse(condition, e_then, e_else) ->  
    (match type_of condition, type_of e_then, type_of e_else with
     | Some Bool, Some ty_then, Some ty_else when ty_then = ty_else -> Some ty_then
     | _ -> None)