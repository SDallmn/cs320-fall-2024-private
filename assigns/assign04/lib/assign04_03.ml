
open Assign04_02

type value = 
| VNum of int
| VBool of bool

exception InvalidExpression

let rec eval expr =
  match expr with
  | True -> VBool true
  | False -> VBool false
  | Num num -> VNum num
  | Or (e1, e2) ->
    (match eval e1, eval e2 with
    | VBool b1, VBool b2 -> VBool (b1 || b2)
    | _ -> failwith "incorrect evaulation")
  | Add (e1, e2) ->
    (match eval e1, eval e2 with
    | VNum n1, VNum n2 -> VNum (n1 + n2)
    | _ -> failwith "incorrect evaulation")
  | IfThenElse (condition, e_then, e_else) ->
    (match eval condition with
    | VBool true -> eval e_then
    | VBool false -> eval e_else
    | _ -> failwith "incorrect evaulation") 