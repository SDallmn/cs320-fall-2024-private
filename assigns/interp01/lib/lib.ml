
open Utils
include My_parser

let parse s = My_parser.parse s

let val_to_expr v = 
  match v with 
  | VNum n -> Num n 
  | VBool true -> True 
  | VBool false -> False 
  | VUnit -> Unit 
  | VFun (x, e) -> Fun (x, e)

let rec subst v x e =
  match e with
  | Num _ | True | False | Unit -> e
  | Var y -> if x = y then val_to_expr v else e
  | App (e1, e2) -> App (subst v x e1, subst v x e2)
  | Bop (op, e1, e2) -> Bop (op, subst v x e1, subst v x e2)
  | If (e1, e2, e3) -> If (subst v x e1, subst v x e2, subst v x e3)
  | Let (y, e1, e2) -> 
      if x = y then Let (y, subst v x e1, e2)
      else Let (y, subst v x e1, subst v x e2)
  | Fun (y, e) -> 
      if x = y then e 
      else Fun (y, subst v x e)

let rec eval e =
  match e with
  | Num n -> Ok (VNum n)
  | True -> Ok (VBool true)
  | False -> Ok (VBool false)
  | Unit -> Ok VUnit
  | Var x -> Error (UnknownVar x) 
  | App (e1, e2) -> 
      (match eval e1 with
        | Ok (VFun (x, e)) -> 
            (match eval e2 with
            | Ok v2 -> eval (subst (v2) x e)
            | Error _ as err -> err) 
        | Ok _ -> Error InvalidApp
        | Error _ as err -> err) 
  | Bop (op, e1, e2) -> 
      let eval_bop v1 v2 = match op with
        | Add -> Ok (VNum (v1 + v2))
        | Sub -> Ok (VNum (v1 - v2))
        | Mul -> Ok (VNum (v1 * v2))
        | Div -> if v2 = 0 then Error DivByZero else Ok (VNum (v1 / v2))
        | Mod -> if v2 = 0 then Error DivByZero else Ok (VNum (v1 mod v2))
        | Lt -> Ok (VBool (v1 < v2))
        | Lte -> Ok (VBool (v1 <= v2))
        | Gt -> Ok (VBool (v1 > v2))
        | Gte -> Ok (VBool (v1 >= v2))
        | Eq -> Ok (VBool (v1 = v2))
        | Neq -> Ok (VBool (v1 <> v2))
        | And -> Ok (VBool (v1 <> 0 && v2 <> 0))
        | Or -> Ok (VBool (v1 <> 0 || v2 <> 0))
      in
      (match eval e1, eval e2 with
        | Ok (VNum v1), Ok (VNum v2) -> eval_bop v1 v2
        | Ok (VBool v1), Ok (VBool v2) -> eval_bop (if v1 then 1 else 0) (if v2 then 1 else 0)
        | Ok (VNum _), Ok (VBool _) | Ok (VBool _), Ok (VNum _) -> Error (InvalidArgs op )
        | Ok _, Ok _ -> Error (InvalidArgs op) 
        | Error err, _ | _, Error err -> (Error err) )
        
  | If (e1, e2, e3) ->
      (match eval e1 with
        | Ok (VBool true) -> eval e2
        | Ok (VBool false) -> eval e3
        | Ok _ -> Error InvalidIfCond 
        | Error _ as err -> err) 
  | Let (x, e1, e2) ->
      (match eval e1 with
        | Ok v1 -> eval (subst (v1) x e2)
        | Error _ as err -> err) 
  | Fun (x, e) -> Ok (VFun (x, e))  
 
let interp input =
  match parse input with
  | Some expr -> eval expr 
  | _ -> Error ParseFail