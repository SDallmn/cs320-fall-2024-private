

open Utils

type env = (string * value) list

let eval_value (v : value) : expr =
  match v with
  | VNum n -> Num n
  | VBool true -> True
  | VBool false -> False
  | VUnit -> Unit
  | VFun (x, body) -> Fun (x, body)

let rec fv expr =
  match expr with
  | Var x -> [x]
  | App (e1, e2) -> (fv e1) @ (fv e2)
  | Bop (_, e1, e2) -> (fv e1) @ (fv e2)
  | Let (v, e1, e2) -> (fv e1) @ (List.filter (fun fv -> fv <> v) (fv e2))
  | Fun (v, e) -> List.filter (fun fv -> fv <> v) (fv e)
  | If (e1, e2, e3) -> (fv e1) @ (fv e2) @ (fv e3)
  | _ -> []

let rec subst (v : value) x e : expr = 
  match e with
  | Var y -> if y = x then eval_value v else e
  | App (e1, e2) -> App (subst v x e1, subst v x e2)
  | Bop (op, e1, e2) -> Bop (op, subst v x e1, subst v x e2)
  | If (cond, e1, e2) -> If (subst v x cond, subst v x e1, subst v x e2)
  | Let (y, e1, e2) -> 
      if y = x then Let (y, subst v x e1, e2) 
      else Let (y, subst v x e1, subst v x e2)
  | Fun (y, body) -> 
      if y = x then e 
      (* else Fun (gensym (), subst v x (subst v x body)) *)
      else Fun (y, subst v x body) 
  | _ -> e

let eval_bop op v1 v2 =
  match op, v1, v2 with
  | Add, VNum x, VNum y -> Ok (VNum (x + y))
  | Sub, VNum x, VNum y -> Ok (VNum (x - y))
  | Mul, VNum x, VNum y -> Ok (VNum (x * y))
  | Div, VNum x, VNum y -> if y = 0 then Error DivByZero else Ok (VNum (x / y))
  | Mod, VNum x, VNum y -> if y = 0 then Error DivByZero else Ok (VNum (x mod y))
  | Lt, VNum x, VNum y -> Ok (VBool (x < y))
  | Lte, VNum x, VNum y -> Ok (VBool (x <= y))
  | Gt, VNum x, VNum y -> Ok (VBool (x > y))
  | Gte, VNum x, VNum y -> Ok (VBool (x >= y))
  | Eq, VNum x, VNum y -> Ok (VBool (x = y))
  | Neq, VNum x, VNum y -> Ok (VBool (x <> y))
  | And, VBool x, VBool y -> Ok (VBool (x && y))
  | Or, VBool x, VBool y -> Ok (VBool (x || y))
  | _ -> Error (InvalidArgs op)

let eval e =
  let rec loop e env =
    match e with
    | Num n -> Ok (VNum n)
    | True -> Ok (VBool true)
    | False -> Ok (VBool false)
    | Unit -> Ok VUnit
    | Var x -> 
        (match List.assoc_opt x env with
        | Some v -> Ok v
        | None -> Error (UnknownVar x))
    | Let (x, e1, e2) -> 
        (match loop e1 env with
        | Ok v1 -> loop e2 ((x, v1) :: env)
        | Error err -> Error err)
    | Fun (x, body) -> Ok (VFun (x, body))
    | App (e1, e2) ->
        (match loop e1 env with
        | Ok (VFun (x, body)) ->
            (match loop e2 env with
              | Ok v2 ->
            (match v2 with 
              | VFun (v, e) -> if List.mem v (fv body) then loop (subst (VFun (gensym (), e)) x body) env else loop (subst v2 x body) env
              | _ -> loop (subst v2 x body) env)
              | Error err -> Error err)
        | Ok _ -> Error InvalidApp
        | Error err -> Error err)
    | Bop (op, e1, e2) -> 
        (match loop e1 env with
        | Ok v1 -> 
            (match loop e2 env with
              | Ok v2 -> eval_bop op v1 v2
              | Error err -> Error err)
        | Error err -> Error err)
    | If (cond, e1, e2) ->
        (match loop cond env with
        | Ok (VBool true) -> loop e1 env
        | Ok (VBool false) -> loop e2 env
        | Ok _ -> Error InvalidIfCond
        | Error err -> Error err)
in loop e []

let parse s = My_parser.parse s

let interp s =
  match (parse s) with
  | None -> Error ParseFail
  | Some p -> eval p