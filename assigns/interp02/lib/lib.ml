open Utils

let parse = My_parser.parse


exception DivByZero
exception AssertFail

let desugar (prog : prog) : expr =
  let rec process_top_level_bindings bindings =
    match bindings with
    | [] -> Unit
    | { is_rec; name; args; ty; value } :: remaining_bindings ->
        let func_type = 
          List.fold_right 
            (fun (_, arg_type) result_type -> FunTy(arg_type, result_type)) 
            args 
            ty 
        in
        let transformed_value =
          List.fold_right
            (fun (arg_name, arg_type) body -> Fun(arg_name, arg_type, body)) 
            args 
            (transform_expression value)
        in
        Let { 
          is_rec; 
          name; 
          ty = func_type; 
          value = transformed_value; 
          body = process_top_level_bindings remaining_bindings 
        }
  and transform_expression expr =
    match expr with
    | SLet { is_rec; name; args; ty; value; body } ->
        let func_type = 
          List.fold_right 
            (fun (_, arg_type) result_type -> FunTy(arg_type, result_type)) 
            args 
            ty 
        in
        let transformed_value =
          List.fold_right
            (fun (arg_name, arg_type) body -> Fun(arg_name, arg_type, body)) 
            args 
            (transform_expression value)
        in
        Let { 
          is_rec; 
          name; 
          ty = func_type; 
          value = transformed_value; 
          body = transform_expression body 
        }
    | SFun { arg; args; body } ->
        List.fold_right
          (fun (arg_name, arg_type) body -> Fun(arg_name, arg_type, body))
          (arg :: args)
          (transform_expression body)
    | SIf (condition, then_branch, else_branch) ->
        If (transform_expression condition, transform_expression then_branch, transform_expression else_branch)
    | SApp (func, argument) ->
        App (transform_expression func, transform_expression argument)
    | SBop (operator, lhs, rhs) ->
        Bop (operator, transform_expression lhs, transform_expression rhs)
    | SAssert assertion ->
        Assert (transform_expression assertion)
    | SUnit -> Unit
    | STrue -> True
    | SFalse -> False
    | SNum number -> Num number
    | SVar variable -> Var variable
  in
  process_top_level_bindings prog

  

let type_of (expr : expr) : (ty, error) result =
    let rec infer_type env expr =
      match expr with
      | Unit -> Ok UnitTy
      | Num _ -> Ok IntTy
      | True | False -> Ok BoolTy
      | Var x -> (
          match Env.find_opt x env with
          | Some ty -> Ok ty
          | None -> Error (UnknownVar x)
        )
      | Let { is_rec; name; ty = expected_ty; value; body } -> (
          if is_rec then
            let env_with_binding = Env.add name expected_ty env in
            match infer_type env_with_binding value with
            | Ok actual_ty when actual_ty = expected_ty ->
                infer_type (Env.add name expected_ty env_with_binding) body
            | Ok actual_ty -> Error (LetTyErr (expected_ty, actual_ty))
            | Error err -> Error err
          else
            match infer_type env value with
            | Ok actual_ty when actual_ty = expected_ty ->
                infer_type (Env.add name actual_ty env) body
            | Ok actual_ty -> Error (LetTyErr (expected_ty, actual_ty))
            | Error err -> Error err
        )
      | Fun (arg, arg_ty, body) ->
          let env_with_arg = Env.add arg arg_ty env in
          (match infer_type env_with_arg body with
          | Ok body_ty -> Ok (FunTy (arg_ty, body_ty))
          | Error err -> Error err)
      | App (func, arg) -> (
          match infer_type env func with
          | Ok (FunTy (arg_ty, ret_ty)) -> (
              match infer_type env arg with
              | Ok actual_ty when arg_ty = actual_ty -> Ok ret_ty
              | Ok actual_ty -> Error (FunArgTyErr (arg_ty, actual_ty))
              | Error err -> Error err
            )
          | Ok ty -> Error (FunAppTyErr ty)
          | Error err -> Error err
        )
      | If (cond, then_expr, else_expr) -> (
          match infer_type env cond with
          | Ok BoolTy -> (
              match infer_type env then_expr with
              | Ok then_ty -> (
                  match infer_type env else_expr with
                  | Ok else_ty when then_ty = else_ty -> Ok then_ty
                  | Ok else_ty -> Error (IfTyErr (then_ty, else_ty))
                  | Error err -> Error err
                )
              | Error err -> Error err
            )
          | Ok ty -> Error (IfCondTyErr ty)
          | Error err -> Error err
        )
      | Bop (operator, left_expr, right_expr) -> (
          let (left_ty, right_ty, result_ty) = 
            match operator with
            | Add | Sub | Mul | Div | Mod -> (IntTy, IntTy, IntTy)
            | Lt | Lte | Gt | Gte | Eq | Neq -> (IntTy, IntTy, BoolTy)
            | And | Or -> (BoolTy, BoolTy, BoolTy)
          in
          match infer_type env left_expr with
          | Error err -> Error err
          | Ok ty1 when ty1 <> left_ty -> Error (OpTyErrL (operator, left_ty, ty1))
          | Ok _ -> (
              match infer_type env right_expr with
              | Error err -> Error err
              | Ok ty2 when ty2 <> right_ty -> Error (OpTyErrR (operator, right_ty, ty2))
              | Ok _ -> Ok result_ty
            )
        )
      | Assert e -> (
          match infer_type env e with
          | Ok BoolTy -> Ok UnitTy
          | Ok ty -> Error (AssertTyErr ty)
          | Error err -> Error err
        )
    in
    infer_type Env.empty expr


    let eval (expr : expr) : value =
        let rec evaluate env expr =
          match expr with
          | Unit -> VUnit
          | Num n -> VNum n
          | True -> VBool true
          | False -> VBool false
          | Var x -> Env.find x env
          | Let { is_rec; name; ty = _; value; body } ->
              let new_env =
                if is_rec then
                  let closure =
                    match value with
                    | Fun (arg, _, body) -> VClos { name = Some name; arg; body; env }
                    | _ ->
                        let fresh_arg = gensym () in
                        let wrapped_body = Fun (fresh_arg, UnitTy, value) in
                        VClos { name = Some name; arg = fresh_arg; body = wrapped_body; env }
                  in
                  Env.add name closure env
                else
                  let v = evaluate env value in
                  Env.add name v env
              in
              evaluate new_env body
          | Fun (arg, _, body) -> VClos { name = None; arg; body; env }
          | App (func, arg) -> (
              match evaluate env func with
              | VClos { name = Some fname; arg = param; body; env = closure_env } ->
                  let arg_val = evaluate env arg in
                  let updated_env =
                    Env.add fname (VClos { name = Some fname; arg = param; body; env = closure_env })
                      (Env.add param arg_val closure_env)
                  in
                  evaluate updated_env body
              | VClos { name = None; arg = param; body; env = closure_env } ->
                  let arg_val = evaluate env arg in
                  let updated_env = Env.add param arg_val closure_env in
                  evaluate updated_env body
              | _ -> assert false
            )
          | If (condition, then_branch, else_branch) -> (
              match evaluate env condition with
              | VBool true -> evaluate env then_branch
              | VBool false -> evaluate env else_branch
              | _ -> assert false
            )
          | Bop (operator, left, right) -> (
              match operator with
              | And -> (
                  match evaluate env left with
                  | VBool false -> VBool false
                  | VBool true -> evaluate env right
                  | _ -> assert false
                )
              | Or -> (
                  match evaluate env left with
                  | VBool true -> VBool true
                  | VBool false -> evaluate env right
                  | _ -> assert false
                )
              | _ ->
                  let left_val = evaluate env left in
                  let right_val = evaluate env right in
                  match (left_val, right_val, operator) with
                  | (VNum n1, VNum n2, Add) -> VNum (n1 + n2)
                  | (VNum n1, VNum n2, Sub) -> VNum (n1 - n2)
                  | (VNum n1, VNum n2, Mul) -> VNum (n1 * n2)
                  | (VNum n1, VNum n2, Div) -> if n2 = 0 then raise DivByZero else VNum (n1 / n2)
                  | (VNum n1, VNum n2, Mod) -> if n2 = 0 then raise DivByZero else VNum (n1 mod n2)
                  | (VNum n1, VNum n2, Lt) -> VBool (n1 < n2)
                  | (VNum n1, VNum n2, Lte) -> VBool (n1 <= n2)
                  | (VNum n1, VNum n2, Gt) -> VBool (n1 > n2)
                  | (VNum n1, VNum n2, Gte) -> VBool (n1 >= n2)
                  | (VNum n1, VNum n2, Eq) -> VBool (n1 = n2)
                  | (VNum n1, VNum n2, Neq) -> VBool (n1 <> n2)
                  | _ -> assert false
            )
          | Assert e -> (
              match evaluate env e with
              | VBool true -> VUnit
              | VBool false -> raise AssertFail
              | _ -> assert false
            )
        in
        evaluate Env.empty expr
      

  let interp (input : string) : (value, error) result =
    match parse input with
    | Some prog -> (
        let expr = desugar prog in
        match type_of expr with
        | Ok _ -> Ok (eval expr)
        | Error e -> Error e
    )
    | None -> Error ParseErr