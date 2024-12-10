(* open Utils
include My_parser

let unify _ _ = assert false

let type_of _ _ = assert false

exception AssertFail
exception DivByZero
exception RecWithoutArg
exception CompareFunVals

let eval_expr _ _ = assert false

let type_check =
  let rec go ctxt = function
  | [] -> Some (Forall ([], TUnit))
  | {is_rec;name;value} :: ls ->
    match type_of ctxt (Let {is_rec;name;value; body = Var name}) with
    | Some ty -> (
      match ls with
      | [] -> Some ty
      | _ ->
        let ctxt = Env.add name ty ctxt in
        go ctxt ls
    )
    | None -> None
  in go Env.empty

let eval p =
  let rec nest = function
    | [] -> Unit
    | [{is_rec;name;value}] -> Let {is_rec;name;value;body = Var name}
    | {is_rec;name;value} :: ls -> Let {is_rec;name;value;body = nest ls}
  in eval_expr Env.empty (nest p)

let interp input =
  match parse input with
  | Some prog -> (
    match type_check prog with
    | Some ty -> Ok (eval prog, ty)
    | None -> Error TypeError
  )
  | None -> Error ParseError

   *)
   
open Utils
include My_parser

(* Helper Functions *)
let rec substitute subst ty =
  match ty with
  | TVar x -> (try List.assoc x subst with Not_found -> ty)
  | TFun (t1, t2) -> TFun (substitute subst t1, substitute subst t2)
  | TPair (t1, t2) -> TPair (substitute subst t1, substitute subst t2)
  | TList t -> TList (substitute subst t)
  | TOption t -> TOption (substitute subst t)
  | _ -> ty








  let rec unify ty constraints =
    let  deduplicate_sorted cmp lst =
      let rec loop acc = function
        | [] -> List.rev acc
        | [x] -> List.rev (x :: acc)
        | x :: (y :: _ as rest) -> if cmp x y = 0 then loop acc rest else loop (x :: acc) rest
      in
      loop [] (List.sort cmp lst)
    in
    match constraints with
    | [] ->
      let rec free_var ty =
        match ty with
        | TVar x -> [x]
        | TFun (t1, t2) | TPair (t1, t2) -> free_var t1 @ free_var t2
        | TList t | TOption t -> free_var t
        | _ -> []
      in
      let free_vars_list = free_var ty in
      let unique_vars = deduplicate_sorted compare free_vars_list in
      Some (Forall (unique_vars, ty))
    | (t1, t2) :: rest ->
      if t1 = t2 then unify ty rest
      else match (t1, t2) with
        | (TVar x, t) | (t, TVar x) ->
          let rec happens x ty =
            match ty with
            | TVar y -> x = y
            | TFun (t1, t2) | TPair (t1, t2) -> happens x t1 || happens x t2
            | TList t | TOption t -> happens x t
            | _ -> false
          in
          if happens x t then None
          else
            let subst = [(x, t)] in
            let updated_ty = substitute subst ty in
            let updated_constraints =
              List.map (fun (a, b) -> (substitute subst a, substitute subst b)) rest
            in
            (match unify updated_ty updated_constraints with
            | Some (Forall (vars, final_ty)) ->
              let filtered_vars = List.filter (fun v -> v <> x) vars in
              Some (Forall (filtered_vars, final_ty))
            | None -> None)
        | (TFun (a1, b1), TFun (a2, b2)) ->
          unify ty ((a1, a2) :: (b1, b2) :: rest)
        | (TPair (a1, b1), TPair (a2, b2)) ->
          unify ty ((a1, a2) :: (b1, b2) :: rest)
        | (TList a, TList b) | (TOption a, TOption b) ->
          unify ty ((a, b) :: rest)
        | (TInt, TFloat) | (TFloat, TInt) | (TBool, TInt) | (TBool, TFloat) -> None
        | _ -> None
  

(* type_of Function *)
(* let type_of (env : stc_env) (e : expr) : ty_scheme option =
  let rec infer env = function
    | Unit -> (TUnit, [])
    | True | False -> (TBool, [])
    | Int _ -> (TInt, [])
    | Float _ -> (TFloat, [])
    | Var x -> let instant (vars, ty) =
      let subst = List.map (fun var -> (var, TVar (gensym ()))) vars in
      substitute subst ty in
      (
        match Env.find_opt x env with
        
        | Some (Forall (vars, t)) -> (instant (vars, t), [])
        | None -> failwith ("Unbound variable: " ^ x)
      )
    | ENone -> (TOption (TVar (gensym ())), [])
    | ESome e ->
      let t, c = infer env e in
      (TOption t, c)
    | Nil -> (TList (TVar (gensym ())), [])
    | OptMatch { matched; some_name; some_case; none_case } ->
      let t_matched, c_matched = infer env matched in
      let fresh_elem = TVar (gensym ()) in
      let env_with_some = Env.add some_name (Forall ([], fresh_elem)) env in
      let t_some_case, c_some = infer env_with_some some_case in
      let t_none_case, c_none = infer env none_case in
      let constraints =
        (t_matched, TOption fresh_elem) ::
        (t_some_case, t_none_case) :: 
        c_matched @ c_some @ c_none
      in
      (t_some_case, constraints)
    | Bop (op, e1, e2) -> (
        let t1, c1 = infer env e1 in
        let t2, c2 = infer env e2 in
        match op with
        | Add | Sub | Mul | Div | Mod ->
            (TInt, (t1, TInt) :: (t2, TInt) :: c1 @ c2)
        | AddF | SubF | MulF | DivF | PowF ->
            (TFloat, (t1, TFloat) :: (t2, TFloat) :: c1 @ c2)
        | And | Or ->
            (TBool, (t1, TBool) :: (t2, TBool) :: c1 @ c2)
        | Eq | Neq | Lt | Lte | Gt | Gte ->
            let fresh = TVar (gensym ()) in
            (TBool, (t1, fresh) :: (t2, fresh) :: c1 @ c2)
        | Cons ->
          let t1, c1 = infer env e1 in
          let t2, c2 = infer env e2 in
          (TList t1, (t2, TList t1) :: c1 @ c2)
        | Concat ->
          let t1, c1 = infer env e1 in
          let t2, c2 = infer env e2 in
          let fresh = TVar (gensym ()) in
          (TList fresh, (t1, TList fresh) :: (t2, TList fresh) :: c1 @ c2)
        | Comma ->
          let t1, c1 = infer env e1 in
          let t2, c2 = infer env e2 in
          (TPair (t1, t2), c1 @ c2)
      )
    | If (e1, e2, e3) ->
        let t1, c1 = infer env e1 in
        let t2, c2 = infer env e2 in
        let t3, c3 = infer env e3 in
        (t3, (t1, TBool) :: (t2, t3) :: c1 @ c2 @ c3)
    | Fun (x, Some ty, body) ->
        let env = Env.add x (Forall ([], ty)) env in
        let t_body, c_body = infer env body in
        (TFun (ty, t_body), c_body)
    | Fun (arg, None, body) ->
      let fresh_arg = TVar (gensym ()) in
      let env = Env.add arg (Forall ([], fresh_arg)) env in
      let t_body, c_body = infer env body in
      (TFun (fresh_arg, t_body), c_body)   
    | App (e1, e2) ->
      let t_fun, c_fun = infer env e1 in
      let t_arg, c_arg = infer env e2 in
      let fresh = TVar (gensym ()) in
      let constraints = (t_fun, TFun (t_arg, fresh)) :: c_fun @ c_arg in
      (fresh, constraints)
    | Let { is_rec = false; name; value; body } ->
      let t_val, c_val = infer env value in
      let env = Env.add name (Forall ([], t_val)) env in
      let t_body, c_body = infer env body in
      (t_body, c_val @ c_body)
    | Let { is_rec = true; name; value; body;} ->
      let fresh1 = TVar (gensym ()) in  
      let fresh2 = TVar (gensym ()) in  
      let env_with_f = Env.add name (Forall ([], TFun (fresh1, fresh2))) env in  
      let t_val, c_val = infer env_with_f value in
      let env_with_f_for_body = Env.add name (Forall ([], TFun (fresh1, fresh2))) env in
      let t_body, c_body = infer env_with_f_for_body body in
      let constraints = c_val @ c_body @ [(t_val, TFun (fresh1, fresh2))] in
      (t_body, constraints)
    | Assert False -> (TVar (gensym ()), [])
    | Assert e ->
      let t, c = infer env e in
      (TUnit, (t, TBool) :: c)  
    | Annot (e, ty) ->
        let t, c = infer env e in
        (ty, (t, ty) :: c)
    | PairMatch { matched; fst_name; snd_name; case } ->
      let t_matched, c_matched = infer env matched in
      let fresh1 = TVar (gensym ()) in
      let fresh2 = TVar (gensym ()) in
      let extended_env = Env.add fst_name (Forall ([], fresh1)) (Env.add snd_name (Forall ([], fresh2)) env) in
      let t_case, c_case = infer extended_env case in
      (t_case, (t_matched, TPair (fresh1, fresh2)) :: c_matched @ c_case)
    | ListMatch { matched; hd_name; tl_name; cons_case; nil_case } ->
      let t_matched, c_matched = infer env matched in
      let fresh_elem = TVar (gensym ()) in
      let env_hd = Env.add hd_name (Forall ([], fresh_elem)) env in
      let env_tl = Env.add tl_name (Forall ([], TList fresh_elem)) env_hd in
      let t_cons_case, c_cons_case = infer env_tl cons_case in
      let t_nil_case, c_nil_case = infer env nil_case in
      let constraints =
        (t_matched, TList fresh_elem)
        :: (t_cons_case, t_nil_case)
        :: c_matched @ c_cons_case @ c_nil_case
      in
      (t_cons_case, constraints)
  in
  try
    let t, c = infer env e in
    let t = unify t c in
    match t with
    | Some t -> Some t 
    | None -> None
  with _ -> None

*)

let type_of (env : stc_env) (expr : expr) : ty_scheme option =
  let fresh_tvar () = TVar (gensym ()) in

  let instantiate (vars, ty) =
    let mapping = List.map (fun v -> (v, fresh_tvar ())) vars in
    substitute mapping ty
  in

  let bind env name t = Env.add name (Forall ([], t)) env in

  let rec infer env e =
    match e with
    | Unit -> (TUnit, [])
    | True | False -> (TBool, [])
    | Int _ -> (TInt, [])
    | Float _ -> (TFloat, [])
    | Var x -> infer_var env x
    | ENone -> infer_none ()
    | ESome e' -> infer_some env e'
    | Nil -> infer_nil ()
    | OptMatch { matched; some_name; some_case; none_case } ->
        infer_opt_match env matched some_name some_case none_case
    | Bop (op, e1, e2) ->
        infer_bop env op e1 e2
    | If (c, tbr, ebr) ->
        infer_if env c tbr ebr
    | Fun (x, anno, body) ->
        infer_fun env x anno body
    | App (f, x) ->
        infer_app env f x
    | Let { is_rec; name; value; body } ->
        infer_let env is_rec name value body
    | Assert e' ->
        infer_assert env e'
    | Annot (e', ty) ->
        infer_annot env e' ty
    | PairMatch { matched; fst_name; snd_name; case } ->
        infer_pair_match env matched fst_name snd_name case
    | ListMatch { matched; hd_name; tl_name; cons_case; nil_case } ->
        infer_list_match env matched hd_name tl_name cons_case nil_case

  and infer_var env x =
    match Env.find_opt x env with
    | Some (Forall (vars, ty)) ->
        (instantiate (vars, ty), [])
    | None -> failwith ("Unbound variable: " ^ x)

  and infer_none () =
    (TOption (fresh_tvar ()), [])

  and infer_some env e' =
    let t, c = infer env e' in
    (TOption t, c)

  and infer_nil () =
    (TList (fresh_tvar ()), [])

  and infer_opt_match env matched_expr s_name s_case n_case =
    let t_matched, c_matched = infer env matched_expr in
    let elem_t = fresh_tvar () in
    let env_some = bind env s_name elem_t in
    let t_s_case, c_s = infer env_some s_case in
    let t_n_case, c_n = infer env n_case in
    let combined = (t_matched, TOption elem_t) :: (t_s_case, t_n_case) :: c_matched @ c_s @ c_n in
    (t_s_case, combined)

  and infer_bop env op e1 e2 =
    let binary t_required_left t_required_right t_result =
      let left_t, left_c = infer env e1 in
      let right_t, right_c = infer env e2 in
      (t_result, (left_t, t_required_left) :: (right_t, t_required_right) :: left_c @ right_c)
    in
    match op with
    | Add | Sub | Mul | Div | Mod ->
        binary TInt TInt TInt
    | AddF | SubF | MulF | DivF | PowF ->
        binary TFloat TFloat TFloat
    | And | Or ->
        binary TBool TBool TBool
    | Eq | Neq | Lt | Lte | Gt | Gte ->
        let fresh = fresh_tvar () in
        binary fresh fresh TBool
    | Cons ->
        let t_h, c_h = infer env e1 in
        let t_t, c_t = infer env e2 in
        (TList t_h, (t_t, TList t_h) :: c_h @ c_t)
    | Concat ->
        let t_l1, c1 = infer env e1 in
        let t_l2, c2 = infer env e2 in
        let el = fresh_tvar () in
        (TList el, (t_l1, TList el) :: (t_l2, TList el) :: c1 @ c2)
    | Comma ->
        let t_f, c_f = infer env e1 in
        let t_s, c_s = infer env e2 in
        (TPair (t_f, t_s), c_f @ c_s)

  and infer_if env c tbr ebr =
    let t_c, c_c = infer env c in
    let t_t, c_t = infer env tbr in
    let t_e, c_e = infer env ebr in
    (t_e, (t_c, TBool) :: (t_t, t_e) :: c_c @ c_t @ c_e)

  and infer_fun env x maybe_anno body =
    match maybe_anno with
    | Some t_arg ->
        let env_fun = bind env x t_arg in
        let t_body, c_body = infer env_fun body in
        (TFun (t_arg, t_body), c_body)
    | None ->
        let fresh = fresh_tvar () in
        let env_fun = bind env x fresh in
        let t_body, c_body = infer env_fun body in
        (TFun (fresh, t_body), c_body)

  and infer_app env f x =
    let t_f, c_f = infer env f in
    let t_x, c_x = infer env x in
    let t_res = fresh_tvar () in
    (t_res, (t_f, TFun (t_x, t_res)) :: c_f @ c_x)

  and infer_let env is_rec name value body =
    if not is_rec then
      let t_val, c_val = infer env value in
      let env_val = bind env name t_val in
      let t_body, c_body = infer env_val body in
      (t_body, c_val @ c_body)
    else
      let in_t = fresh_tvar () in
      let out_t = fresh_tvar () in
      let env_pre = bind env name (TFun (in_t, out_t)) in
      let t_val, c_val = infer env_pre value in
      let env_body = bind env name (TFun (in_t, out_t)) in
      let t_body, c_body = infer env_body body in
      (t_body, c_val @ c_body @ [(t_val, TFun (in_t, out_t))])

  and infer_assert env e' =
    match e' with
    | False ->
        (fresh_tvar (), [])
    | _ ->
        let t, c = infer env e' in
        (TUnit, (t, TBool) :: c)

  and infer_annot env e' ty =
    let t, c = infer env e' in
    (ty, (t, ty) :: c)

  and infer_pair_match env matched fst_name snd_name case =
    let t_m, c_m = infer env matched in
    let f_t = fresh_tvar () in
    let s_t = fresh_tvar () in
    let env_ext = bind (bind env snd_name s_t) fst_name f_t in
    let t_case, c_case = infer env_ext case in
    (t_case, (t_m, TPair (f_t, s_t)) :: c_m @ c_case)

  and infer_list_match env matched hd_name tl_name cons_case nil_case =
    let t_m, c_m = infer env matched in
    let elem_t = fresh_tvar () in
    let env_hd = bind env hd_name elem_t in
    let env_tl = bind env_hd tl_name (TList elem_t) in
    let t_cons, c_cons = infer env_tl cons_case in
    let t_nil, c_nil = infer env nil_case in
    (t_cons, (t_m, TList elem_t) :: (t_cons, t_nil) :: c_m @ c_cons @ c_nil)
  in

  try
    let (t_inferred, constraints) = infer env expr in
    let unified = unify t_inferred constraints in
    match unified with
    | Some final_type -> Some final_type
    | None -> None
  with _ -> None

exception AssertFail
exception DivByZero
exception RecWithoutArg
exception CompareFunVals 




  let rec eval_expr env expr : value =
    let rec eval_local local_env e =
      let fail msg = failwith msg in
  
      let eval_binop op left_expr right_expr =
        let l_val = eval_local local_env left_expr in
        let r_val = eval_local local_env right_expr in
        match op with
        | Add -> (match l_val, r_val with VInt m, VInt n -> VInt (m+n) | _ -> fail "Add requires two ints")
        | Sub -> (match l_val, r_val with VInt m, VInt n -> VInt (m-n) | _ -> fail "Sub requires two ints")
        | Mul -> (match l_val, r_val with VInt m, VInt n -> VInt (m*n) | _ -> fail "Mul requires two ints")
        | Div -> (
            match l_val with
            | VInt m ->
              (match r_val with
               | VInt 0 -> raise DivByZero
               | VInt n -> VInt (m / n)
               | _ -> fail "Division requires int")
            | _ -> fail "Division requires int dividend")
        | Mod -> (
            match l_val, r_val with
            | VInt m, VInt n when n <> 0 -> VInt (m mod n)
            | VInt _, VInt 0 -> fail "Modulo by zero"
            | _ -> fail "Modulo requires two ints")
        | AddF -> (match l_val, r_val with VFloat m, VFloat n -> VFloat (m +. n) | _ -> fail "AddF requires floats")
        | SubF -> (match l_val, r_val with VFloat m, VFloat n -> VFloat (m -. n) | _ -> fail "SubF requires floats")
        | MulF -> (match l_val, r_val with VFloat m, VFloat n -> VFloat (m *. n) | _ -> fail "MulF requires floats")
        | DivF -> (
            match l_val with
            | VFloat m ->
              (match r_val with
               | VFloat n -> VFloat (m /. n)
               | _ -> fail "DivF requires floats")
            | _ -> fail "DivF requires float dividend")
        | PowF -> (match l_val, r_val with VFloat a, VFloat b -> VFloat (a ** b) | _ -> fail "PowF requires floats")
        | Eq | Neq | Lt | Lte | Gt | Gte ->
          let lhs = eval_expr local_env left_expr in
          let rhs = eval_expr local_env right_expr in
          (match lhs, rhs with
           | VClos _, _ | _, VClos _ -> raise CompareFunVals
           | _ ->
             let cmp = 
               match op with
               | Eq -> lhs = rhs
               | Neq -> lhs <> rhs
               | Lt -> lhs < rhs
               | Lte -> lhs <= rhs
               | Gt -> lhs > rhs
               | Gte -> lhs >= rhs
               | _ -> fail "Unexpected comparison operator"
             in VBool cmp)
        | And -> (
            match eval_local local_env left_expr with
            | VBool false -> VBool false
            | VBool true -> eval_local local_env right_expr
            | _ -> fail "And requires bools"
          )
        | Or -> (
            match eval_local local_env left_expr with
            | VBool true -> VBool true
            | VBool false -> eval_local local_env right_expr
            | _ -> fail "Or requires bools"
          )
        | Comma ->
          let l = eval_local local_env left_expr in
          let r = eval_local local_env right_expr in
          VPair (l, r)
        | Cons ->
          let hd_val = eval_local local_env left_expr in
          let tl_val = eval_local local_env right_expr in
          (match tl_val with
           | VList tail_list -> VList (hd_val :: tail_list)
           | _ -> fail "Cons expects a list on the right")
        | Concat ->
          let lhs = eval_local local_env left_expr in
          let rhs = eval_local local_env right_expr in
          (match lhs, rhs with
           | VList l1, VList l2 -> VList (l1 @ l2)
           | _ -> fail "Concat requires two lists")
      in
  
      let apply_function f_expr arg_expr =
        match eval_local local_env f_expr with
        | VClos { env = closure_env; name = fn_name; arg; body } ->
          let closure_env =
            match fn_name with
            | None -> closure_env
            | Some f -> Env.add f (VClos { env = closure_env; name = Some f; arg; body }) closure_env
          in
          let closure_env = Env.add arg (eval_local local_env arg_expr) closure_env in
          eval_expr closure_env body
        | _ -> fail "Application to a non-closure"
      in
  
      let eval_let is_rec name value_expr body_expr =
        if not is_rec then
          let evaluated_val = eval_local local_env value_expr in
          let extended = Env.add name evaluated_val local_env in
          eval_expr extended body_expr
        else
          let closure_val =
            match eval_expr local_env value_expr with
            | VClos { name = None; arg; body; env = clos_env } ->
              VClos { name = Some name; arg; body; env = clos_env }
            | VClos { name = Some _; _ } ->
              raise RecWithoutArg
            | _ -> fail "Recursive let expects a closure"
          in
          let rec_env = Env.add name closure_val local_env in
          eval_expr rec_env body_expr
      in
  
      let eval_opt_match matched_expr some_name some_case none_case =
        match eval_expr local_env matched_expr with
        | VSome v -> eval_expr (Env.add some_name v local_env) some_case
        | VNone -> eval_expr local_env none_case
        | _ -> fail "OptMatch expects an option"
      in
  
      let eval_list_match matched_expr hd_name tl_name cons_case nil_case =
        match eval_local local_env matched_expr with
        | VList (h :: t) ->
          let env_cons = Env.add hd_name h (Env.add tl_name (VList t) local_env) in
          eval_expr env_cons cons_case
        | VList [] -> eval_expr local_env nil_case
        | _ -> fail "ListMatch expects a list"
      in
  
      let eval_pair_match matched_expr fst_name snd_name case =
        match eval_local local_env matched_expr with
        | VPair (v1, v2) ->
          let env_pair = Env.add fst_name v1 (Env.add snd_name v2 local_env) in
          eval_expr env_pair case
        | _ -> fail "PairMatch expects a pair"
      in
  
      match e with
      | Unit -> VUnit
      | True -> VBool true
      | False -> VBool false
      | Int n -> VInt n
      | Float f -> VFloat f
      | ENone -> VNone
      | Nil -> VList []
      | Var x -> Env.find x local_env
      | Fun (x, _, body) -> VClos { name = None; arg = x; body; env = local_env }
      | App (fn_e, arg_e) -> apply_function fn_e arg_e
      | Bop (op, e1, e2) -> eval_binop op e1 e2
      | ESome inner -> VSome (eval_expr local_env inner)
      | OptMatch { matched; some_name; some_case; none_case } -> eval_opt_match matched some_name some_case none_case
      | If (cond, then_e, else_e) -> (
          match eval_local local_env cond with
          | VBool true -> eval_local local_env then_e
          | VBool false -> eval_local local_env else_e
          | _ -> fail "If condition must be boolean"
        )
      | ListMatch { matched; hd_name; tl_name; cons_case; nil_case } ->
        eval_list_match matched hd_name tl_name cons_case nil_case
      | PairMatch { matched; fst_name; snd_name; case } ->
        eval_pair_match matched fst_name snd_name case
      | Assert assert_e -> (
          match eval_local local_env assert_e with
          | VBool true -> VUnit
          | _ -> raise AssertFail
        )
      | Let { is_rec; name; value; body } -> eval_let is_rec name value body
      | Annot (annot_e, _) -> eval_expr local_env annot_e
    in
    eval_local env expr
  

let type_check =
  let rec go ctxt = function
  | [] -> Some (Forall ([], TUnit))
  | {is_rec;name;value} :: ls ->
    match type_of ctxt (Let {is_rec;name;value; body = Var name}) with
    | Some ty -> (
      match ls with
      | [] -> Some ty
      | _ ->
        let ctxt = Env.add name ty ctxt in
        go ctxt ls
    )
    | None -> None
  in go Env.empty

let eval p =
  let rec nest = function
    | [] -> Unit
    | [{is_rec;name;value}] -> Let {is_rec;name;value;body = Var name}
    | {is_rec;name;value} :: ls -> Let {is_rec;name;value;body = nest ls}
  in eval_expr Env.empty (nest p)

let interp input =
  match parse input with
  | Some prog -> (
    match type_check prog with
    | Some ty -> Ok (eval prog, ty)
    | None -> Error TypeError
  )
  | None -> Error ParseError