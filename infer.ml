open Ast
open Types

exception TypeError of string

module Env = Map.Make(String)

type tyenv = scheme Env.t
type subst = (string * ty) list

let empty_subst : subst = []

(* fresh type variables *)
let counter = ref 0
let fresh_tyvar prefix =
  incr counter;
  TVar (prefix ^ string_of_int !counter)

(* free type variables *)
let rec ftv_ty = function
  | TInt | TBool -> []
  | TVar a -> [a]
  | TFun (t1, t2) -> ftv_ty t1 @ ftv_ty t2
  | TCon (_, None) -> []
  | TCon (_, Some t) -> ftv_ty t

let ftv_scheme (Forall (vars, t)) =
  List.filter (fun v -> not (List.mem v vars)) (ftv_ty t)

let ftv_env env =
  Env.fold (fun _ s acc -> ftv_scheme s @ acc) env []

(* substitution *)
let rec apply_subst s = function
  | TInt | TBool as t -> t
  | TVar a ->
      (match List.assoc_opt a s with
       | Some t -> t
       | None -> TVar a)
  | TFun (t1, t2) ->
      TFun (apply_subst s t1, apply_subst s t2)
  | TCon (n, ot) ->
      TCon (n, Option.map (apply_subst s) ot)

let apply_subst_scheme s (Forall (vars, t)) =
  let s' = List.filter (fun (v, _) -> not (List.mem v vars)) s in
  Forall (vars, apply_subst s' t)

let apply_subst_env s env =
  Env.map (apply_subst_scheme s) env

let compose s1 s2 =
  List.map (fun (v, t) -> (v, apply_subst s1 t)) s2 @ s1

(* unification *)
let rec occurs a = function
  | TVar b -> a = b
  | TFun (t1, t2) -> occurs a t1 || occurs a t2
  | TCon (_, Some t) -> occurs a t
  | _ -> false

let rec unify t1 t2 =
  match t1, t2 with
  | TInt, TInt | TBool, TBool -> empty_subst
  | TVar a, t | t, TVar a ->
      if t = TVar a then empty_subst
      else if occurs a t then
        raise (TypeError "occurs check failed")
      else [ (a, t) ]
  | TFun (l1, r1), TFun (l2, r2) ->
      let s1 = unify l1 l2 in
      let s2 = unify (apply_subst s1 r1) (apply_subst s1 r2) in
      compose s2 s1
  | _ ->
      raise (TypeError "cannot unify types")

(* generalization *)
let generalize env t =
  let env_ftv = ftv_env env in
  let t_ftv = ftv_ty t in
  let vars =
    List.filter (fun v -> not (List.mem v env_ftv)) t_ftv
  in
  Forall (vars, t)

(* instantiation *)
let instantiate (Forall (vars, t)) =
  let subst =
    List.map (fun v -> (v, fresh_tyvar "a")) vars
  in
  apply_subst subst t

(* pattern inference *)
let infer_pattern t = function
  | PInt _ ->
      (unify t TInt, Env.empty)
  | PBool _ ->
      (unify t TBool, Env.empty)
  | PVar x ->
      (empty_subst, Env.singleton x (Forall ([], t)))
  | PWildcard ->
      (empty_subst, Env.empty)

(* inference *)
let rec infer env = function
  | Int _ -> (empty_subst, TInt)
  | Bool _ -> (empty_subst, TBool)

  | Var x ->
      (match Env.find_opt x env with
       | Some s -> (empty_subst, instantiate s)
       | None -> raise (TypeError ("unbound variable " ^ x)))

  | Fun (x, body) ->
      let tv = fresh_tyvar "a" in
      let env' = Env.add x (Forall ([], tv)) env in
      let s1, t1 = infer env' body in
      (s1, TFun (apply_subst s1 tv, t1))

  | App (e1, e2) ->
      let s1, t1 = infer env e1 in
      let s2, t2 = infer (apply_subst_env s1 env) e2 in
      let tv = fresh_tyvar "r" in
      let s3 = unify (apply_subst s2 t1) (TFun (t2, tv)) in
      let s = compose s3 (compose s2 s1) in
      (s, apply_subst s3 tv)

  | Let (x, e1, e2) ->
      let s1, t1 = infer env e1 in
      let env' = apply_subst_env s1 env in
      let scheme = generalize env' t1 in
      let env'' = Env.add x scheme env' in
      let s2, t2 = infer env'' e2 in
      (compose s2 s1, t2)

  | LetRec (f, x, body, cont) ->
      let tv_arg = fresh_tyvar "a" in
      let tv_res = fresh_tyvar "r" in
      let f_ty = TFun (tv_arg, tv_res) in
      let env' =
        Env.add f (Forall ([], f_ty))
          (Env.add x (Forall ([], tv_arg)) env)
      in
      let s1, t1 = infer env' body in
      let s2 = unify (apply_subst s1 tv_res) t1 in
      let env'' =
        Env.add f (generalize env (apply_subst s2 f_ty)) env
      in
      let s3, t3 = infer (apply_subst_env s2 env'') cont in
      (compose s3 (compose s2 s1), t3)

  | Match (e, branches) ->
      let s0, t_scrut = infer env e in
      let tv_res = fresh_tyvar "m" in

      let infer_branch (s_acc, _) (pat, expr) =
        let s_pat, env_pat =
          infer_pattern (apply_subst s_acc t_scrut) pat
        in
        let env' =
          Env.union (fun _ _ b -> Some b)
            (apply_subst_env s_pat env)
            env_pat
        in
        let s_expr, t_expr = infer env' expr in
        let s_res = unify (apply_subst s_expr tv_res) t_expr in
        let s = compose s_res (compose s_expr (compose s_pat s_acc)) in
        (s, apply_subst s tv_res)
      in

      List.fold_left infer_branch (s0, tv_res) branches
