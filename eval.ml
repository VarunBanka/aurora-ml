open Ast

exception RuntimeError of string

type value =
  | VInt of int
  | VBool of bool
  | VClosure of string * expr * env

and env = (string * value) list

let rec lookup x = function
  | [] -> raise (RuntimeError ("unbound variable " ^ x))
  | (y, v) :: rest -> if x = y then v else lookup x rest

let rec eval env = function
  | Int n -> VInt n
  | Bool b -> VBool b
  | Var x -> lookup x env

  | Fun (x, body) ->
      VClosure (x, body, env)

  | App (e1, e2) ->
      let v1 = eval env e1 in
      let v2 = eval env e2 in
      (match v1 with
       | VClosure (x, body, clo_env) ->
           eval ((x, v2) :: clo_env) body
       | _ -> raise (RuntimeError "application of non-function"))

  | Let (x, e1, e2) ->
      let v1 = eval env e1 in
      eval ((x, v1) :: env) e2

  | LetRec (f, x, body, cont) ->
      let rec closure =
        VClosure (x, body, (f, closure) :: env)
      in
      eval ((f, closure) :: env) cont

  | If (c, t, e) ->
      (match eval env c with
       | VBool true -> eval env t
       | VBool false -> eval env e
       | _ -> raise (RuntimeError "condition not boolean"))

  | Match (e, branches) ->
      let v = eval env e in
      eval_match v env branches

and eval_match v env = function
  | [] ->
      raise (RuntimeError "non-exhaustive pattern match")
  | (p, expr) :: rest ->
      (match match_pattern v p with
       | Some binds -> eval (binds @ env) expr
       | None -> eval_match v env rest)

and match_pattern v = function
  | PInt n ->
      (match v with VInt m when m = n -> Some [] | _ -> None)
  | PBool b ->
      (match v with VBool c when b = c -> Some [] | _ -> None)
  | PVar x ->
      Some [ (x, v) ]
  | PWildcard ->
      Some []
