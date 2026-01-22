type ty =
  | TInt
  | TBool
  | TVar of tvar ref
  | TFun of ty * ty
  | TCon of string * ty option

and tvar =
  | Unbound of string * int
  | Link of ty

type scheme = Forall of string list * ty

let counter = ref 0

let fresh_tyvar name =
  let id = !counter in
  incr counter;
  TVar (ref (Unbound (name, id)))
