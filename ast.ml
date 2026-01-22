type ident = string

type expr =
  | Int of int
  | Bool of bool
  | Var of ident
  | Fun of ident * expr
  | App of expr * expr
  | Let of ident * expr * expr
  | LetRec of ident * ident * expr * expr
  | If of expr * expr * expr
  | Match of expr * (pattern * expr) list

and pattern =
  | PInt of int
  | PBool of bool
  | PVar of ident
  | PConstructor of ident * pattern option
