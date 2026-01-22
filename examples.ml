open Ast

let id =
  Fun ("x", Var "x")

let const =
  Fun ("x", Fun ("y", Var "x"))

let rec_fact =
  LetRec ("fact", "n",
    If (
      App (App (Var "==", Var "n"), Int 0),
      Int 1,
      App (
        App (Var "*", Var "n"),
        App (Var "fact",
          App (App (Var "-", Var "n"), Int 1)
        )
      )
    ),
    Var "fact"
  )

(* NOTE:
   Examples assume a primitive environment.
   They are not directly evaluatable.
*)
