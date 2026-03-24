(* EXPRESSION. Represent an algebraic expression. *)

type expression =
  Var of string |                    (* a, b, c ... *)
  Neg of expression |                (*   ¬ R *)
  Add of expression * expression |   (* L + R *)
  Div of expression * expression |   (* L / R *)
  Equ of expression * expression |   (* L = R *)
  Mul of expression * expression |   (* L × R *)
  Sub of expression * expression ;;  (* L − R *)
