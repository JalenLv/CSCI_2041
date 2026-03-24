type expression =
  Var of string |                    (* a, b, c ... *)
  Neg of expression |                (*   ¬ R *)
  Add of expression * expression |   (* L + R *)
  Div of expression * expression |   (* L / R *)
  Equ of expression * expression |   (* L = R *)
  Mul of expression * expression |   (* L × R *)
  Sub of expression * expression ;;  (* L − R *)

exception SolvingError of string ;;
exception Unreachable

let rec isInside name expression =
  match expression with
  | Var x -> x = name
  | Neg e -> isInside name e
  | Add (e1, e2) | Div (e1, e2) | Equ (e1, e2) | Mul (e1, e2) | Sub (e1, e2) ->
    isInside name e1 || isInside name e2 ;;

let rec solver name left right =
  match left with
  | Var _ -> Equ (left, right)
  | Neg e -> solver name e (Neg right)
  | Add (l, r) ->
    (match (isInside name l, isInside name r) with
    | (true, false) -> solver name l (Sub (right, r))
    | (false, true) -> solver name r (Sub (right, l))
    | (true, true) -> raise (SolvingError "Multiple occurrences of variable in one side of the equation")
    | (false, false) -> raise Unreachable)
  | Div (l, r) ->
    (match (isInside name l, isInside name r) with
    | (true, false) -> solver name l (Mul (right, r))
    | (false, true) -> solver name r (Div (l, right))
    | (true, true) -> raise (SolvingError "Multiple occurrences of variable in one side of the equation")
    | (false, false) -> raise Unreachable)
  | Mul (l, r) ->
    (match (isInside name l, isInside name r) with
    | (true, false) -> solver name l (Div (right, r))
    | (false, true) -> solver name r (Div (right, l))
    | (true, true) -> raise (SolvingError "Multiple occurrences of variable in one side of the equation")
    | (false, false) -> raise Unreachable)
  | Sub (l, r) ->
    (match (isInside name l, isInside name r) with
    | (true, false) -> solver name l (Add (right, r))
    | (false, true) -> solver name r (Sub (l, right))
    | (true, true) -> raise (SolvingError "Multiple occurrences of variable in one side of the equation")
    | (false, false) -> raise Unreachable)
  | Equ (l, r) -> raise (SolvingError "Cannot solve an equation on the left side of another equation")

let solve name equation =
  match equation with
  | Equ (left, right) ->
    (match (isInside name left, isInside name right) with
    | (true, false) -> solver name left right
    | (false, true) -> solver name right left
    | (true, true) -> raise (SolvingError "Variable appears on both sides of the equation")
    | (false, false) -> raise (SolvingError "Variable does not appear in the equation"))
  | _ -> raise (SolvingError "Not an equation") ;;