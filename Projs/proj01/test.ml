open Printf ;;

let rec string_of_expression expression =
  match expression with
  | Var x -> sprintf "Var(\"%s\")" x
  | Neg e -> sprintf "Neg(%s)" (string_of_expression e)
  | Add (l, r) -> sprintf "Add(%s, %s)" (string_of_expression l) (string_of_expression r)
  | Div (l, r) -> sprintf "Div(%s, %s)" (string_of_expression l) (string_of_expression r)
  | Equ (l, r) -> sprintf "Equ(%s, %s)" (string_of_expression l) (string_of_expression r)
  | Mul (l, r) -> sprintf "Mul(%s, %s)" (string_of_expression l) (string_of_expression r)
  | Sub (l, r) -> sprintf "Sub(%s, %s)" (string_of_expression l) (string_of_expression r) ;;

let passed = ref 0 ;;
let failed = ref 0 ;;

let pass name =
  passed := !passed + 1 ;
  printf "[PASS] %s\n" name ;;

let fail name message =
  failed := !failed + 1 ;
  printf "[FAIL] %s: %s\n" name message ;;

let expect_bool name expected actual =
  if expected = actual
  then pass name
  else fail name (sprintf "expected %b but got %b" expected actual) ;;

let expect_expr name expected actual =
  if expected = actual
  then pass name
  else
    fail name
      (sprintf
         "expected %s but got %s"
         (string_of_expression expected)
         (string_of_expression actual)) ;;

let expect_solving_error name thunk =
  try
    let _ = thunk () in
    fail name "expected SolvingError, but no exception was raised"
  with
  | SolvingError _ -> pass name
  | exn -> fail name (sprintf "expected SolvingError, got %s" (Printexc.to_string exn)) ;;

let expect_unreachable name thunk =
  try
    let _ = thunk () in
    fail name "expected Unreachable, but no exception was raised"
  with
  | Unreachable -> pass name
  | exn -> fail name (sprintf "expected Unreachable, got %s" (Printexc.to_string exn)) ;;

let () =
  (* isInside tests *)
  expect_bool "isInside_same_var" true (isInside "u" (Var "u")) ;
  expect_bool "isInside_different_var" false (isInside "u" (Var "v")) ;
  expect_bool "isInside_nested" true (isInside "u" (Add (Mul (Var "a", Var "u"), Var "b"))) ;
  expect_bool "isInside_absent_nested" false (isInside "u" (Sub (Var "a", Mul (Var "b", Var "c")))) ;

  (* solver contract tests from project requirements *)
  expect_expr
    "solver_var_alone_returns_equation"
    (Equ (Var "x", Var "c"))
    (solver "x" (Var "x") (Var "c")) ;

  expect_expr
    "solver_add_rule"
    (Equ (Var "x", Sub (Var "c", Var "b")))
    (solver "x" (Add (Var "x", Var "b")) (Var "c")) ;

  expect_expr
    "solver_sub_rule_right_contains_var"
    (Equ (Var "x", Sub (Var "a", Var "c")))
    (solver "x" (Sub (Var "a", Var "x")) (Var "c")) ;

  expect_expr
    "solver_mul_rule"
    (Equ (Var "x", Div (Var "c", Var "b")))
    (solver "x" (Mul (Var "x", Var "b")) (Var "c")) ;

  expect_expr
    "solver_div_rule_denominator_contains_var"
    (Equ (Var "x", Div (Var "a", Var "c")))
    (solver "x" (Div (Var "a", Var "x")) (Var "c")) ;

  expect_expr
    "solver_neg_rule"
    (Equ (Var "x", Neg (Var "c")))
    (solver "x" (Neg (Var "x")) (Var "c")) ;

  expect_solving_error
    "solver_multiple_occurrences_same_side"
    (fun () -> solver "x" (Add (Var "x", Var "x")) (Var "c")) ;

  expect_solving_error
    "solver_multiple_occurrences_div"
    (fun () -> solver "x" (Div (Var "x", Var "x")) (Var "c")) ;

  expect_solving_error
    "solver_multiple_occurrences_mul"
    (fun () -> solver "x" (Mul (Var "x", Var "x")) (Var "c")) ;

  expect_solving_error
    "solver_multiple_occurrences_sub"
    (fun () -> solver "x" (Sub (Var "x", Var "x")) (Var "c")) ;

  expect_solving_error
    "solver_left_is_equation"
    (fun () -> solver "x" (Equ (Var "x", Var "a")) (Var "c")) ;

  expect_unreachable
    "solver_precondition_violation_no_variable_inside"
    (fun () -> solver "x" (Add (Var "a", Var "b")) (Var "c")) ;

  (* solve tests from project requirements *)
  expect_expr
    "solve_example_from_spec"
    (Equ (Var "u", Div (Sub (Var "v", Var "b"), Var "a")))
    (solve "u" (Equ (Add (Mul (Var "a", Var "u"), Var "b"), Var "v"))) ;

  expect_expr
    "solve_variable_on_right_side"
    (Equ (Var "u", Div (Sub (Var "v", Var "b"), Var "a")))
    (solve "u" (Equ (Var "v", Add (Mul (Var "a", Var "u"), Var "b")))) ;

  expect_solving_error
    "solve_not_equation"
    (fun () -> solve "u" (Add (Var "a", Var "b"))) ;

  expect_solving_error
    "solve_variable_both_sides"
    (fun () -> solve "u" (Equ (Add (Var "u", Var "a"), Sub (Var "u", Var "b")))) ;

  expect_solving_error
    "solve_variable_not_present"
    (fun () -> solve "u" (Equ (Add (Var "a", Var "b"), Var "c"))) ;

  expect_solving_error
    "solve_variable_both_sides_simple"
    (fun () -> solve "u" (Equ (Var "u", Var "u"))) ;

  printf "\nSummary: %d passed, %d failed\n" !passed !failed ;
  if !failed > 0 then exit 1 else exit 0 ;;
