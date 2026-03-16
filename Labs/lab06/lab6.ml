let rec c n k =
  if k = 0 then
    1
  else if n = 0 then
    0
  else
    (c (n - 1) k) + (c (n - 1) (k - 1)) ;;

type key = Key of int * int ;;
let memyC n k =
  let memo = Hashtbl.create (n * k) in
  let rec memyCing n k =
    let key = Key (n, k) in
    let value = Hashtbl.find_opt memo key in
    match value with
    | Some v -> v
    | None ->
      let result =
        if k = 0 then
          1
        else if n = 0 then
          0
        else
          (memyCing (n - 1) k) + (memyCing (n - 1) (k - 1)) in
      Hashtbl.add memo key result;
      result
  in
  memyCing n k ;;