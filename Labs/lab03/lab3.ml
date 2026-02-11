type 'key bst = BstEmpty | BstNode of 'key * 'key bst * 'key bst ;;
exception BadEmptyBst ;;

let rec bstMaxKey tree =
  match tree with
  | BstEmpty -> raise BadEmptyBst
  | BstNode(key, _, right) -> if right = BstEmpty then key else bstMaxKey right

let rec bstDelete tree key =
  match tree with
  | BstEmpty -> BstEmpty
  | BstNode(k, BstEmpty, BstEmpty) -> if key = k then BstEmpty else tree
  | BstNode(k, BstEmpty, right) ->
    if key > k then
      BstNode(k, BstEmpty, bstDelete right key)
    else if key = k then
      right
    else
      tree
  | BstNode(k, left, BstEmpty) ->
    if key < k then
      BstNode(k, bstDelete left key, BstEmpty)
    else if key = k then
      left
    else
      tree
  | BstNode(k, left, right) ->
    if key < k then
      BstNode(k, bstDelete left key, right)
    else if key > k then
      BstNode(k, left, bstDelete right key)
    else
      let max_key = bstMaxKey left in
      BstNode(max_key, bstDelete left max_key, right)