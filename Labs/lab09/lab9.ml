type thing = 
  Cons of thing * thing |
  Nil | 
  Number of int | 
  Symbol of string ;;

let rec every p e =
  match e with
  | Cons (h, t) ->
    (match p h with
    | true -> every p t
    | false -> false)
  | _ -> true ;;

let rec substitute e o n =
  match e with
  | Cons (h, t) -> Cons ((if h = o then n else h), substitute t o n)
  | _ -> Nil ;;

let rec questyEqual l r =
  match (l, r) with
  | (Cons (lh, lt), Cons (rh, rt)) -> (questyEqual lh rh) && (questyEqual lt rt)
  | (_, _) -> (l = r) || (l = Symbol "?")
