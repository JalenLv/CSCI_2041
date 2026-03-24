open Lazy ;;

type 's lazyList =
  | LazyEmpty
  | LazyNode of 's Lazy.t * 's lazyList Lazy.t ;;

exception LazyListError ;;

let lazyCons h t = LazyNode (h, t) ;;

let lazyHead l =
  match l with
  | LazyEmpty -> raise LazyListError
  | LazyNode (h, _) -> force h ;;

let lazyTail l =
  match l with
  | LazyEmpty -> raise LazyListError
  | LazyNode (_, l) -> force l ;;

let rec lazyTake l n =
  if n > 0 then
    lazyHead l :: lazyTake (lazyTail l) (n - 1)
  else
    [] ;;