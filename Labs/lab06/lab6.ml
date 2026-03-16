let rec allbut things thing =
  match things with
  | [] -> []
  | head :: tail ->
      if head = thing then
        tail
      else
        head :: allbut tail thing ;;

let rec choose etc things =
  match things with
  | [] -> ()
  | head :: tail ->
      etc head; choose etc tail ;;

let permute etc things =
  let rec permuting etc permutedThings unpermutedThings =
    if unpermutedThings = [] then
      etc permutedThings
    else
      choose
        (fun thing ->
          permuting etc (thing :: permutedThings) (allbut unpermutedThings thing))
        unpermutedThings
  in
  permuting etc [] things ;;