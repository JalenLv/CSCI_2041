open Printf ;;

type thing =
  | Closure of thing * thing * environment
  | Cons of thing * thing
  | Nil
  | Number of int
  | Primitive of (thing -> environment -> thing)
  | Symbol of string
and
  environment = (string * thing) list ;;

let rec printingThing thing =
  match thing with
  | Closure (_, _, _) -> printf "[Closure]"
  | Cons (_, _) -> printingThings thing
  | Nil -> printf "nil"
  | Number what -> printf "%i" what
  | Primitive _ -> printf "[Primitive]"
  | Symbol what -> printf "%s" what

and printingThings things =
  let rec printingRest things =
    match things with
    | Nil -> ()
    | Cons (first, rest) ->
        printf " ";
        printingThing first;
        printingRest rest
    | _ -> ()
  in
  printf "(";
  match things with
  | Nil -> ()
  | Cons (first, rest) ->
      printingThing first;
      printingRest rest
  | _ -> ();
  printf ")"

let printThing thing =
  printingThing thing;
  printf "\n"
