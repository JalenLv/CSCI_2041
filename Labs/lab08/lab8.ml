module type Associaty = sig
  type ('k, 'v) t = Empty | Pair of 'k * 'v * ('k, 'v) t

  exception NoSuchKey

  val make : unit -> ('k, 'v) t
  val delete : ('k, 'v) t -> 'k -> ('k, 'v) t
  val get : ('k, 'v) t -> 'k -> 'v
  val put : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
end

module Association : Associaty = struct
  type ('k, 'v) t = Empty | Pair of 'k * 'v * ('k, 'v) t

  exception NoSuchKey

  let error () = raise NoSuchKey
  let make () = Empty

  let rec delete pairs key =
    match pairs with
    | Empty -> error ()
    | Pair (k, v, tail) -> if k = key then tail else Pair (k, v, delete tail key)

  let rec get pairs key =
    match pairs with
    | Empty -> error ()
    | Pair (k, v, tail) -> if k = key then v else get tail key

  let put key value pairs = Pair (key, value, pairs)
end
