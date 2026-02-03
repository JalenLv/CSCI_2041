let num = fst ;;
let den = snd ;;
let rec gcd i j =
  if i <> 0 then
    if j > i then
      gcd i (j - i)
    else
      gcd (i - j) j
  else
    j ;;

let rat n d =
  let g = gcd n d in
  (n / g, d / g) ;;

let ratAdd a b =
  rat (num a * den b + den a * num b) (den a * den b) ;;

let ratMul a b =
  rat (num a * num b) (den a * den b) ;;

let ratDiv a b =
  rat (num a * den b) (den a * num b) ;;

let ratGt a b =
  (num a * den b) > (den a * num b) ;;

let rec eulering c t =
  let one = rat 1 1 in
  let zero = rat 0 1 in
  let eps = rat 1 100000 in
  if ratGt t eps then
    ratAdd t (eulering (ratAdd one c) (ratDiv t c))
  else
    zero

let euler () =
  let one = rat 1 1 in
  eulering one one ;;