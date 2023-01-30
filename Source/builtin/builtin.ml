(** Tweaking OCaml builtin euclidean division

The OCaml built-in euclidian divisions operations do not follow the
standard mathematical conventions. We adapt OCaml available
primitives to suit maths conventions.

 **)

(** Sign function
    @param x integer
*)
let sign x =
  if x < 0
  then -1
  else 1;;

(** Quotient of an integer by a natural number.
    This is the quotient in euclidiant division sense.
    @param a dividend
    @param b natural number you divide by.
 *)
let quot a b =
  if a mod b < 0
  then (a/b) - 1
  else a/b ;;

  (*
  if a > 0 && b > 0 then a/b
  else
  let rec main u v q = match sign u with
      (-1) -> if u > v
              then q
              else main (u+v) v (q - 1)
     |_ -> if u < v
           then q
           else main (u-v) v (q + 1)
  in main a b 0;; *)

(** Quotient of two integers. Fully Recursive.
    General case ; explicit by-hand computations. Not efficient enough as
    it is not a low level implementation.
*)

(** Modulo of two integers.
    Following euclidean division NOT OCAML DEFAULT. Positive integer
    between 0 (included) and modulo (excluded) resulting from euclidian
    division of entry by modulo.

    OCAML DEFAULT : For negative numbers eucldean result - modulo base.

    @param a input integer
    @param b moduli integer.
 *)
let modulo a b =
  if a mod b == 0
  then 0
  else
    if a > 0
    then a mod b
    else
      if b <= 0
      then a mod b - b
      else
        a mod b + b;;

(** Division of an integer by a natural number. NOT OCAML DEFAULT.
    Division of an integer by a non-zero integer b is the unique couple
    of integers (q, r) such that a = b*q + r and r is in [0, abs b[.
    @param a dividend
    @param b integer you divide by.
*)
let div a b =
  ((quot a b), (modulo a b));;
