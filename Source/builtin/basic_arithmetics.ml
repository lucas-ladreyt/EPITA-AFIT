(** Basic arithmetics with builtin integers *)

open Builtin

(** Greater common (positive) divisor of two non-zero integers.
    @param a non-zero integer
    @param b non-zero integer
 *)

let rec gcd a b =
  let r = modulo a b in
  if r = 0
  then b
  else gcd b r;;

(** Extended euclidean division of two integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param a non-zero integer
    @param b non-zero integer.
*)
let bezout a b =
  let rec main a b u v u1 v1 =
    if b = 0
    then (u,v,a)
    else
      let c = quot a b
      in main b (a - c*b) u1 v1 (u - c*u1) (v - c*v1)
  in main a b 1 0 0 1;;
