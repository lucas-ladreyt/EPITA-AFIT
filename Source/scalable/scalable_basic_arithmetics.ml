(** Basic arithmetics for ordered euclidian ring, case of bitarrays. *)


open Scalable


(** Greater common (positive) divisor of two non-zero integers.
    @param bA non-zero bitarray.
    @param bB non-zero bitarray.
 *)

let rec gcd_b bA bB =
  let r = mod_b bA bB in
  if compare_n r [] = 0
  then bB
  else gcd_b bB r;;

(** Extended euclidean division of two integers NOT OCAML DEFAULT.
    Given non-zero entries a b computes triple (u, v, d) such that
    a*u + b*v = d and d is gcd of a and b.
    @param bA non-zero bitarray.
    @param bB non-zero bitarray.
*)
let bezout_b a b =
  let rec main a b u v u1 v1 =
    if b = []
    then (u,v,a)
    else
      let c = quot_b a b
      in main b (diff_b a (mult_b c b)) u1 v1 (diff_b u (mult_b c u1)) (diff_b v (mult_b c v1))
  in main a b [0;1] [] [] [0;1];;
