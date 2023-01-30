(** Factoring Builtin int primes *)


open Builtin
open Basic_arithmetics

(** Factors product of two primes.
    @param key is public key of an RSA cryptosystem.
 *)

let break key =
  let (n,_) = key
  in
  let rec main e =
    if e > n
    then (0,0)
    else
      if modulo n e = 0
      then (e, (quot n e))
      else main (e+1)
  in main 2;;
