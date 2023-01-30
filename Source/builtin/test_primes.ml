(** Testing for primality *)

open Builtin
open Basic_arithmetics
open Power

(** Deterministic primality test *)
let is_prime n =
  let rec cpt_div n n2 = match n2 with
      1 -> 1
     |_ when modulo n n2 = 0 -> 1 + (cpt_div n (n2-1))
     |_ -> cpt_div n (n2-1)
  in (cpt_div n n) <= 2;;

(** Primality test based on small Fermat theorem
    @param p tested integer
    @param testSeq sequence of integers against which to test
 *)
let is_pseudo_prime p test_seq =
  let rec main test_seq = match test_seq with
      [] -> true
     |e::l -> if mod_power e p p = e
              then main l
              else
                if mod_power e p p = modulo e p
                then main l
                else false
  in main test_seq;;
