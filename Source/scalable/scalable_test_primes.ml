(** Testing for primality *)

open Scalable
open Scalable_basic_arithmetics
open Scalable_power


(** Deterministic primality test *)
let is_prime n = true

(** Pseudo-primality test based on Fermat's Little Theorem
    @param p tested bitarray
    @param testSeq sequence of bitarrays againt which to test
 *)
let is_pseudo_prime p test_seq =
  if (<<) p [0;0;1] then false else
  let rec main test_seq = match test_seq with
      [] -> true
     |e::l -> if mod_power e p p = e
              then main l
              else
                if mod_power e p p = mod_b e p
                then main l
                else false
  in main test_seq;;
