(** Power function implementations for bitarrays *)


open Scalable
open Scalable_basic_arithmetics

 

(** Naive power function. Linear complexity
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
 *)
let pow x n = 
  if (<<) n [] then [] else
  match n with
    [] -> [0;1]
   |[0;1] -> x
   |_ -> let rec main x n =
           if (<<=) n [0;1]
           then x
           else mult_b x (main x (diff_b n [0;1]))
         in main x n;;

(** Fast bitarray exponentiation function. Logarithmic complexity.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
 *)
let power x n =
  if (<<) n [] then [] else
  let rec main x n = match n with
      [] -> [0;1]
     |[0;1] -> x
     |_ when mod_b n [0; 0; 1] = [] -> main (mult_b x x) (quot_b n [0; 0; 1])
     |_ -> mult_b x (main (mult_b x x) (quot_b n [0; 0; 1]))
  in main x n;;
(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
    @param m modular base, a positive bitarray
 *)
let mod_power x n m =
  let rec main x n m =
    let main2 = match n with
        [] -> [0;1]
       |_ -> let divn2 = main x (quot_b n [0;0;1]) m
             in match mod_b n [0;0;1] with
                  [] -> mod_b (mult_b divn2 divn2) m
                 |_ -> mod_b (mult_b (mod_b (mult_b divn2 divn2) m) x) m
    in
    if sign_b x < 0 && mod_b n [0;0;1] = [0;1]
    then add_b m main2
    else main2
  in mod_b (main x n m) m;;

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base, a bitarray
    @param n exponent, a non-negative bitarray
    @param p prime modular base, a positive bitarray
 *)
let prime_mod_power x n p =
  let main x n = match n with
      [] -> [0;1]
     |[0;1] -> mod_b x n
     |_ when x = [] -> []
     |_ -> let r = (mod_b n (diff_b p [0;1])) in mod_power x r p
  in main x n;;
