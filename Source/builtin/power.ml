(** Power function implementations for builtin integers *)

open Builtin
open Basic_arithmetics

(** Naive power function. Linear complexity
    @param x base
    @param n exponent
 *)

let pow x n = match n with
    0 -> 1
   |1 -> x
   |_ when n < 0 -> 0
   |_ -> let rec main x n =
           if n <= 1
           then x
           else x * main x (n-1)
         in main x n;;

(** Fast integer exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
 *)
let power x n =
  let rec main x n = match n with
      0 -> 1
     |1 -> x
     |_ when n < 0 -> 0
     |_ when modulo n 2 = 0 -> main (x*x) (n/2)
     |_ -> x * main (x*x) (n/2)
  in main x n;;

(** Fast modular exponentiation function. Logarithmic complexity.
    @param x base
    @param n exponent
    @param m modular base
 *)

let mod_power x n m =
  let rec main x n m =
    let main2 = match n with
        0 -> 1
       |_ -> let divn2 = main x (n/2) m
             in match modulo n 2 with
                  0 -> modulo (divn2*divn2) m
                 |_ -> modulo ((modulo (divn2*divn2) m)*x) m
    in
    if x < 0 && modulo n 2 = 1
    then m + main2
    else main2
  in modulo (main x n m) m;;

(** Fast modular exponentiation function mod prime. Logarithmic complexity.
    It makes use of the Little Fermat Theorem.
    @param x base
    @param n exponent
    @param p prime modular base
 *)
let prime_mod_power x n p =
  let main x n = match n with
      0 -> 1
     |1 -> modulo x n
     |_ when x = 0 -> 0
     |_ -> let r = (modulo  n (p-1)) in mod_power x r p
  in main x n;;
