(** Generating prime bitarrays *)

(*
#use "scalable.ml";;
#use "scalable_basic_arithmetics.ml";;
*)

open Scalable
open Scalable_basic_arithmetics


(** List composed of 2 and then odd bitarrays starting at 3.
    @param n upper bound to elements in the list of bitarrays.
 *)
let init_eratosthenes n =
  let rec main m = match m with
      _ when (>>=) m (add_b n [0;1]) -> []
     |_ when (>>=) m [0;1;1] && mod_b m [0;0;1] = [0;1] -> m::main (add_b m [0;1])
     |_ when (>>=) m [0;1;1] && mod_b m [0;0;1] = [] -> main (add_b m [0;1])
     |_ -> [0;0;1]::main (add_b m [0;1])
  in main [0;0;1];;

(** Eratosthene sieve.
    @param n upper bound to elements in the list of primes, starting
           at 2.
*)
let eratosthenes n =
  let rec deldivs list a = match list with
      [] -> []
     |e::l when mod_b e a = [] -> deldivs l a
     |e::l -> e::(deldivs l a)
  in let rec main a = match a with
         _ when (>>) a n -> []
        |_ when (>>) (mult_b a a) n -> a::(main (add_b a [0;1]))
        |_ -> a::(deldivs (main (add_b a [0;1])) a)
     in main [0;0;1];;

(** Write a list into a file. Element seperator is newline. Inner
   seperator within elements of an element is ','.
   @param file path to write to.
*)
let write_list li file =
  let oc = open_out file
  in
  let rec main = function
      [] -> close_out oc
     |e::l -> List.iter (Printf.fprintf oc "%d\n") e;
              main l
  in main li;;

(** Write a list of prime numbers up to limit into a txt file.
    @param n limit of prime bitarrays up to which to build up a list of primes.
    @param file path to write to.
*)
let write_list_primes n file =
  write_list (eratosthenes n) file;;

(** Read file safely ; catch End_of_file exception.
    @param in_c input channel.
 *)
let input_line_opt in_c =
  try Some (input_line in_c)
  with End_of_file -> None

(** Create a list of bitarrays out of reading a line per line channel.
    @param in_c input channel.  *)
let create_list in_c =
  let rec _create_list in_c =
    match input_line_opt in_c with
    | Some line -> let rec workaround in_c = match input_line_opt in_c with
                     |Some line -> (int_of_string line)::(workaround in_c)
                     |None -> []
                   in (workaround in_c)::_create_list in_c
    | None -> []
  in
  _create_list in_c

(** Load list of prime bitarrays into OCaml environment.
    @param file path to load from.
 *)
let read_list_primes file =
  create_list (open_in file);;

(** Get last element of a list.
    @param l list of prime bitarrays.
 *)
let rec last_element l = match l with
  | [] -> failwith "Scalable.generate_primes.last_element: Youre list \
                    is empty. "
  | e::[] -> e
  | h::t -> last_element t

(** Get two last elements.
    @param l list of prime bitarrays.
 *)
let rec last_two l = match l with
  | [] | [_] -> failwith "Scalable.generate_primes.last_two: List has \
                          to have at least two elements."
  | e::g::[] -> (e, g)
  | h::t -> last_two t

(** Finding couples of prime bitarrays where second entry is twice the
    first plus 1.
    @param upper bound for searched for prime bitarrays, a built-in integer.
    @param isprime function testing for (pseudo)primality.  *)

let isprime n =
  let rec cpt_div n n2 = match n2 with
      [0;1] -> [0;1]
     |_ when mod_b n n2 = [] -> add_b [0;1] (cpt_div n (diff_b n2 [0;1]))
     |_ -> cpt_div n (diff_b n2 [0;1])
  in (<<=) (cpt_div n n) [0;0;1];;

let double_primes limit isprime =
  let rec main = function
      [] -> []
     |e::l when isprime (mult_b [0;0;1] (add_b e [0;1])) -> (e,(mult_b [0;0;1] (add_b e [0;1])))::main l
     |e::l -> main l
  in main (eratosthenes limit);;

(** Finding twin primes.
    @param upper bound for searched for prime bitarrays, a built-in integer..
    @param isprime function testing for (pseudo)primality.
 *)
let twin_primes limit isprime =
  let rec main = function
      [] -> []
     |e::l when isprime (add_b e [0;0;1]) && e <> [0;0;1] -> (e,(add_b e [0;0;1]))::main l
     |e::l -> main l
  in main (eratosthenes limit);;
