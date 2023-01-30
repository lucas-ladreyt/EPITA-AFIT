(** Generating primes *)

open Builtin
open Basic_arithmetics

(** List composed of 2 and then odd integers starting at 3.
    @param n limit of list of odd integers, minimum value is 2.
 *)
let init_eratosthenes n =
  let rec main m = match m with
      _ when m >= n + 1 -> []
     |_ when m >= 3 && modulo m 2 = 1 -> m::main (m+1)
     |_ when m >= 3 && modulo m 2 = 0 -> main (m+1)
     |_ -> 2::main (m+1)
  in main 2;;

(** Eratosthene sieve.
    @param n limit of list of primes, starting at 2.
 *)
(*
let rec test_eachs a n = match a with
    _ when a>n -> []
   |_ when a*a>n -> a::(main (a+1))
   |_ -> a::(deldivs (main (a+1)) a);;
 *)
let eratosthenes n =
  let rec deldivs list a = match list with
      [] -> []
    |e::l when e mod a = 0 -> deldivs l a
    |e::l -> e::(deldivs l a)
  in let rec main a = match a with
        _ when a>n -> []
      |_ when a*a>n -> a::(main (a+1))
      |_ -> a::(deldivs (main (a+1)) a)
  in main 2;;

(** Write a list into a file. Element seperator is newline.
    @param file path to write to.
 *)
let write_list li file =
  let oc = open_out file
  in
  let rec main = function
      [] -> close_out oc
     |e::l -> Printf.fprintf oc "%d\n" e;
              main l
  in main li;;

(** Write a list of prime numbers up to limit into a txt file.
    @param n limit of prime numbers up to which to build up a list of primes.
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

(** Create a list out of reading a line per line channel.
    @param in_c input channel.
 *)
let create_list in_c =
  let rec _create_list in_c =
    match input_line_opt in_c with
    | Some line -> (int_of_string line)::(_create_list in_c)
    | None -> []
  in
  _create_list in_c

(** Load list of primes into OCaml environment.
    @param file path to load from.
 *)
let read_list_primes file =
  create_list (open_in file);;

(** Get biggest prime.
    @param l list of prime numbers.
 *)
let rec last_element l = match l with
  | [] -> failwith "You're list is empty. "
  | e::[] -> e
  | h::t -> last_element t

(** Get two biggest primes.
    @param l list of prime numbers.
 *)
let rec last_two l = match l with
  | [] | [_] -> failwith "List has to have at least two prime numbers."
  | e::g::[] -> (e, g)
  | h::t -> last_two t

(** Finding couples of primes where second entry is twice the first
    plus 1.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)

let isprime n =
  let rec cpt_div n n2 = match n2 with
      1 -> 1
     |_ when modulo n n2 = 0 -> 1 + (cpt_div n (n2-1))
     |_ -> cpt_div n (n2-1)
  in (cpt_div n n) <= 2;;

let double_primes limit isprime =
  let rec main = function
      [] -> []
     |e::l when isprime (2*e+1) -> (e,(2*e+1))::main l
     |e::l -> main l
  in main (eratosthenes limit);;

(** Finding twin primes.
    @param limit positive integer bounding searched for primes.
    @param isprime function testing for (pseudo)primality.
 *)
let twin_primes limit isprime =
  let rec main = function
      [] -> []
     |e::l when isprime (e+2) && e <> 2 -> (e,(e+2))::main l
     |e::l -> main l
  in main (eratosthenes limit);;
