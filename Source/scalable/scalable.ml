(** A naive implementation of big integers

This module aims at creating a set of big integers naively. Such data
types will be subsequently called bitarrays. A bitarray is a list of
zeros and ones ; first integer representing the sign bit. In this
contexte zero is reprensented by the empty list []. The list is to
be read from left to right ; this is the opposite convention to the
one you usually write binary decompositions with. After the sign bit
the first encountered bit is the coefficient in front of two to
the power zero. This convention has been chosen to ease writing
down code.

 *)

(* uwuwuwuwuwuwuwuwuwuwuwuwuuwuw *)
(* uwuwuwuwuwuTOOLBOXwuwuwuwuwuw *)
(* uwuwuwuwuwuwuwuwuwuwuwuwuuwuw *)
let reverse list = (* used in: from_int *)
  let rec main l1 l2 = match l1 with
    [] -> l2
    |e::l -> main l (e::l2)
  in main list [];;
let rec length list = match list with
    [] -> 0
   |e::li -> 1 + length li;;
let modulo a b = (* used in: power *)
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
let power x n = (* used in: to_int *)
  let rec main x n = match n with
      0 -> 1
     |1 -> x
     |_ when modulo n 2 = 0 -> main (x*x) (n/2)
     |_ -> x * main (x*x) (n/2)
  in main x n;;
let signl list = match list with (* used in: to_int *)
    [] -> 1
   |e::l -> if e = 0 then 1 else -1;;
let shrink list = match list with (* used in: compare_b *)
    [] -> (1,[])
   |e::l -> (e,l);;
let where_last x list = (* used in: comp2 *)
  let rec main list i j = match list with
      [] -> i
     |e::l when e <> x -> main l i (j+1)
     |e::l -> main l j (j+1)
  in main list 0 0;;
let del_zeros list = (* used in: PAS diff_n J'AI PERDU 4h DE MA VIE *)
  let rec main li = match li with
      [] -> []
     |e::l when e <> 0 -> (e::l)
     |e::l -> main l
  in let res = reverse(main (reverse list))
     in if res = [1] || res = [0] then [] else res;;
let del_zeros_n list = (* used in: diff_n *)
  let rec main li = match li with
      [] -> []
     |e::l when e <> 0 -> (e::l)
     |e::l -> main l
  in let res = reverse(main (reverse list))
     in if res = [0] then [] else res;;
(* uwuwuwuwuwuwuwuwuwuwuwuwuuwuw *)
(* uwuwuwuwuwuTOOLBOXwuwuwuwuwuw *)
(* uwuwuwuwuwuwuwuwuwuwuwuwuuwuw *)

(** Creates a bitarray from a built-in integer.
    @param x built-in integer.
 *)
let from_int x =
  if x = 0
  then []
  else
    let rec main x = match x with
        0 -> []
       |_ -> (x mod 2)::(main (x/2))
    in
    if x < 0
    then 1::(main (-x))
    else 0::(main x);;

(** Transforms bitarray of built-in size to built-in integer.
    UNSAFE: possible integer overflow.
    @param bA bitarray object.
 *)

let to_int bA =
  let rec main bA i = match bA with
      [] -> 0
     |e::l when i = (-1) -> main l (i+1)
     |e::l when e <> 0 -> (power 2 i) + (main l (i+1))
     |e::l -> main l (i+1)
  in
  (signl bA) * (main bA (-1));;

(** Prints bitarray as binary number on standard output.
    @param bA a bitarray.
  *)
let print_b bA =
  let rec main = function
      [] -> ()
     |e::l -> main l;
              print_int e;
  in main bA;;

(** Toplevel directive to use print_b as bitarray printer.
    CAREFUL: print_b is then list int printer.
    UNCOMMENT FOR TOPLEVEL USE.
*)
(* #install_printer print_b *)

(** Internal comparisons on bitarrays and naturals. Naturals in this
    context are understood as bitarrays missing a bit sign and thus
    assumed to be non-negative.
*)

(** Comparing naturals. Output is 1 if first argument is bigger than
    second -1 otherwise.
    @param nA A natural, a bitarray having no sign bit.
           Assumed non-negative.
    @param nB A natural.
 *)
let rec compare_n nA nB =
  let rec main list1 list2 msb = match (list1,list2) with
      ([],[]) -> msb
     |([],e2::l2) -> (-1)
     |(e::l1,[]) -> (1)
     |(e1::l1,e2::l2) when e1 > e2 -> main l1 l2 (1)
     |(e1::l1,e2::l2) when e1 < e2 -> main l1 l2 (-1)
     |(e1::l1,e2::l2) -> main l1 l2 msb
  in main nA nB 0;;

(** Bigger inorder comparison operator on naturals. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>!) nA nB =
  compare_n nA nB = 1;;

(** Smaller inorder comparison operator on naturals. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<!) nA nB =
  compare_n nA nB = -1;;

(** Bigger or equal inorder comparison operator on naturals. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>=!) nA nB =
  compare_n nA nB >= 0;;

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<=!) nA nB =
  compare_n nA nB <= 0;;

(** Comparing two bitarrays. Output is 1 if first argument is bigger
    than second -1 otherwise.
    @param bA A bitarray.
    @param bB A bitarray.
*)
let rec compare_b bA bB =
  let (e0A,nA) = shrink bA
  and (e0B,nB) = shrink bB in
  if e0A > e0B
  then -1
  else
    if e0A < e0B
    then 1
    else
      compare_n nA nB * signl bA;;

(** Bigger inorder comparison operator on bitarrays. Returns true if
    first argument is bigger than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>) bA bB =
  compare_b bA bB = 1;;

(** Smaller inorder comparison operator on bitarrays. Returns true if
    first argument is smaller than second and false otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<) bA bB =
  compare_b bA bB = -1;;

(** Bigger or equal inorder comparison operator on bitarrays. Returns
    true if first argument is bigger or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (>>=) bA bB =
  compare_b bA bB >= 0;;

(** Smaller or equal inorder comparison operator on naturals. Returns
    true if first argument is smaller or equal to second and false
    otherwise.
    @param nA natural.
    @param nB natural.
 *)
let (<<=) bA bB =
  compare_b bA bB <= 0;;


(** Sign of a bitarray.
    @param bA Bitarray.
*)
let sign_b bA =
  signl bA;;

(** Absolute value of bitarray.
    @param bA Bitarray.
*)
let abs_b bA =
  let (e0A,nA) = shrink bA
  in nA;;

(** Quotient of integers smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _quot_t a = if a < 2 then 0 else 1

(** Modulo of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _mod_t a = if a = 1 || a = 3 then 1 else 0

(** Division of integer smaller than 4 by 2.
    @param a Built-in integer smaller than 4.
*)
let _div_t a = (_quot_t a, _mod_t a)

(** Addition of two naturals.
    @param nA Natural.
    @param nB Natural.
*)
let add_n nA nB =
  let rec main list1 list2 ret = match (list1,list2) with
      ([],[]) when ret = 0 -> []
     |([],[]) -> [1]
     |(e1::l1,[]) when ret + e1 = 2 -> 0::(main l1 [] 1) (*cas 5.4 bis*)
     |(e1::l1,[]) -> (ret + e1)::(main l1 [] 0) (*cas 5.{1,2,3} bis*)
     |([],e2::l2) when ret + e2 = 2 -> 0::(main [] l2 1) (*cas 5.4*)
     |([],e2::l2) -> (ret + e2)::(main [] l2 0) (*cas 5.{1,2,3}*)
     |(e1::l1,e2::l2) when e1 + e2 = 0 -> ret::(main l1 l2 0) (*cas 1*)
     |(e1::l1,e2::l2) when e1 + e2 = 1 -> (if ret = 0 then 1 else 0)::(main l1 l2 ret) (*cas 2 et 3*)
     |(e1::l1,e2::l2) -> ret::(main l1 l2 1) (*cas 4*)
  in main nA nB 0;;

(** Difference of two naturals.
    UNSAFE: First entry is assumed to be bigger than second.
    @param nA Natural.
    @param nB Natural.
*)
let diff_n nA nB =
  if (<<!) nA nB
  then invalid_arg "UNSAFE: First entry is assumed to be bigger than second."
  else
  let rec main list1 list2 ret = match (list1,list2) with
      ([],[]) -> []
     |(e1::l1,[]) when e1 = 0 -> ret::(main l1 [] ret) (*cas n11*)
     |(e1::l1,[]) -> (e1 - ret)::(main l1 [] 0) (*cas n12*)
     |([],e2::l2) when e2 = 0 -> ret::(main [] l2 ret) (*cas n9*)
     |([],e2::l2) -> (e2 - ret)::(main [] l2 1) (*cas n10*)
     |(e1::l1,e2::l2) when e1 = e2 -> ret::(main l1 l2 ret) (*cas n{1,2,7,8}*)
     |(e1::l1,e2::l2) when e1 - e2 = 1 -> (e1 - ret)::(main l1 l2 0) (*cas n{5,6}*)
     |(e1::l1,e2::l2) -> (e2 - ret)::(main l1 l2 1) (*cas n{3,4}*)
  in del_zeros_n (main nA nB 0);;

(** Addition of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
 *)
let add_b_dirty bA bB = match (bA,bB) with
    (_,[]) -> bA
   |([],_) -> bB
   |(a::la,b::lb) when a = b -> a::(add_n la lb)
   |(a::la,b::lb) when a < b -> if (>>!) la lb     (*a < b => a et -b*)
                                then (a::(diff_n la lb))
                                else (b::(diff_n lb la))
   |(a::la,b::lb) -> if (>>!) la lb
                     then (a::(diff_n la lb))
                     else (b::(diff_n lb la));;
let add_b bA bB = del_zeros (add_b_dirty bA bB);;

(** Difference of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
 *)
let diff_b bA bB =
  let main bA bB = match (bA,bB) with
      (_,[])-> bA
     |([],e::l)-> (if e = 0 then 1 else 0)::l
     |(a::la,b::lb) when a = b -> add_b (a::la) ((if b = 0 then 1 else 0)::lb) (* cas 1 *)
     |(a::la,b::lb) when a < b -> add_b (a::la) ((if b = 0 then 1 else 0)::lb) (* on dirait au'on peut merge les cas mais nan ... *)
     |(a::la,b::lb) -> add_b (a::la) ((if b = 0 then 1 else 0)::lb)
  in del_zeros (main bA bB);;

(*
let diff_b_tmp bA bB = match (bA,bB) with
      (_,[])-> bA
     |([],e::l)-> (if e = 0 then 1 else 0)::l
     |(a0::lat,b0::lbt) when a0 = 0 && b0 = 0 -> (if (>=!) lat lbt then 0 else 1)::diff_n lat lbt
     |(a0::lat,b0::lbt) when a0 = 0 && b0 = 1 -> 0::add_n lat lbt
     |(a0::lat,b0::lbt) when a0 = 1 && b0 = 0 -> 1::add_n lat lbt
     |(a0::lat,b0::lbt) (*when a0 = 1 && b0 = 1*) -> (if (>>!) lat lbt then 1 else 0)::diff_n lbt lat;; *)

(** Shifts bitarray to the left by a given natural number.
    @param bA Bitarray.
    @param d Non-negative integer.
*)
let rec shift bA d = match bA with
    [] -> bA
   |_ when d <= 0 -> bA
   |e::l -> shift (e::0::l) (d-1);;

(** Multiplication of two bitarrays.
    @param bA Bitarray.
    @param bB Bitarray.
 *)
let mult_b bA bB = match (bA,bB) with
  |(_,[])|([],_) -> []
  |(a0::lat,b0::lbt) -> let rec main list1 list2 i = match (list1,list2) with
                            (_,[])|([],_) -> [] (* ABS!!!!!! *)
                            |(laf,b::lb) when b = 1 -> add_b (shift laf i) (main laf lb (i+1))
                            |(laf,b::lb) -> (main laf lb (i+1))
                        in main ((if a0 = b0 then 0 else 1)::lat) lbt 0;;

(** Quotient of two bitarrays.
    @param bA Bitarray you want to divide by second argument.
    @param bB Bitarray you divide by. Non-zero!
 *)
let quot nA nB =
  let rec quot2 list (nA,nB) = match (nA,nB) with
      (_,_) when (<<!) nA nB -> list
     |(_,_) -> quot2 (add_n list (from_int 1)) ((diff_n nA nB),nB)
  in quot2 [] (nA,nB);;

let quot_b bA bB =
  if del_zeros bA = del_zeros bB
  then [0;1]
  else match (bA,bB) with
         ([],_)|(_,[]) -> if del_zeros bB = []
                          then invalid_arg "Division by zero !"
                          else []
         |(a0::lat,b0::lbt) when a0 = 1 -> let test = mult_b (quot (0::lat) (0::lbt)) [(1 - b0);1]
                                           in if (mult_b test bB) = bA
                                              then test
                                              else diff_b test [b0;1]
         |(a0::lat,b0::lbt) -> mult_b (quot (0::lat) (0::lbt)) [b0;1];;


(*
let quot_b bA bB =
  if del_zeros bA = del_zeros bB
  then [0;1]
  else match (bA,bB) with
         ([],_)|(_,[]) -> if del_zeros bB = []
                          then invalid_arg "Division by zero !"
                          else []
         |(a0::lat,b0::lbt) when (<<!) lat lbt -> []
         |(a0::lat,b0::lbt) when lbt = [1] -> (if a0 = b0 then 0 else 1)::lat
         |(a0::lat,b0::lbt) -> let rec main list la =
                                if (<<!) la lbt
                                then list
                                else main (add_n [1] list) (diff_n la lbt)
                               in if (>=!) lat lbt
                                  then (if a0 = b0 then 0 else 1)::(main [] lat)
                                  else
                                    if lat = (mult_b lbt (main [] lat))
                                    then (if a0 = b0 then 0 else 1)::(main [] lat)
                                    else (if a0 = b0 then 0 else 1)::(add_n [1] (main [] lat));;

*)

(** Modulo of a bitarray against a positive one.
    @param bA Bitarray the modulo of which you're computing.
    @param bB Bitarray which is modular base.
 *)
let mod_b bA bB = diff_b bA (mult_b (quot_b bA bB) bB);;

(** Integer division of two bitarrays.
    @param bA Bitarray you want to divide.
    @param bB Bitarray you wnat to divide by.
*)
let div_b bA bB =
  (quot_b bA bB, mod_b bA bB);;
