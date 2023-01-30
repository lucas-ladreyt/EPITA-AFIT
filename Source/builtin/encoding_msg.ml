(** Encoding Strings *)


open Builtin
open Basic_arithmetics
open Power

(** Encode a string containing ASCII characters.
    @param str is a string representing message.
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7.
 *)
let encode str bits =
  let ln = String.length str in
  let rec main i =
    if i = -1
    then 0
    else
      let intNum = int_of_char (str.[i])
      and p2bNum = power 2 (bits*(ln-i-1))
      in intNum * p2bNum + main (i-1)
  in main (ln-1);;

(** Decode a string containing ASCII characters.
    @param msg is an integer representing an encoded message.
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7.
 *)
let decode msg bits =
  let rec main msgg =
    if msgg <= 0
    then ""
    else
      let p2b = power 2 bits in
      let (quotMsg,modMsg) = div msgg p2b
      in (main quotMsg) ^
           Char.escaped (char_of_int (modMsg)) (*Convert int to string*)
  in main msg;;

