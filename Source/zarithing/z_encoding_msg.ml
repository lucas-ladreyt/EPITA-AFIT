(** Encoding Strings *)

open Z
open Z_power

(** Encode a string containing ASCII characters.
    @param str is a string representing message.
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7.
 *)
let encode str bits = zero
  (*
  let ln = String.length str in
  let rec main i =
    if i < zero
    then zero
    else
      let intNum = int_of_char (str.[(to_int i)])
      and p2bNum = pow (of_int 2) (bits*(ln-(to_int i)-1))
      in intNum * p2bNum + main (i-one)
  in main (ln-one);; *)

(** Decode a string containing ASCII characters.
    @param msg is an integer representing an encoded message.
    @param bits number of bits on which to store a character ; alphanumeric ASCII is 7.
 *)
let decode msg bits = ""
                    (*
  let rec main msgg =
    if msgg <= zero
    then ""
    else
      let p2b = pow (of_int 2) bits in
      let (quotMsg,modMsg) = ediv msgg p2b
      in (main quotMsg) ^
           Char.escaped (char_of_int (of_int modMsg))
  in main msg;;
                     *)
