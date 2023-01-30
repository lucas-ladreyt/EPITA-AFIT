(** Ciphers
    Builtin integer based ciphers.
*)

open Builtin
open Basic_arithmetics
open Power

(********** Cesar Cipher **********)

(** Cesar's cipher encryption
    @param k is an integer corresponding to key
    @param m word to cipher.
    @param b base ; for ASCII codes should be set to 256.
 *)
let encrypt_cesar k m b =
  let rec main = function
      [] -> []
     |e::l when (e+k)>b || (e+k)<b -> (modulo (e+k) b)::(main l)
     |e::l -> (e+k)::(main l)
  in main m;;

(** Cesar's cipher decryption
    @param k is an integer corresponding to key
    @param m encrypted word.
    @param b base ; for ASCII code should be set to 256.
 *)
let decrypt_cesar k m b =
  let rec main = function
      [] -> []
     |e::l when (e-k)>b || (e-k)<b -> (modulo (e-k) b)::(main l)
     |e::l -> (e-k)::(main l)
  in main m;;


(********** RSA Cipher **********)

(** Generate an RSA ciphering keys.
    Involved prime numbers need to be distinct. Output is a couple
    of public, private keys.
    @param p prime number
    @param q prime number
*)
let generate_keys_rsa p q =
  let phi = (p-1)*(q-1)
  in
  let rec genpub x =
    if (gcd x phi = 1 && x > 1)
    then x
    else genpub (x+1)
  in
  let e = genpub (Random.int phi)
  in
  let rec genpriv x =
    if (modulo (e*x) phi) = 1 (* Inverse modulaire *)
    then x
    else genpriv (x+1)
  in
  let d = genpriv (Random.int phi)
  and n = p*q
  in ((n,e),(n,d));;

(** Encryption using RSA cryptosystem.
    @param m integer hash of message
    @param pub_key a tuple (n, e) composing public key of RSA cryptosystem.
 *)
let encrypt_rsa m (n, e) =
  mod_power m e n;;

(** Decryption using RSA cryptosystem.
    @param m integer hash of encrypter message.
    @param pub_key a tuple (n, d) composing private key of RSA cryptosystem.
 *)
let decrypt_rsa m (n , d) =
  mod_power m d n;;


(********** ElGamal Cipher **********)

(** Generate ElGamal public data. Generates a couple (g, p)
    where p is prime and g primitive root in F_p.
    @param p is prime having form 2*q + 1 for prime q.
 *)
let rec public_data_g p =
  let g = (quot (p-1) 2)
  in (p, g);;

(** Generate ElGamal public and private keys.
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let generate_keys_g (g, p) =
  let a = Random.int(p)
  in
  let kA = prime_mod_power g a p
  in (kA, a);;

(** ElGamal encryption process.
    @param msg message to be encrypted.
    @param pub_data a tuple (g, p) of ElGamal public data.
    @param kA ElGamal public key.
 *)
let encrypt_g msg (g, p) kA =
  let r = Random.int(p)
  in
  let msgA = prime_mod_power g r p
  and msgB = modulo (msg * (prime_mod_power kA r p)) p
  in (msgA, msgB);;

(** ElGamal decryption process.
    @param msg a tuple (msgA, msgB) forming an encrypted ElGamal message.
    @param a private key
    @param pub_data a tuple (g, p) of public data for ElGamal cryptosystem.
 *)
let decrypt_g (msgA, msgB) a (g, p) =
   modulo (quot msgB (prime_mod_power msgA a p)) p;;
