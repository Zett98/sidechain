open Deku_crypto
open Deku_concepts

(* TODO: probably should be under concepts *)
type identity
type t = identity

val sign : hash:BLAKE2b.t -> identity -> Verified_signature.t
