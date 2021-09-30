open Helpers;
open Mirage_crypto_ec;

let blake2b_20_encoding: Data_encoding.t(BLAKE2B_20.t);
module type S = {
  module Pub: {
    type t;
    let encoding: Data_encoding.t(t);
    let to_string: (~alphabet: Base58.Alphabet.t=?, t) => string;
    let of_string: (~alphabet: Base58.Alphabet.t=?, string) => option(t);
  };
  module Hash: {
    type t;
    let hash_key: Pub.t => t;
    let encoding: Data_encoding.t(t);
    let to_string: (~alphabet: Base58.Alphabet.t=?, t) => string;
    let of_string: (~alphabet: Base58.Alphabet.t=?, string) => option(t);
  };
  module Secret: {
    type t;
    let to_string: (~alphabet: Base58.Alphabet.t=?, t) => string;
    let of_string: (~alphabet: Base58.Alphabet.t=?, string) => option(t);
  };
  module Signature: {
    type t;
    let sign: (Secret.t, t) => t;
    let check: (Pub.t, t, t) => bool;
    let to_string: (~alphabet: Base58.Alphabet.t=?, t) => t;
    let of_string: (~alphabet: Base58.Alphabet.t=?, t) => option(t);
  };
};
module Ed25519:
  S with
    type Pub.t = Ed25519.pub_ and
    type Hash.t = BLAKE2B_20.t and
    type Secret.t = Ed25519.priv and
    type Signature.t = string;
module P256:
  S with
    type Pub.t = P256.Dsa.pub_ and
    type Hash.t = BLAKE2B_20.t and
    type Secret.t = P256.Dsa.priv and
    type Signature.t = string;