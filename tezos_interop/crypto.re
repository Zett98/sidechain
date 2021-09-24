open Helpers;
open Mirage_crypto_ec;

let blake2b_20_encoding =
  Data_encoding.(
    conv(
      hash => BLAKE2B_20.to_raw_string(hash) |> Bytes.of_string,
      // TODO: I don't like this exception below
      bytes =>
        Bytes.to_string(bytes) |> BLAKE2B_20.of_raw_string |> Option.get,
      Fixed.bytes(20),
    )
  );
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
module Ed25519 = {
  open Ed25519;
  module Pub = {
    type t = pub_;

    let size = 32;
    let prefix = Base58.Prefix.ed25519_public_key;
    let encoding = {
      // TODO: in tezos this is splitted json is not same as binary
      let to_bytes = t => pub_to_cstruct(t) |> Cstruct.to_bytes;
      let of_bytes_exn = b =>
        Cstruct.of_bytes(b) |> pub_of_cstruct |> Result.get_ok;
      Data_encoding.(conv(to_bytes, of_bytes_exn, Fixed.bytes(size)));
    };

    let to_raw = t => Cstruct.to_string(Ed25519.pub_to_cstruct(t));
    let of_raw = string =>
      Ed25519.pub_of_cstruct(Cstruct.of_string(string)) |> Result.to_option;
    let to_string = Base58.simple_encode(~prefix, ~to_raw);
    let of_string = Base58.simple_decode(~prefix, ~of_raw);
  };

  module Hash = {
    type t = BLAKE2B_20.t;

    let hash_key = t =>
      BLAKE2B_20.hash(Ed25519.pub_to_cstruct(t) |> Cstruct.to_string);

    let encoding = {
      let name = "Ed25519.Public_key_hash";
      // TODO: in tezos this is splitted json is not same as bin
      Data_encoding.(obj1(req(name, blake2b_20_encoding)));
    };

    let prefix = Base58.Prefix.ed25519_public_key_hash;
    let to_raw = BLAKE2B_20.to_raw_string;
    let of_raw = BLAKE2B_20.of_raw_string;
    let to_string = Base58.simple_encode(~prefix, ~to_raw);
    let of_string = Base58.simple_decode(~prefix, ~of_raw);
  };

  module Secret = {
    type t = priv;

    let _size = 32;
    let prefix = Base58.Prefix.ed25519_seed;
    let to_raw = t => Cstruct.to_string(Ed25519.priv_to_cstruct(t));
    let of_raw = string =>
      Ed25519.priv_of_cstruct(Cstruct.of_string(string)) |> Result.to_option;

    let to_string = Base58.simple_encode(~prefix, ~to_raw);
    let of_string = Base58.simple_decode(~prefix, ~of_raw);
  };
  module Signature = {
    type t = string;
    let sign = (secret, message) => {
      // double hash because tezos always uses blake2b on CHECK_SIGNATURE
      let hash = BLAKE2B.hash(message);
      Cstruct.of_string(BLAKE2B.to_raw_string(hash))
      // TODO: isn't this double hashing? Seems weird
      |> Ed25519.sign(~key=secret)
      |> Cstruct.to_string;
    };
    let check = (public, signature, message) => {
      let hash = BLAKE2B.hash(message);
      verify(
        ~key=public,
        ~msg=Cstruct.of_string(BLAKE2B.to_raw_string(hash)),
        Cstruct.of_string(signature),
      );
    };

    let size = 64;
    let prefix = Base58.Prefix.ed25519_signature;
    let to_raw = Fun.id;
    let of_raw = string =>
      String.length(string) == size ? Some(string) : None;
    let to_string = Base58.simple_encode(~prefix, ~to_raw);
    let of_string = Base58.simple_decode(~prefix, ~of_raw);
  };
};
module P256 = {
  module Dsa = P256.Dsa;

  open Dsa;
  module Pub = {
    type t = pub_;

    let size = 33;
    let prefix = Base58.Prefix.p256_public_key_hash;
    let encoding = {
      let to_bytes = t => pub_to_cstruct(t) |> Cstruct.to_bytes;
      let of_bytes_exn = b =>
        Cstruct.of_bytes(b) |> pub_of_cstruct |> Result.get_ok;
      Data_encoding.(conv(to_bytes, of_bytes_exn, Fixed.bytes(size)));
    };

    let to_raw = t => Cstruct.to_string(P256.Dsa.pub_to_cstruct(t));
    let of_raw = string =>
      Dsa.pub_of_cstruct(Cstruct.of_string(string)) |> Result.to_option;
    let to_string = Base58.simple_encode(~prefix, ~to_raw);
    let of_string = Base58.simple_decode(~prefix, ~of_raw);
  };
  module Hash = {
    type t = BLAKE2B_20.t;

    let hash_key = t =>
      BLAKE2B_20.hash(P256.Dsa.pub_to_cstruct(t) |> Cstruct.to_string);

    let encoding = {
      let name = "P256.Public_key_hash";
      Data_encoding.(obj1(req(name, blake2b_20_encoding)));
    };

    let prefix = Base58.Prefix.ed25519_public_key_hash;
    let to_raw = BLAKE2B_20.to_raw_string;
    let of_raw = BLAKE2B_20.of_raw_string;
    let to_string = Base58.simple_encode(~prefix, ~to_raw);
    let of_string = Base58.simple_decode(~prefix, ~of_raw);
  };

  module Secret = {
    type t = priv;

    let _size = 32;
    let prefix = Base58.Prefix.p256_secret_key;
    let to_raw = t => Cstruct.to_string(Dsa.priv_to_cstruct(t));
    let of_raw = string =>
      Dsa.priv_of_cstruct(Cstruct.of_string(string)) |> Result.to_option;

    let to_string = Base58.simple_encode(~prefix, ~to_raw);
    let of_string = Base58.simple_decode(~prefix, ~of_raw);
  };
  module Signature = {
    type t = string;
    let sign = (secret, message) => {
      let hash = BLAKE2B.hash(message);
      Cstruct.of_string(BLAKE2B.to_raw_string(hash))
      |> sign(~key=secret)
      |> (((r, s)) => Cstruct.to_string(r) ++ Cstruct.to_string(s));
    };
    let check = (public, signature, message) => {
      let hash = BLAKE2B.hash(message);
      let (s, r) = (
        String.sub(signature, 0, 32),
        String.sub(signature, 32, 32),
      );
      verify(
        ~key=public,
        (Cstruct.of_string(r), Cstruct.of_string(s)),
        Cstruct.of_string(BLAKE2B.to_raw_string(hash)),
      );
    };

    let size = 64;
    let prefix = Base58.Prefix.p256_signature;
    let to_raw = Fun.id;
    let of_raw = string =>
      String.length(string) == size ? Some(string) : None;
    let to_string = Base58.simple_encode(~prefix, ~to_raw);
    let of_string = Base58.simple_decode(~prefix, ~of_raw);
  };
};