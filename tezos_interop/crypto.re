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
  module Compresss = {
    let compress = t => {
      let res = Cstruct.create(33);
      let b = Dsa.pub_to_cstruct(t);
      let ident = 2 + Cstruct.get_uint8(b, 63) land 1;
      Cstruct.blit(b, 1, res, 1, 32);
      Cstruct.set_uint8(res, 0, ident);
      Cstruct.to_string(res);
    };

    let modulo = (x, y) => {
      open Z;
      let result = x mod y;
      if (geq(result, zero)) {
        result;
      } else {
        result + y;
      };
    };
    let tonelli = (n, p) => {
      open Z;
      let (q, s) = (p - one, zero);
      let rec go = (q, s) =>
        if (equal(modulo(q, of_int(2)), zero)) {
          go(q /< of_int(2), succ(s));
        } else {
          (q, s);
        };
      let (q, s) = go(q, s);
      let rec go = z =>
        if (!
              equal(
                powm(z, (p - one) /< of_int(2), p),
                modulo(neg(one), p),
              )) {
          go(z + one);
        } else {
          z;
        };
      let z = go(of_int(2));
      let (m, c, t, r) = (
        s,
        powm(z, q, p),
        powm(n, q, p),
        powm(n, (q + one) /< of_int(2), p),
      );
      let rec go = (m, c, t, r) => {
        let rec go' = (i, m') =>
          if (equal(powm(t, i ** 2, p), one)) {
            i;
          } else {
            go'(succ(i), m');
          };
        if (!equal(t, one)) {
          let i = go'(one, m);
          let b = powm(c, of_int(2) ** to_int(m - i - one), p);
          go(
            i,
            powm_sec(b, of_int(2), p),
            modulo(powm(b, b, p), p),
            modulo(r * b, p),
          );
        } else {
          (m, c, t, r);
        };
      };
      let (_, _, _, r) = go(m, c, t, r);
      r;
    };
    let sqrt_mod = (n, p) => {
      Z.(
        if (equal(modulo(p, of_int(4)), of_int(3))) {
          let k = p - of_int(3) /< of_int(4);
          let x = powm(n, succ(k), p);
          x;
        } else {
          tonelli(n, p);
        }
      );
    };


    let p =
      Z.of_string(
        "115792089210356248762697446949407573530086143415290314195533631308867097853951",
      );

    let b =
      Z.of_string_base(
        16,
        "5AC635D8AA3A93E7B3EBBD55769886BC651D06B0CC53B0F63BCE3C3E27D2604B",
      );

    let decompress = string => {
      let buf = Cstruct.of_string(string);      
      let res = Cstruct.create(65);
      let y_bit = Cstruct.get_uint8(buf, 0);
      let x = Cstruct.sub(buf, 1, 32) |> Cstruct.to_string |> Z.of_bits; // this is wildly incorrent
      let.some y = {
        open Z;
        let interm = modulo(powm(x, of_int(3), p) - modulo(of_int(3) * x,p) + b,p);
        let r = sqrt_mod(interm,p);
        let y_point =
          if (Int.equal(y_bit, 3)) {
            equal(modulo(r, of_int(2)), one) ? r : modulo(neg(r), p);
          } else {
            equal(modulo(r, of_int(2)), zero) ? r : modulo(neg(r), p);
          };
        Printf.printf("\nare interm and y_point^2 equal:  %b\n", equal(powm(y_point,of_int(2),p), interm));
        let y_point = Z.to_bits(y_point) |> Cstruct.of_string;
        Some(y_point);
      };
      Printf.printf("bytes %d", Cstruct.length(y)); // should be 32
      Cstruct.set_uint8(res, 0, 4);
      Cstruct.blit(buf, 1, res, 1, 32);
      Cstruct.blit(y, 0, res, 33, 32);
      res |> Dsa.pub_of_cstruct |> Result.to_option;
    };
  };
  module Pub = {
    type t = Dsa.pub_;

    let size = 33;
    let prefix = Base58.Prefix.p256_public_key;
    let encoding = {
      let to_bytes = t => Dsa.pub_to_cstruct(t) |> Cstruct.to_bytes;
      let of_bytes_exn = b =>
        Cstruct.of_bytes(b) |> Dsa.pub_of_cstruct |> Result.get_ok;
      Data_encoding.(conv(to_bytes, of_bytes_exn, Fixed.bytes(size)));
    };

    let to_raw = t => Compresss.compress(t);

    let of_raw = string => Compresss.decompress(string);

    let to_string = Base58.simple_encode(~prefix, ~to_raw);
    let of_string = Base58.simple_decode(~prefix, ~of_raw);
  };
  module Hash = {
    type t = BLAKE2B_20.t;

    let hash_key = t => BLAKE2B_20.hash(Compresss.compress(t));

    let encoding = {
      let name = "P256.Public_key_hash";
      Data_encoding.(obj1(req(name, blake2b_20_encoding)));
    };

    let prefix = Base58.Prefix.p256_public_key_hash;
    let to_raw = BLAKE2B_20.to_raw_string;
    let of_raw = BLAKE2B_20.of_raw_string;
    let to_string = Base58.simple_encode(~prefix, ~to_raw);
    let of_string = Base58.simple_decode(~prefix, ~of_raw);
  };

  module Secret = {
    type t = Dsa.priv;

    let _size = 56;
    let prefix = Base58.Prefix.p256_secret_key;
    let to_raw = t => Cstruct.to_string(Dsa.priv_to_cstruct(t));
    let of_raw = string => {
      Dsa.priv_of_cstruct(Cstruct.of_string(string)) |> Result.to_option;
    };

    let to_string = Base58.simple_encode(~prefix, ~to_raw);
    let of_string = Base58.simple_decode(~prefix, ~of_raw);
  };
  module Signature = {
    type t = string;
    let sign = (secret, message) => {
      let hash = BLAKE2B.hash(message);
      Cstruct.of_string(BLAKE2B.to_raw_string(hash))
      |> Dsa.sign(~key=secret)
      |> (((r, s)) => Cstruct.to_string(r) ++ Cstruct.to_string(s));
    };
    let check = (public, signature, message) => {
      let hash = BLAKE2B.hash(message);
      let (s, r) = (
        String.sub(signature, 0, 32),
        String.sub(signature, 32, 32),
      );
      Dsa.verify(
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
