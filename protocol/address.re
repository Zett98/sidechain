open Helpers;
open Mirage_crypto_ec;

type key = Ed25519.priv;

let key_to_yojson = key =>
  `String(Tezos_interop.Secret.to_string(Ed25519(key)));
let key_of_yojson = json => {
  let.ok string = [%of_yojson: string](json);
  let k = Tezos_interop.Secret.of_string(string);
  Option.bind(
    k,
    fun
    | Ed25519(key) => Some(key)
    | P256(_) => None,
  )
  |> Option.to_result(~none="failed to parse");
};

type t = Ed25519.pub_; // TODO: is okay to have this public

let make_pubkey = () => {
  let (_priv, pub_) = Ed25519.generate();
  pub_;
};

let compare = (a, b) =>
  Cstruct.compare(Ed25519.pub_to_cstruct(a), Ed25519.pub_to_cstruct(b));
let to_string = t => Tezos_interop.Key.to_string(Ed25519(t));
let of_string = string => {
  let t = Tezos_interop.Key.of_string(string);
  Option.bind(
    t,
    fun
    | Ed25519(t) => Some(t)
    | P256(_) => None,
  );
};

let to_yojson = t => `String(to_string(t));
let of_yojson = json => {
  let.ok string = [%of_yojson: string](json);
  of_string(string) |> Option.to_result(~none="failed to parse");
};

let of_key = Ed25519.pub_of_priv;

let genesis_key = {|edsk4bfbFdb4s2BdkW3ipfB23i9u82fgji6KT3oj2SCWTeHUthbSVd|};
let genesis_key =
  switch (key_of_yojson(`String(genesis_key))) {
  | Ok(key) => key
  | Error(error) => failwith(error)
  };
let genesis_address = Ed25519.pub_of_priv(genesis_key);
