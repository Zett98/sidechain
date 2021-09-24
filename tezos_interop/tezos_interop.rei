open Helpers;

module Base58 = Base58;
module Ed25519 = Crypto.Ed25519;
module P256 = Crypto.P256;

module Key: {
  type t =
    | Ed25519(Ed25519.Pub.t)
    | P256(P256.Pub.t);

  let to_string: t => string;
  let of_string: string => option(t);
};

module Key_hash: {
  type t =
    | Ed25519(BLAKE2B_20.t)
    | P256(BLAKE2B_20.t);

  let of_key: Key.t => t;

  let to_string: t => string;
  let of_string: string => option(t);
};

module Secret: {
  type t =
    | Ed25519(Ed25519.Secret.t)
    | P256(P256.Secret.t);

  let to_string: t => string;
  let of_string: string => option(t);
};

module Signature: {
  type t = pri | Ed25519(string) | P256(string);

  let sign: (Secret.t, string) => t;
  let check: (Key.t, t, string) => bool;

  let to_string: t => string;
  let of_string: string => option(t);

  // TODO: this is a leaky abstraction
  let of_raw_string: [ | `Ed25519(string) | `P256(string)] => t;
};

module Contract_hash: {
  type t = BLAKE2B_20.t;

  let to_string: t => string;
  let of_string: string => option(t);
};

module Address: {
  type t =
    | Implicit(Key_hash.t)
    | Originated({
        contract: Contract_hash.t,
        entrypoint: option(string),
      });

  let to_string: t => string;
  let of_string: string => option(t);

  let to_yojson: t => Yojson.Safe.t;
  let of_yojson: Yojson.Safe.t => result(t, string);
};

module Ticket: {
  type t = {
    ticketer: Address.t,
    data: bytes,
  };

  let to_string: t => string;
  let of_string: string => option(t);
};

module Operation_hash: {
  type t = BLAKE2B.t;

  let to_string: t => string;
  let of_string: string => option(t);
};

module Pack: {
  type t;

  let int: Z.t => t;
  let bytes: bytes => t;
  let pair: (t, t) => t;
  let list: list(t) => t;
  let key: Key.t => t;
  let key_hash: Key_hash.t => t;
  let address: Address.t => t;

  let to_bytes: t => bytes;
};

module Context: {
  type t = {
    rpc_node: Uri.t,
    secret: Secret.t,
    consensus_contract: Address.t,
    required_confirmations: int,
  };
};
module Consensus: {
  let hash_validators: list(Key.t) => BLAKE2B.t;
  let hash_block:
    (
      ~block_height: int64,
      ~block_payload_hash: BLAKE2B.t,
      ~state_root_hash: BLAKE2B.t,
      ~handles_hash: BLAKE2B.t,
      ~validators_hash: BLAKE2B.t
    ) =>
    BLAKE2B.t;
  let hash_withdraw_handle:
    (
      ~id: Z.t,
      ~owner: Address.t,
      ~amount: Z.t,
      ~ticketer: Address.t,
      ~data: bytes
    ) =>
    BLAKE2B.t;

  /** ~signatures should be in the same order as the old validators */
  let commit_state_hash:
    (
      ~context: Context.t,
      ~block_hash: BLAKE2B.t,
      ~block_height: int64,
      ~block_payload_hash: BLAKE2B.t,
      ~state_hash: BLAKE2B.t,
      ~handles_hash: BLAKE2B.t,
      ~validators: list(Key.t),
      ~signatures: list((Key.t, option(Signature.t)))
    ) =>
    Lwt.t(unit);

  type parameters =
    | Deposit({
        ticket: Ticket.t,
        // TODO: proper type for amounts
        amount: Z.t,
        destination: Address.t,
      });
  type operation = {
    hash: Operation_hash.t,
    index: int,
    parameters,
  };
  let listen_operations:
    (~context: Context.t, ~on_operation: operation => unit) => unit;
};

module Discovery: {let sign: (Secret.t, ~nonce: int64, Uri.t) => Signature.t;};
