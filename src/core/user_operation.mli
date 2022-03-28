open Crypto
type initial_operation =
  | Transaction          of {
      destination : Key_hash.t;
      amount : Amount.t;
      ticket : Ticket_id.t;
    }
  | Contract_origination of {
      to_originate : Smart_contracts.Origination_payload.t;
    }
  | Contract_invocation  of {
      to_invoke : Tezos.Contract_hash.t;
      argument : Smart_contracts.Invocation_payload.t;
    }
  | Tezos_withdraw       of {
      owner : Tezos.Address.t;
      amount : Amount.t;
      ticket : Ticket_id.t;
    }

type t = private {
  hash : BLAKE2B.t;
  sender : Key_hash.t;
  initial_operation : initial_operation;
}
[@@deriving eq, ord, yojson]

val make : sender:Key_hash.t -> initial_operation -> t
