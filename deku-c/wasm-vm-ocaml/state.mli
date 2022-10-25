open Deku_ledger

type t [@@deriving eq, ord, show]

val empty : t
val add_contract : t -> Contract_address.t -> State_entry.t -> t
val fetch_contract : t -> Contract_address.t -> State_entry.t
