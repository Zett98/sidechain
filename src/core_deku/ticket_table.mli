module Ticket_repr : sig
  type t = {
    ticket : Ticket_id.t;
    amount : Amount.t;
  }
  [@@deriving yojson, eq, ord]

  val ticket : t -> Ticket_id.t
  val make : Ticket_id.t -> Amount.t -> t
end
module Errors : sig
  type t =
    [ `Insufficient_funds
    | `Ticket_doesnt_exist
    | `Ticket_ownership_violation
    | `Ticket_split_invalid_amount
    | `Attempted_to_merge_different_tickets ]
  [@@deriving show]
end
type t [@@deriving yojson]

val empty : t
val validate : t -> t

val amount : t -> Address.t -> Ticket_id.t -> Amount.t option

val unsafe_deposit_ticket :
  t -> ticket:Ticket_id.t -> destination:Address.t -> amount:Amount.t -> t

val own :
  t ->
  Address.t ->
  Ticket_handle.t ->
  ( Ticket_handle.t * t,
    [`Ticket_doesnt_exist | `Ticket_ownership_violation] )
  result

val read_ticket :
  t ->
  sender:Address.t ->
  ticket_handle:Ticket_handle.t ->
  ( (Ticket_id.t * Amount.t * Ticket_handle.t) * t,
    [`Ticket_doesnt_exist | `Ticket_ownership_violation] )
  result

val disown :
  t ->
  Address.t ->
  Ticket_handle.t ->
  (t, [`Ticket_doesnt_exist | `Ticket_ownership_violation]) result

val split_ticket :
  t ->
  sender:Address.t ->
  ticket_handle:Ticket_handle.t ->
  amounts:Amount.t * Amount.t ->
  ( (Ticket_handle.t * Ticket_handle.t) * t,
    [ `Insufficient_funds
    | `Ticket_doesnt_exist
    | `Ticket_ownership_violation
    | `Ticket_split_invalid_amount ] )
  result
val join_tickets :
  t ->
  source:Address.t ->
  senders:Address.t * Address.t ->
  handles:Ticket_handle.t * Ticket_handle.t ->
  ( Ticket_handle.t * t,
    [ `Attempted_to_merge_different_tickets
    | `Ticket_doesnt_exist
    | `Ticket_ownership_violation ] )
  result
val transfer :
  t ->
  sender:Address.t ->
  ticket:Ticket_id.t ->
  destination:Address.t ->
  amount:Amount.t ->
  ( t,
    [ `Insufficient_funds
    | `Ticket_doesnt_exist
    | `Ticket_ownership_violation
    | `Ticket_split_invalid_amount ] )
  result

val unsafe_withdraw :
  t ->
  ticket:Ticket_id.t ->
  sender:Address.t ->
  amount:Amount.t ->
  ( t,
    [ `Insufficient_funds
    | `Ticket_doesnt_exist
    | `Ticket_ownership_violation
    | `Ticket_split_invalid_amount ] )
  result

val add_to_temporary : t -> Ticket_handle.t list -> t
