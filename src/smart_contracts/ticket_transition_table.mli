module Errors : sig
  type t =
    [ `Attempted_to_merge_different_tickets
    | `Insufficient_funds
    | `Ticket_doesnt_exist
    | `Ticket_ownership_violation
    | `Ticket_split_invalid_amount ]
  [@@deriving show]
end

module type S = sig
  include Conversions.S

  module Ticket_handle : Ticket_handle.S

  type t

  val own :
    t ->
    Address.t ->
    Ticket_handle.t ->
    ( Ticket_handle.t,
      [> `Ticket_doesnt_exist | `Ticket_ownership_violation] )
    result

  val read_ticket :
    t ->
    sender:Address.t ->
    ticket_handle:Ticket_handle.t ->
    ( Ticket_id.t * Amount.t * Ticket_handle.t,
      [> `Ticket_doesnt_exist | `Ticket_ownership_violation] )
    result

  val split_ticket :
    t ->
    sender:Address.t ->
    ticket_handle:Ticket_handle.t ->
    amounts:Amount.t * Amount.t ->
    ( Ticket_handle.t * Ticket_handle.t,
      [> `Ticket_doesnt_exist
      | `Ticket_ownership_violation
      | `Ticket_split_invalid_amount ] )
    result

  val join_tickets :
    t ->
    sender:Address.t ->
    handles:Ticket_handle.t * Ticket_handle.t ->
    ( Ticket_handle.t,
      [> `Ticket_doesnt_exist | `Ticket_ownership_violation] )
    result

  val init :
    sender:Address.t ->
    self:Address.t ->
    tickets:(Ticket_id.t * Amount.t) Seq.t ->
    temporary_tickets:(Ticket_id.t * Amount.t) Seq.t ->
    t

  val finalize : t -> (Ticket_id.t * Amount.t) Seq.t
end

module Make (CC : Conversions.S) :
  S with module Address = CC.Address and module Amount = CC.Amount
