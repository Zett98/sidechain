open Deku_ledger
module Table = Map.Make (Contract_address)

type t = State : State_entry.t Table.t -> t [@@unboxed] [@@deriving eq, ord]

let show (State t) =
  [%show: (Contract_address.t * State_entry.t) list] (Table.bindings t)

let empty = State Table.empty

let pp fmt (State t) =
  Format.fprintf fmt "%a"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@")
       (fun fmt (k, v) ->
         Format.fprintf fmt "%a -> %a" Contract_address.pp k State_entry.pp v))
    (Table.bindings t)

let add_contract (State t) address entry = State (Table.add address entry t)
let fetch_contract (State t) address = Table.find address t
