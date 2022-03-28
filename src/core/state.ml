open Helpers
open Crypto
type t = {
  ledger : Ledger.t;
  contract_storage : Contract_storage.t;
}
[@@deriving yojson]
type receipt = Receipt_tezos_withdraw of Ledger.Withdrawal_handle.t
[@@deriving yojson]
let empty = { ledger = Ledger.empty; contract_storage = Contract_storage.empty }
let ledger t = t.ledger
let contract_storage t = t.contract_storage
let hash t = to_yojson t |> Yojson.Safe.to_string |> BLAKE2B.hash
let apply_tezos_operation t tezos_operation =
  let open Tezos_operation in
  let apply_internal_operation t internal_operation =
    let { ledger; contract_storage = _ } = t in
    match internal_operation with
    | Tezos_deposit { destination; amount; ticket } ->
      let ledger =
        match destination with
        | Implicit key_hash ->
          let destination = key_hash in
          Ledger.deposit destination amount ticket ledger
        | Originated _ -> failwith "not implemented" in
      { t with ledger } in
  let { hash = _; payload } = tezos_operation in
  let { tezos_operation_hash = _; internal_operations } = payload in
  List.fold_left apply_internal_operation t internal_operations

let apply_user_operation t user_operation =
  let open User_operation in
  let { hash; sender; initial_operation } = user_operation in
  match initial_operation with
  | Transaction { destination; amount; ticket } ->
    let%ok ledger =
      Ledger.transfer ~sender ~destination amount ticket t.ledger in
    Ok ({ contract_storage = t.contract_storage; ledger }, None)
  | Tezos_withdraw { owner; amount; ticket } ->
    let%ok ledger, handle =
      Ledger.withdraw ~sender ~destination:owner amount ticket t.ledger in
    Ok
      ( { ledger; contract_storage = t.contract_storage },
        Some (Receipt_tezos_withdraw handle) )
  | Contract_origination { to_originate } ->
    (* @TODO: deduct gas from account and check *)
    let balance = max_int |> Amount.of_int in
    let origination_cost = 250 |> Amount.of_int in
    let%assert () =
      Amount.
        ( `Origination_error t,
          let comparison_result = compare balance origination_cost in
          comparison_result >= 0 ) in

    let initial_gas = Amount.to_int Amount.(balance - origination_cost) in
    let burn_and_update t _ _to_burn = `Origination_error t in
    (* TODO: Burn on storage size change, need CTEZ *)
    let%ok contract, _to_burn =
      Smart_contracts.Contract.Compile.compile_script ~originated_by:sender
        ~gas:initial_gas ~on_error:(burn_and_update t) to_originate in
    let address = hash |> BLAKE2B.to_raw_string |> BLAKE2B_20.hash in
    let contract_storage =
      Contract_storage.originate_contract t.contract_storage ~address ~contract
    in
    Ok ({ contract_storage; ledger = t.ledger }, None)

let apply_user_operation t user_operation =
  match apply_user_operation t user_operation with
  | Ok (t, receipt) -> (t, receipt)
  | Error (`Origination_error t) -> (t, None)
  | Error
      ( `Not_enough_funds | `Transaction_sender_must_be_implicit_account
      | `Withdraw_sender_must_be_implicit_account
      | `Contract_originator_must_be_implicit_account ) ->
    (t, None)
