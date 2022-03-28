open Crypto
open Helpers
open Core

let make_ticket ?ticketer ?data () =
  let open Tezos in
  let ticketer =
    match ticketer with
    | Some ticketer -> ticketer
    | None ->
      let random_hash =
        Random.generate 20
        |> Cstruct.to_string
        |> BLAKE2B_20.of_raw_string
        |> Option.get in
      Address.Originated { contract = random_hash; entrypoint = None } in
  let data =
    match data with
    | Some data -> data
    | None -> Random.generate 256 |> Cstruct.to_bytes in
  let open Ticket_id in
  { ticketer; data }

let make_address () =
  let _secret, _key, key_hash = Key_hash.make_ed25519 () in
  key_hash

let make_tezos_address () =
  let open Crypto in
  let open Tezos in
  let _key, address = Ed25519.generate () in
  let hash = Ed25519.Key_hash.of_key address in
  Address.Implicit (Ed25519 hash)

let setup ?(initial_amount = 10000) () =
  let t2 = make_ticket () in
  let tezos_address = make_tezos_address () in
  let op =
    Tezos_operation.Tezos_deposit
      {
        destination = tezos_address;
        ticket = t2;
        amount = Amount.of_int initial_amount;
      } in
  let s = State.empty in
  let opp =
    {
      Tezos_operation.tezos_operation_hash =
        "opCAkifFMh1Ya2J4WhRHskaXc297ELtx32wnc2WzeNtdQHp7DW4"
        |> Tezos.Operation_hash.of_string
        |> Option.get;
      internal_operations = [op];
    } in
  let opp = Tezos_operation.make opp in
  let make_address =
    tezos_address |> Tezos.Address.to_string |> Key_hash.of_string |> Option.get
  in
  (State.apply_tezos_operation s opp, make_address)

let amount =
  Alcotest.of_pp (fun ppf x -> Format.fprintf ppf "%d" (Amount.to_int x))

let test msg =
  let initial_state, address = setup () in
  let script = [%lambda_vm.script fun x -> x + 1L] in
  let value = Lambda_vm.(Ast.Int64 1L) in
  let open Smart_contracts in
  let code =
    Lambda_vm.Ast.script_to_yojson script
    |> Yojson.Safe.to_string
    |> Raw.Script.make
    |> Result.get_ok in
  let storage =
    Lambda_vm.Ast.value_to_yojson value
    |> Yojson.Safe.to_string
    |> Raw.Value.make
    |> Result.get_ok in
  let payload =
    Origination_payload.make_lambda ~code ~storage |> Result.get_ok in
  let operation =
    User_operation.Contract_origination { to_originate = payload } in
  let user_op = User_operation.make ~sender:address operation in
  let state, _ = State.apply_user_operation initial_state user_op in
  [
    Alcotest.test_case msg `Quick (fun () ->
        Alcotest.(check' bool)
          ~msg ~expected:false
          ~actual:
            (Contract_storage.equal
               (State.contract_storage state)
               Contract_storage.empty));
  ]
let payload_failures ~msg =
  let script = [%lambda_vm.script fun x -> x + 1L] in
  let value = Lambda_vm.(Ast.Int64 1L) in
  let open Smart_contracts in
  let code = Lambda_vm.Ast.script_to_yojson script |> Yojson.Safe.to_string in
  let storage = Lambda_vm.Ast.value_to_yojson value |> Yojson.Safe.to_string in
  let code_empty = "" in
  let code_invalid = "awdw" in
  let storage_empty = "" in
  let storage_invalid = "awd" in
  let originate code storage =
    let%ok code = Raw.Script.make code in
    let%ok storage = Raw.Value.make storage in
    Origination_payload.make_lambda ~code ~storage in
  let check res () =
    let _ = res |> Result.map_error (fun x -> raise (Failure x)) in
    () in
  [
    Alcotest.test_case msg `Quick (fun () ->
        Alcotest.(check_raises)
          msg (Failure "invalid input")
          (originate code_empty storage_empty |> check));
    Alcotest.test_case msg `Quick (fun () ->
        Alcotest.(check_raises)
          msg (Failure "invalid input")
          (originate code storage_empty |> check));
    Alcotest.test_case msg `Quick (fun () ->
        Alcotest.(check_raises)
          msg (Failure "invalid input")
          (originate code_empty storage |> check));
    Alcotest.test_case msg `Quick (fun () ->
        Alcotest.(check_raises)
          msg (Failure "invalid input")
          (originate code_invalid storage_invalid |> check));
    Alcotest.test_case msg `Quick (fun () ->
        Alcotest.(check_raises)
          msg (Failure "invalid input")
          (originate code storage_invalid |> check));
    Alcotest.test_case msg `Quick (fun () ->
        Alcotest.(check_raises)
          msg (Failure "invalid input")
          (originate code_invalid storage |> check));
  ]
let test_origination =
  ( "Origination gas costs",
    [
      test "Origination should succeed";
      payload_failures ~msg:"Invalid payloads should fail origination";
    ]
    |> List.flatten )
