open Helpers
open Crypto
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
let setup_two () =
  let t1 = make_ticket () in
  let t2 = make_ticket () in
  let a = make_address () in
  let b = make_address () in
  let t =
    Ledger.(
      empty
      |> deposit a (Amount.of_int 100) t1
      |> deposit a (Amount.of_int 300) t2
      |> deposit b (Amount.of_int 200) t1
      |> deposit b (Amount.of_int 400) t2) in
  (t, (t1, t2), (a, b))
let amount_testable =
  Alcotest.testable
    (fun fmt t -> Format.fprintf fmt "%d" (Amount.to_int t))
    Amount.equal
let test_amount ledger ~address ~ticket ~expected =
  Alcotest.(check' amount_testable)
    ~msg:"Amount should be the same" ~expected:(Amount.of_int expected)
    ~actual:(Ledger.balance address ticket ledger)
let check_amount_raises gen f =
  QCheck_alcotest.to_alcotest
    QCheck2.(Test.make ~count:100 ~name:"Raise on negative integers" gen f)

let amount_tests =
  let raise_subtract amount =
    let failing_amount = amount + 1 in
    match Amount.(of_int amount - of_int failing_amount) with
    | _ -> false
    | exception Invalid_argument _ -> true in
  let raise_negative amount =
    match Amount.(of_int amount) with
    | _ -> false
    | exception Invalid_argument _ -> true in
  [
    check_amount_raises QCheck2.Gen.big_nat raise_subtract;
    check_amount_raises
      QCheck2.(Gen.int_range ~origin:0 Int.min_int (-1))
      raise_negative;
  ]
let balance_tests =
  let t, (t1, t2), (a, b) = setup_two () in
  let tests () =
    test_amount ~address:a ~ticket:t1 ~expected:100 t;
    test_amount ~address:a ~ticket:t2 ~expected:300 t;
    test_amount ~address:b ~ticket:t1 ~expected:200 t;
    test_amount ~address:b ~ticket:t2 ~expected:400 t;
    test_amount ~address:(make_address ()) ~ticket:t1 ~expected:0 t;
    test_amount ~address:(make_address ()) ~ticket:t2 ~expected:0 t;
    test_amount ~address:a ~ticket:(make_ticket ()) ~expected:0 t;
    test_amount ~address:b ~ticket:(make_ticket ()) ~expected:0 t in
  [Alcotest.test_case "Balance" `Quick tests]
let transfer_tests =
  let tests () =
    let t, (t1, t2), (a, b) = setup_two () in
    let expect_result_ok t =
      Alcotest.(check bool) "should be successful" true (Result.is_ok t) in
    let expect_result_failure t =
      Alcotest.(check bool)
        "should fail" true
        (Result.get_error t = `Not_enough_funds) in
    let c = make_address () in
    let t = Ledger.transfer ~sender:a ~destination:b (Amount.of_int 1) t1 t in
    expect_result_ok t;
    let t = Result.get_ok t in
    test_amount ~address:a ~ticket:t1 ~expected:99 t;
    test_amount ~address:a ~ticket:t2 ~expected:300 t;
    test_amount ~address:b ~ticket:t1 ~expected:201 t;
    test_amount ~address:b ~ticket:t2 ~expected:400 t;
    test_amount ~address:c ~ticket:t1 ~expected:0 t;
    test_amount ~address:c ~ticket:t2 ~expected:0 t;
    let t = Ledger.transfer ~sender:b ~destination:a (Amount.of_int 3) t2 t in
    expect_result_ok t;
    let t = Result.get_ok t in
    test_amount ~address:a ~ticket:t1 ~expected:99 t;
    test_amount ~address:a ~ticket:t2 ~expected:303 t;
    test_amount ~address:b ~ticket:t1 ~expected:201 t;
    test_amount ~address:b ~ticket:t2 ~expected:397 t;
    test_amount ~address:c ~ticket:t1 ~expected:0 t;
    test_amount ~address:c ~ticket:t2 ~expected:0 t;
    let t = Ledger.transfer ~sender:b ~destination:c (Amount.of_int 5) t2 t in
    expect_result_ok t;
    let t = Result.get_ok t in
    test_amount ~address:a ~ticket:t1 ~expected:99 t;
    test_amount ~address:a ~ticket:t2 ~expected:303 t;
    test_amount ~address:b ~ticket:t1 ~expected:201 t;
    test_amount ~address:b ~ticket:t2 ~expected:392 t;
    test_amount ~address:c ~ticket:t1 ~expected:0 t;
    test_amount ~address:c ~ticket:t2 ~expected:5 t;
    let t = Ledger.transfer ~sender:a ~destination:c (Amount.of_int 99) t1 t in
    expect_result_ok t;
    let t = Result.get_ok t in
    test_amount ~address:a ~ticket:t1 ~expected:0 t;
    test_amount ~address:a ~ticket:t2 ~expected:303 t;
    test_amount ~address:b ~ticket:t1 ~expected:201 t;
    test_amount ~address:b ~ticket:t2 ~expected:392 t;
    test_amount ~address:c ~ticket:t1 ~expected:99 t;
    test_amount ~address:c ~ticket:t2 ~expected:5 t;
    (let t = Ledger.transfer ~sender:b ~destination:c (Amount.of_int 202) t1 t in
     expect_result_failure t);
    (let d = make_address () in
     let t = Ledger.transfer ~sender:d ~destination:c (Amount.of_int 1) t2 t in
     expect_result_failure t);
    let t3 = make_ticket () in
    let t = Ledger.transfer ~sender:a ~destination:b (Amount.of_int 1) t3 t in
    expect_result_failure t in
  [Alcotest.test_case "Transfer tests" `Quick tests]

(*
    test "deposit" (fun _ expect_balance ->
        let t, (t1, t2), (a, b) = setup_two () in
        let t = deposit a (Amount.of_int 123) t1 t in
        expect_balance a t1 223 t;
        expect_balance a t2 300 t;
        expect_balance b t1 200 t;
        expect_balance b t2 400 t;
        let t = deposit b (Amount.of_int 456) t2 t in
        expect_balance a t1 223 t;
        expect_balance a t2 300 t;
        expect_balance b t1 200 t;
        expect_balance b t2 856 t);
    test "withdraw" (fun expect expect_balance ->
        let t, (t1, t2), (a, b) = setup_two () in
        let destination = make_tezos_address () in
        let t = withdraw ~sender:a ~destination (Amount.of_int 10) t1 t in
        (expect.result t).toBeOk ();
        let t, handle = Result.get_ok t in
        expect_balance a t1 90 t;
        expect_balance a t2 300 t;
        expect_balance b t1 200 t;
        expect_balance b t2 400 t;
        expect.equal handle.id 0;
        expect.equal handle.owner destination;
        expect.equal handle.amount (Amount.of_int 10);
        let t = withdraw ~sender:b ~destination (Amount.of_int 9) t2 t in
        (expect.result t).toBeOk ();
        let t, handle = Result.get_ok t in
        expect_balance a t1 90 t;
        expect_balance a t2 300 t;
        expect_balance b t1 200 t;
        expect_balance b t2 391 t;
        expect.equal handle.id 1;
        expect.equal handle.owner destination;
        expect.equal handle.amount (Amount.of_int 9);
        let t = withdraw ~sender:a ~destination (Amount.of_int 8) t2 t in
        (expect.result t).toBeOk ();
        let t, handle = Result.get_ok t in
        expect_balance a t1 90 t;
        expect_balance a t2 292 t;
        expect_balance b t1 200 t;
        expect_balance b t2 391 t;
        expect.equal handle.id 2;
        expect.equal handle.owner destination;
        expect.equal handle.amount (Amount.of_int 8);
        (let t = withdraw ~sender:a ~destination (Amount.of_int 91) t1 t in
         (expect.result t).toBeError ());
        (let t = withdraw ~sender:b ~destination (Amount.of_int 203) t1 t in
         (expect.result t).toBeError ());
        (let c = make_address () in
         let t = withdraw ~sender:c ~destination (Amount.of_int 1) t1 t in
         (expect.result t).toBeError ());
        ());
    test "compare" (fun expect _ ->
        let t, (t1, _), (a, b) = setup_two () in
        (let t1' = make_ticket ~data:t1.data () in
         let t = transfer ~sender:a ~destination:b (Amount.of_int 1) t1' t in
         (expect.result t).toBeError ();
         expect.equal (Result.get_error t) `Not_enough_funds);
        (let t1' = make_ticket ~ticketer:t1.ticketer () in
         let t = transfer ~sender:a ~destination:b (Amount.of_int 1) t1' t in
         (expect.result t).toBeError ();
         expect.equal (Result.get_error t) `Not_enough_funds);
        ())) *)

let () =
  let open Alcotest in
  run "Ledger tests"
    [
      ( "Ledger",
        [amount_tests; balance_tests; transfer_tests; []] |> List.concat );
    ]
