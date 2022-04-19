open Crypto
module Value = struct
  type t = {
    key : int;
    hash : BLAKE2B.t;
        [@printer
          fun fmt t -> Format.fprintf fmt "%s" (BLAKE2B.to_raw_string t)]
  }
  [@@deriving yojson, eq]
  let hash t = t.hash
  let make data key = { key; hash = BLAKE2B.hash data }
end
module Patricia = Incremental_patricia.Make (Value)

let build list =
  let tree, values =
    List.fold_left
      (fun (acc, values) x ->
        let tree, value = Patricia.add (Value.make x) (List.hd acc) in
        (tree :: acc, value :: values))
      ([Patricia.empty], [])
      list in
  (List.rev tree, List.rev values)
let add_and_find list =
  let tree, values = build list in
  let tree = List.rev tree |> List.hd in
  let found =
    List.fold_left
      (fun acc (x : Value.t) ->
        let _, stored = Patricia.find x.key tree |> Option.get in
        stored :: acc)
      [] values in
  List.for_all2
    (fun actual stored -> Value.equal stored actual)
    (values |> List.rev) found

let hashing list =
  let size = List.length list in
  let rec group size f acc = function
    | [] -> acc
    | [x] when size = 1 -> group size f (f x :: acc) []
    | [x] ->
      group size f (BLAKE2B.both (f x) (Patricia.hash Patricia.empty) :: acc) []
    | x :: y :: rest -> group size f (BLAKE2B.both (f x) (f y) :: acc) rest
  in
  let group f = group size f in
  let rec hash_and_go size lst =
    match lst with
    | [] -> Patricia.hash Patricia.empty
    | [x] -> x
    | all -> hash_and_go size (group Fun.id [] all) in
  let hash_and_go t = hash_and_go size (group Value.hash [] t) in
  let tree, values = build list in
  let tree =
    List.mapi (fun x y -> (x, y)) tree |> List.filter (fun (x, _) -> x = 0)
  in
  List.for_all
    (fun (idx, tree) ->
      let nodes = values |> List.to_seq |> Seq.take idx |> List.of_seq in
      let hashed_tree = Patricia.hash tree in
      let hashed_nodes = hash_and_go nodes in
      BLAKE2B.equal hashed_tree hashed_nodes)
    tree

let create ~name ~fn =
  QCheck_alcotest.to_alcotest
    QCheck2.(
      Test.make ~count:100 ~name
        (Gen.list_size (Gen.int_bound 120) Gen.string)
        (fun size -> fn size))
let produces_the_same_result list =
  let tree, values = build list in
  let tree_1_root = List.rev tree |> List.hd |> Patricia.hash in
  let tree', values' = build list in
  let tree_2_root = List.rev tree' |> List.hd |> Patricia.hash in
  BLAKE2B.equal tree_1_root tree_2_root
  && List.for_all2
       (fun a b -> BLAKE2B.equal (Value.hash a) (Value.hash b))
       values values'

let () =
  let open Alcotest in
  run "Patricia_tests"
    [
      ( "Patricia tree",
        [
          create ~name:"Add and find elements" ~fn:add_and_find;
          create ~name:"always produces the same result"
            ~fn:produces_the_same_result;
          create ~name:"hashing" ~fn:hashing;
        ] );
    ]
