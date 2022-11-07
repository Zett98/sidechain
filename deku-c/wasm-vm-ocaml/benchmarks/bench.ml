open Core_bench

let incrementer =
  let open Wasm in
  match
    {|
(module
  (memory 1 100)
  (func (export "addTwo") (param i32 i32) (result i32)
    local.get 0
    local.get 1
    i32.add))
|}
    |> Parse.string_to_module
  with
  | Source.{ it = Textual m; _ } -> m
  | _ -> assert false

let initialize m =
  let open Wasm in
  Eval.init m [] ~gas_limit:Int64.max_int

let get_main t =
  let open Wasm in
  match Wasm.Instance.export t (Utf8.decode "addTwo") |> Option.get with
  | Instance.ExternFunc x -> x
  | _ -> assert false

let call t =
  let open Wasm in
  match
    Eval.invoke t [ Wasm.Values.Num (I32 1l); Wasm.Values.Num (I32 1l) ]
  with
  | [ Wasm.Values.Num (I32 _) ] -> ()
  | _ -> failwith "lifecycle error"

let harness size f =
  let rec go size f =
    if size <= 0 then ()
    else
      let () = ignore (f ()) in
      go (size - 1) f
  in
  go size f

let benchmarks =
  let initialized' = incrementer |> initialize in
  let initialized = initialized' |> get_main in
  [
    Bench.Test.create_indexed ~name:"Call initialized" ~args:[ 10; 100; 1000 ]
      (fun len ->
        Base.Staged.stage (fun () ->
            ignore
              (harness len (fun () ->
                   Wasm.Instance.set_gas_limit initialized' Int64.max_int;
                   call initialized))));
    Bench.Test.create_indexed ~name:"Initialize" ~args:[ 10; 100; 1000 ]
      (fun len ->
        Base.Staged.stage (fun () ->
            ignore (harness len (fun () -> initialize incrementer))));
    Bench.Test.create_indexed ~name:"Initialize_and_call"
      ~args:[ 10; 100; 1000 ] (fun len ->
        Base.Staged.stage (fun () ->
            ignore
              (harness len (fun () ->
                   let initialized = incrementer |> initialize in
                   let initialized = initialized |> get_main in
                   call initialized))));
  ]

let () = benchmarks |> Bench.make_command |> Command_unix.run
