open Base
open Core_bench
open Zinc_interpreter.Dummy
open Types.Zinc
open Z

let convert zinc =
  let from = List.nth_exn zinc 0 |> snd |> Types.Zinc.to_yojson in
  let to_ = Types.Zinc.of_yojson from |> Result.ok_or_failwith in
  to_

let prog1 =
  let list =
    List.init 1000 ~f:(fun x -> [Plain_old_data (Mutez ~$x); Operation Cons])
    |> List.concat
  in
  [("a", List.concat [[Plain_old_data Nil]; list; [Core Return]])] |> convert

let prog2 =
  [
    ( "or_true",
      [
        Core Grab;
        Plain_old_data (Bool true);
        Core (Access 0);
        Operation Or;
        Core Return;
      ] );
    ( "or_false",
      [
        Core
          (Closure
             [
               Core Grab;
               Plain_old_data (Bool true);
               Core (Access 0);
               Operation Or;
               Core Return;
             ]);
        Core Grab;
        Core Grab;
        Plain_old_data (Bool false);
        Core (Access 0);
        Operation Or;
        Core Return;
      ] );
    ( "and_true",
      [
        Core
          (Closure
             [
               Core Grab;
               Plain_old_data (Bool true);
               Core (Access 0);
               Operation Or;
               Core Return;
             ]);
        Core Grab;
        Core
          (Closure
             [
               Core Grab;
               Plain_old_data (Bool false);
               Core (Access 0);
               Operation Or;
               Core Return;
             ]);
        Core Grab;
        Core Grab;
        Plain_old_data (Bool true);
        Core (Access 0);
        Operation And;
        Core Return;
      ] );
    ( "and_false",
      [
        Core
          (Closure
             [
               Core Grab;
               Plain_old_data (Bool true);
               Core (Access 0);
               Operation Or;
               Core Return;
             ]);
        Core Grab;
        Core
          (Closure
             [
               Core Grab;
               Plain_old_data (Bool false);
               Core (Access 0);
               Operation Or;
               Core Return;
             ]);
        Core Grab;
        Core
          (Closure
             [
               Core Grab;
               Plain_old_data (Bool true);
               Core (Access 0);
               Operation And;
               Core Return;
             ]);
        Core Grab;
        Core Grab;
        Plain_old_data (Bool false);
        Core (Access 0);
        Operation And;
        Core Return;
      ] );
    ( "not_bool",
      [
        Core
          (Closure
             [
               Core Grab;
               Plain_old_data (Bool true);
               Core (Access 0);
               Operation Or;
               Core Return;
             ]);
        Core Grab;
        Core
          (Closure
             [
               Core Grab;
               Plain_old_data (Bool false);
               Core (Access 0);
               Operation Or;
               Core Return;
             ]);
        Core Grab;
        Core
          (Closure
             [
               Core Grab;
               Plain_old_data (Bool true);
               Core (Access 0);
               Operation And;
               Core Return;
             ]);
        Core Grab;
        Core
          (Closure
             [
               Core Grab;
               Plain_old_data (Bool false);
               Core (Access 0);
               Operation And;
               Core Return;
             ]);
        Core Grab;
        Core Grab;
        Core (Access 0);
        Operation Not;
        Core Return;
      ] );
  ]
  |> convert

let prog2_stack = [Types.Stack_item.Z (Plain_old_data (Bool false))]

let prog3 =
  [
    ("a", [Plain_old_data (Num ~$2); Core Return]);
    ( "b",
      [
        Plain_old_data (Num ~$2);
        Core Grab;
        Plain_old_data (Num ~$3);
        Core Return;
      ] );
    ( "lf",
      [
        Plain_old_data (Num ~$2);
        Core Grab;
        Plain_old_data (Num ~$3);
        Core Grab;
        Core Grab;
        Core (Access 0);
        Core Grab;
        Core (Access 0);
        Core Grab;
        Core (Access 0);
        Adt (RecordAccess 1);
        Core Grab;
        Core (Access 1);
        Adt (RecordAccess 0);
        Core Grab;
        Core (Access 1);
        Core (Access 0);
        Operation Or;
        Core Grab;
        Core (Access 0);
        Adt
          (MatchVariant
             [|
               ( [Core Grab; Core (Access 7); Core Return]);
               ( [Core Grab; Core (Access 8); Core Return]);
             |]);
      ] );
  ]
  |> convert

module Executor : Zinc_interpreter.Dummy.Executor = struct
  let get_contract_opt a = Some (a, None)

  let chain_id = "chain id goes here"

  let hash = Fn.id

  let key_hash s = s ^ "hash"
end

let tests () =
  let runner1 = Interpreter.eval (module Executor) in
  let runner2 = Interpreter.eval' (module Executor) ~debug:false in
  let prog1 = Interpreter.initial_state prog1 in
  let prog2 = Interpreter.initial_state ~initial_stack:prog2_stack prog2 in
  let prog3 = Interpreter.initial_state prog3 in
  let test name f = Bench.Test.create f ~name in
  [
    test "old_eval list_cons" (fun _ -> runner1 prog1);
    test "new_eval list_cons" (fun _ -> runner2 prog1);
    test "old_eval bools" (fun _ -> runner1 prog2);
    test "new_eval bools" (fun _ -> runner2 prog2);
    test "old_eval if_then_else" (fun _ -> runner1 prog3);
    test "new_eval if_then_else" (fun _ -> runner2 prog3);
  ]

let () = tests () |> Bench.make_command |> Core.Command.run
