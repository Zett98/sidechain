open Zinc_utils
open Zinc_interpreter_intf

module Make (E : Executor) = struct
  module Types = Zinc_types.Make (E)

  module Interpreter = struct
    open Types

    external env_to_stack : Env_item.t -> Stack_item.t = "%identity"

    external stack_to_env_ext : Stack_item.t -> Env_item.t = "%identity"

    let stack_to_env = function
      | Stack_item.Marker _ ->
          failwith "type error, cant convert a stack_item into an env_item"
      | Stack_item.(
          Clos _ | Record _ | Variant _ | List _ | Z _ | NonliteralValue _) as x
        ->
          stack_to_env_ext x

    module Steps = struct
      type t =
        | Done
        | Internal_error of string
        | Failwith of string
        | Continue of Zinc.t * Env.t * Stack.t
    end

    let initial_state ?initial_stack:(stack = []) a : Interpreter_input.t =
      (a, [], stack)

    let[@warning "-4"] eval (code, env, stack) =
      let apply_once (code : Zinc.t) env stack =
        let () =
          print_endline
            (Format.asprintf
               "interpreting:\ncode:  %s\nenv:   %s\nstack: %s"
               (Zinc.to_string code)
               (Env.to_string env)
               (Stack.to_string stack))
        in
        let open Zinc in
        match (code, env, stack) with
        | (Plain_old_data Nil :: c, env, s) ->
            Steps.Continue (c, env, Stack_item.List [] :: s)
        | (Operation Cons :: c, env, item :: Stack_item.List x :: s) ->
            Steps.Continue (c, env, Stack_item.List (item :: x) :: s)
        | (Core Grab :: c, env, Stack_item.Marker (c', e') :: s) ->
            Steps.Continue
              (c', e', Stack_item.Clos {Clos.code = Core Grab :: c; env} :: s)
        | (Core Grab :: c, env, v :: s) ->
            Steps.Continue (c, stack_to_env v :: env, s)
        | (Core Grab :: _, _, []) -> failwith "nothing to grab!"
        | ( Core Return :: _,
            _,
            Stack_item.Z v :: Stack_item.Marker (c', e') :: s ) ->
            Steps.Continue (c', e', Stack_item.Z v :: s)
        | (Core Return :: _, _, Stack_item.Clos {Clos.code = c'; env = e'} :: s)
          ->
            Steps.Continue (c', e', s)
        | (Core (PushRetAddr c') :: c, env, s) ->
            Steps.Continue (c, env, Stack_item.Marker (c', env) :: s)
        | (Core Apply :: _, _, Stack_item.Clos {Clos.code = c'; env = e'} :: s)
          ->
            Steps.Continue (c', e', s)
        (* Below here is just modern SECD *)
        | (Core (Access n) :: c, env, s) -> (
            let nth = Base.List.nth env n in
            match nth with
            | Some nth -> Steps.Continue (c, env, (nth |> env_to_stack) :: s)
            | None ->
                Steps.Internal_error "Tried to access env item out of bounds")
        | (Core (Closure c') :: c, env, s) ->
            Steps.Continue (c, env, Stack_item.Clos {Clos.code = c'; env} :: s)
        | (Core EndLet :: c, _ :: env, s) -> Steps.Continue (c, env, s)
        (* zinc extensions *)
        (* operations that jsut drop something on the stack haha *)
        | ( (Plain_old_data
               ( Num _ | Address _ | Key _ | Hash _ | Bool _ | String _
               | Mutez _ | Bytes _ ) as v)
            :: c,
            env,
            s ) ->
            Steps.Continue (c, env, Stack_item.Z v :: s)
        (* ADTs *)
        | (Adt (MakeRecord r) :: c, env, s) ->
            let list_split_at ~n lst =
              let rec go n acc = function
                | [] ->
                    if Int.equal n 0 then (acc, [])
                    else
                      raise (Invalid_argument "not enough entries on the list")
                | x when Int.equal n 0 -> (acc, x)
                | x :: xs -> go (n - 1) (x :: acc) xs
              in
              go n [] lst
            in
            let (record, stack) = list_split_at ~n:r s in
            let record_contents = LMap.of_list (List.rev record) in
            Steps.Continue (c, env, Stack_item.Record record_contents :: stack)
        | (Adt (RecordAccess accessor) :: c, env, Stack_item.Record r :: s) ->
            let res =
              match LMap.find r accessor with
              | None -> failwith "field not found"
              | Some x -> Steps.Continue (c, env, x :: s)
            in
            res
        | ( Adt (MatchVariant vs) :: c,
            env,
            Stack_item.Variant (label, item) :: s ) -> (
            match
              Base.List.find_map vs ~f:(fun (match_arm, constructors) ->
                  if String.equal match_arm label then Some constructors
                  else None)
            with
            | None -> Steps.Internal_error "inexhaustive match"
            | Some match_code ->
                Steps.Continue (List.concat [match_code; c], env, item :: s))
        | (Adt (MakeVariant label) :: c, env, value :: s) ->
            Steps.Continue (c, env, Stack_item.Variant (label, value) :: s)
        (* Math *)
        | ( Operation Add :: c,
            env,
            Stack_item.Z (Plain_old_data (Num a))
            :: Stack_item.Z (Plain_old_data (Num b)) :: s ) ->
            Steps.Continue
              (c, env, Stack_item.Z (Plain_old_data (Num (Z.add a b))) :: s)
        | ( Operation Add :: c,
            env,
            Stack_item.Z (Plain_old_data (Mutez a))
            :: Stack_item.Z (Plain_old_data (Mutez b)) :: s ) ->
            Steps.Continue
              (c, env, Stack_item.Z (Plain_old_data (Mutez (Z.add a b))) :: s)
        (* Booleans *)
        | (Operation Eq :: c, env, a :: b :: s) ->
            Steps.Continue
              ( c,
                env,
                Stack_item.Z (Plain_old_data (Bool (Stack_item.equal a b))) :: s
              )
        (* Crypto *)
        | ( Operation HashKey :: c,
            env,
            Stack_item.Z (Plain_old_data (Key _key)) :: s ) ->
            let h = failwith "need to move this into interpreter_context" in
            Steps.Continue (c, env, Stack_item.Z (Plain_old_data (Hash h)) :: s)
        (* Tezos specific *)
        | (Domain_specific_operation ChainID :: c, env, s) ->
            Steps.Continue
              ( c,
                env,
                Stack_item.Z
                  (* TODO: fix this usage of Digestif.BLAKE2B.hmac_string - should use an effect system or smth.
                     Also probably shouldn't use key like this. *)
                  (let h =
                     E.Hash.hash "need to move this into interpreter_context"
                   in
                   Plain_old_data (Hash h))
                :: s )
        | ( Domain_specific_operation Contract_opt :: c,
            env,
            Stack_item.Z (Plain_old_data (Address address)) :: s ) ->
            (* todo: abstract this into a function *)
            let contract =
              match E.Address.get_contract_opt address with
              | Some contract ->
                  Stack_item.Variant
                    ("Some", Stack_item.NonliteralValue (Contract contract))
              | None -> Stack_item.Variant ("None", Utils.unit_record_stack)
            in
            Steps.Continue (c, env, contract :: s)
        | ( Domain_specific_operation MakeTransaction :: c,
            env,
            r
            :: Stack_item.Z (Plain_old_data (Mutez amount))
               :: Stack_item.NonliteralValue (Contract contract) :: s )
          when Stack_item.equal r Utils.unit_record_stack ->
            Steps.Continue
              ( c,
                env,
                Stack_item.NonliteralValue
                  (Chain_operation (Transaction (amount, contract)))
                :: s )
        (* should be unreachable except when program is done *)
        | ([Core Return], _, _) -> Steps.Done
        | ( Control_flow Failwith :: _,
            _,
            Stack_item.Z (Plain_old_data (String s)) :: _ ) ->
            Steps.Failwith s
        (* should not be reachable *)
        | (x :: _, _, _) ->
            Steps.Internal_error
              (Format.asprintf
                 "%s unimplemented!"
                 (Zinc.instruction_to_string x))
        | _ ->
            Steps.Internal_error
              (Format.asprintf
                 "somehow ran out of code without hitting return!")
      in
      let rec loop code env stack =
        match apply_once code env stack with
        | Steps.Done -> Interpreter_output.Success (env, stack)
        | Steps.Failwith s -> Interpreter_output.Failure s
        | Steps.Internal_error s -> failwith s
        | Steps.Continue (code, env, stack) -> loop code env stack
      in
      loop code env stack
  end
end

module Dummy_executor = struct
  module Hash = struct
    type t = string [@@deriving show, eq, yojson]

    let _ = pp

    let to_string = Fun.id

    let of_string (x : string) : t option = Some x

    let hash = Fun.id
  end

  module Contract = struct
    type t = string * string option [@@deriving show, eq, yojson]

    let _ = pp

    let to_string x = to_yojson x |> Yojson.Safe.to_string

    let of_string x = Yojson.Safe.from_string x |> of_yojson |> Result.to_option

    let _ = pp
  end

  module Address = struct
    type t = string [@@deriving show, eq, yojson]

    let _ = pp

    let to_string = Fun.id

    let of_string (x : string) : t option = Some x

    let _ = pp

    let get_contract_opt (_ : t) : Contract.t option = None
  end

  module Key = struct
    type t = string [@@deriving show, eq, yojson]

    let _ = pp

    let to_string = Fun.id

    let of_string (x : string) : t option = Some x

    let _ = pp
  end
end

module Dummy = Make (Dummy_executor)
