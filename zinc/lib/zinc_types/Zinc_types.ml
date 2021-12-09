open Zinc_utils
include Zinc_types_intf

module Make (D : Domain_types) :
  S
    with type Zinc.Hash.t := D.Hash.t
     and type Zinc.Address.t := D.Address.t
     and type Zinc.Contract.t := D.Contract.t
     and type Zinc.Key.t := D.Key.t = struct
  module Zinc = struct
    open D

    module Hash = struct
      include Hash

      let pp fmt t = Format.fprintf fmt "%s" (to_string t)
    end

    module Address = struct
      include Address

      let pp fmt t = Format.fprintf fmt "%s" (to_string t)
    end

    module Key = struct
      include Key

      let pp fmt t = Format.fprintf fmt "%s" (to_string t)
    end

    module Contract = struct
      include Contract

      let pp fmt t = Format.fprintf fmt "%s" (to_string t)
    end

    type core_instruction =
      | Grab
      | Return
      | PushRetAddr of t
      | Apply
      | Access of int
      | Closure of t
      | EndLet
    [@@deriving show {with_path = false}, eq, yojson]

    and plain_old_data =
      | Bool of bool
      | String of string
      | Num of Z.t
      | Mutez of Z.t
      | Nil
      | Bytes of bytes
      | Address of Address.t
      | Key of Key.t
      | Hash of Hash.t
    [@@deriving show {with_path = false}, eq, yojson]

    and adt =
      | MakeRecord of int
      | RecordAccess of label
      | MakeVariant of variant_label
      | MatchVariant of (variant_label * t) list
    [@@deriving show {with_path = false}, eq, yojson]

    and operation = Eq | Add | Cons | HashKey
    [@@deriving show {with_path = false}, eq, yojson]

    and domain_specific_operation = ChainID | Contract_opt | MakeTransaction
    [@@deriving show {with_path = false}, eq, yojson]

    and control_flow = Failwith
    [@@deriving show {with_path = false}, eq, yojson]

    and instruction =
      (*
      Everything in here should be safe and trustworthy. Our assumption is that an adversary
      can create whatever zinc they want and provide it as code to the interpreter.
      The code is guaranteed
  *)
      | Core of core_instruction
      | Plain_old_data of plain_old_data
      | Adt of adt
      | Operation of operation
      | Domain_specific_operation of domain_specific_operation
      | Control_flow of control_flow
    [@@deriving show {with_path = false}, eq, yojson]

    and t = instruction list [@@deriving show {with_path = false}, eq, yojson]

    let to_string = show

    let instruction_to_string = show_instruction

    (*
    Not all zinc values can be expressed directly in code as literals.
    So they're represented as a seperate type.
  *)
    type nonliteral_value =
      | Contract of Contract.t
      | Chain_operation of chain_operation
    [@@deriving show {with_path = false}, eq, yojson]

    and chain_operation =
      | Transaction of Z.t * Contract.t (* todo: add parameter *)
    [@@deriving show {with_path = false}, eq, yojson]
  end

  module Program = struct
    type t = (string * Zinc.t) list
    [@@deriving show {with_path = false}, eq, yojson]

    let to_string = show
  end

  module rec Env_item : sig
    type t =
      | Z of Zinc.instruction
      | NonliteralValue of Zinc.nonliteral_value
      | Clos of Clos.t
      | Record of Stack_item.t LMap.t
      | List of Stack_item.t list
      | Variant of variant_label * Stack_item.t

    include Zinc_types_intf.With_default_derivation with type t := t
  end = struct
    type t =
      | Z of Zinc.instruction
      | NonliteralValue of Zinc.nonliteral_value
      | Clos of Clos.t
      | Record of Stack_item.t LMap.t
      | List of Stack_item.t list
      | Variant of variant_label * Stack_item.t
    [@@deriving show {with_path = false}, eq, yojson]

    let to_string = show
  end

  and Stack_item : sig
    type t =
      | Z of Zinc.instruction
      | NonliteralValue of Zinc.nonliteral_value
      | Clos of Clos.t
      | Record of t LMap.t
      | List of t list
      | Variant of variant_label * t
      | Marker of Zinc.t * Env_item.t list

    include Zinc_types_intf.With_default_derivation with type t := t
  end = struct
    type t =
      | Z of Zinc.instruction
      | NonliteralValue of Zinc.nonliteral_value
      | Clos of Clos.t
      | Record of t LMap.t
      | List of t list
      | Variant of variant_label * t
      | Marker of Zinc.t * Env_item.t list
    [@@deriving show {with_path = false}, eq, yojson]

    let to_string = show
  end

  and Clos : sig
    type t = {code : Zinc.t; env : Env_item.t list}

    include Zinc_types_intf.With_default_derivation with type t := t
  end = struct
    type t = {code : Zinc.t; env : Env_item.t list}
    [@@deriving show {with_path = false}, eq, yojson]

    let to_string = show
  end

  module Env = struct
    type t = Env_item.t list [@@deriving show {with_path = false}, eq, yojson]

    let to_string = show
  end

  module Stack = struct
    type t = Stack_item.t list [@@deriving show {with_path = false}, eq, yojson]

    let to_string = show
  end

  module Interpreter_input = struct
    type t = Zinc.t * Env.t * Stack.t
    [@@deriving show {with_path = false}, eq, yojson]

    let to_string = show
  end

  module Interpreter_output = struct
    type t = Success of Env.t * Stack.t | Failure of string
    [@@deriving show {with_path = false}, eq, yojson]

    let to_string = show
  end

  module Utils = struct
    let unit_record_stack = Stack_item.Record LMap.empty

    let unit_record_env = Env_item.Record LMap.empty
  end
end

module Exec = struct
  module Address = struct
    type t = string [@@deriving show, eq, yojson]

    let _ = pp

    let to_string = Fun.id

    let equal (t1 : t) (t2 : t) = equal t1 t2

    let of_string a = Some a
  end

  module Key = struct
    type t = string [@@deriving show, eq, yojson]

    let _ = pp

    let to_string = Fun.id

    let of_string a = Some a
  end

  module Hash = struct
    type t = string [@@deriving show, eq, yojson]

    let _ = pp

    let to_string = Fun.id

    let of_string a = Some a
  end

  module Contract = struct
    type t = string * string option [@@deriving show, eq, yojson]

    let _ = pp

    let to_string x = to_yojson x |> Yojson.Safe.to_string

    let of_string x = Yojson.Safe.from_string x |> of_yojson |> Result.to_option
  end
end

module Raw :
  S
    with type Zinc.Hash.t := string
     and type Zinc.Address.t := string
     and type Zinc.Contract.t := string * string option
     and type Zinc.Key.t := string =
  Make (Exec)
