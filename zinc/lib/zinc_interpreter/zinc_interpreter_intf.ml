module type With_string = sig
  type t

  val of_string : string -> t option

  val to_string : t -> string
end

module type With_yojson = sig
  type t

  val of_yojson : Yojson.Safe.t -> t Ppx_deriving_yojson_runtime.error_or

  val to_yojson : t -> Yojson.Safe.t
end

module type With_eq = sig
  type t

  val equal : t -> t -> bool
end

module type Executor = sig
  module Key : sig
    (*Crypto.Key*)
    type t

    include With_string with type t := t

    include With_yojson with type t := t

    include With_eq with type t := t
  end

  module Contract : sig
    (* Tezos.Contract - TODO *)
    type t

    include With_string with type t := t

    include With_yojson with type t := t

    include With_eq with type t := t
  end

  module Address : sig
    (*Tezos.Address*)
    type t

    include With_string with type t := t

    include With_yojson with type t := t

    include With_eq with type t := t

    val get_contract_opt : t -> Contract.t option
  end

  module Hash : sig
    (*Crypto.BLAKE2B*)
    type t

    include With_string with type t := t

    include With_yojson with type t := t

    include With_eq with type t := t

    val hash : string -> t
  end
end
