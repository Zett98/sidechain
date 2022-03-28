open Crypto
module Raw : sig
  module Script : sig
    type t [@@deriving yojson]
    val make : string -> (t, string) result
  end
  module Value : sig
    type t [@@deriving yojson]
    val make : string -> (t, string) result
  end
end

module Origination_payload : sig
  type t [@@deriving yojson]
  val make_lambda :
    code:Raw.Script.t -> storage:Raw.Value.t -> (t, string) result
end

module Contract : sig
  type t [@@deriving yojson, eq]
  val to_string : t -> string
  val originated_by : t -> Key_hash.t
  module Compile : sig
    val compile_script :
      Origination_payload.t ->
      gas:int ->
      on_error:(string -> int -> 'a) ->
      originated_by:Key_hash.t ->
      (t * int, 'a) result
  end
end
