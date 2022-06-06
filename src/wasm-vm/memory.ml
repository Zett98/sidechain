open Wasm

type t = Memory.t

let load t ~address = Memory.load_byte t address

let store_bytes t ~address ~content =
  Memory.store_bytes t address (Bytes.to_string content)

let load_bytes t ~address ~size =
  let loaded = Memory.load_bytes t address size in
  Bytes.of_string loaded
