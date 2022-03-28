open Helpers
open Smart_contracts
open Tezos
module M = Map.Make_with_yojson (Contract_hash)

type t = Contract.t M.t [@@deriving yojson, eq]

let empty = M.empty

let originate_contract t ~address ~contract = M.add address contract t

let update_contract_storage t ~address ~updated_contract =
  M.update address (Option.map (fun _ -> updated_contract)) t

let get_contract t ~address = M.find_opt address t
