module type S = Zinc_types_intf.S

module type Domain_types = Zinc_types_intf.Domain_types

module Make (D : Domain_types) :
  S
    with type Zinc.Hash.t := D.Hash.t
     and type Zinc.Address.t := D.Address.t
     and type Zinc.Contract.t := D.Contract.t
     and type Zinc.Key.t := D.Key.t
[@@warning "-67"]

module Raw :
  S
    with type Zinc.Hash.t := string
     and type Zinc.Address.t := string
     and type Zinc.Contract.t := string * string option
     and type Zinc.Key.t := string
