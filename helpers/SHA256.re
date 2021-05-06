open Mirage_crypto.Hash.SHA256;

[@deriving yojson]
type t('a) = {
  hash: string,
  data: 'a,
};

let to_hex = str => {
  let `Hex(str) = Hex.of_string(str);
  str;
};

let hash = data => {
  let hash = {
    let data = Marshal.to_string(data, []);
    Cstruct.of_string(data)
    |> feed(empty)
    |> get
    |> Cstruct.to_string
    |> to_hex;
  };
  {hash, data};
};
let verify = (~hash as expected_hash, data) => {
  let t = hash(data);
  t.hash == expected_hash ? Ok(t) : Error("Invalid hash");
};

let of_yojson = (f, json) =>
  Result.bind(of_yojson(f, json), ({hash, data}) => verify(~hash, data));
