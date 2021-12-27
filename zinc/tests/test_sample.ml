[@@@warning "-40"]

module Zinc_types = Zinc_types.Raw

let zinc =
  Zinc_types.Zinc.
    [
      Plain_old_data (Address "tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV");
      Core Grab;
      Core (Access 0);
      Domain_specific_operation Contract_opt;
      Core Grab;
      Core (Access 0);
      Adt
        (MatchVariant
           [|
             ( [Core Grab; Core (Access 0)]);
             (
               [
                 Core Grab;
                 Plain_old_data (String "Not a contract");
                 Control_flow Failwith;
               ] );
           |]);
      Core EndLet;
      Core Grab;
      Plain_old_data (String "my string");
      Plain_old_data
        (Key "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav");
      Core (Access 0);
      Plain_old_data (Mutez (Z.of_int 10));
      Adt (MakeRecord 0);
      Domain_specific_operation MakeTransaction;
      Adt (MakeRecord 3);
      Core Return;
    ]
  |> Zinc_types.Zinc.to_yojson

module Executor : Zinc_interpreter.Dummy.Executor = struct
  let get_contract_opt a = Some (a, None)

  let chain_id = "chain id goes here"

  let hash = Fun.id

  let key_hash s = s ^ "hash"
end

let%expect_test _ =
  let open Zinc_interpreter.Dummy in
  let open Interpreter in
  let zinc = Types.Zinc.of_yojson zinc |> Result.get_ok in
  let run_resutl = eval (module Executor) (initial_state zinc) in
  Types.Interpreter_output.to_string run_resutl |> print_endline ;
  [%expect
    {|
          interpreting:
          code:  [(Plain_old_data (Address tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV));
            (Core Grab); (Core (Access 0)); (Domain_specific_operation Contract_opt);
            (Core Grab); (Core (Access 0));
            (Adt
               (MatchVariant
                  [|("Some", [(Core Grab); (Core (Access 0))]);
                    ("None",
                     [(Core Grab); (Plain_old_data (String "Not a contract"));
                       (Control_flow Failwith)])
                    |]));
            (Core EndLet); (Core Grab); (Plain_old_data (String "my string"));
            (Plain_old_data
               (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav));
            (Core (Access 0)); (Plain_old_data (Mutez 10)); (Adt (MakeRecord 0));
            (Domain_specific_operation MakeTransaction); (Adt (MakeRecord 3));
            (Core Return)]
          env:   []
          stack: []
          interpreting:
          code:  [(Core Grab); (Core (Access 0)); (Domain_specific_operation Contract_opt);
            (Core Grab); (Core (Access 0));
            (Adt
               (MatchVariant
                  [|("Some", [(Core Grab); (Core (Access 0))]);
                    ("None",
                     [(Core Grab); (Plain_old_data (String "Not a contract"));
                       (Control_flow Failwith)])
                    |]));
            (Core EndLet); (Core Grab); (Plain_old_data (String "my string"));
            (Plain_old_data
               (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav));
            (Core (Access 0)); (Plain_old_data (Mutez 10)); (Adt (MakeRecord 0));
            (Domain_specific_operation MakeTransaction); (Adt (MakeRecord 3));
            (Core Return)]
          env:   []
          stack: [(Z (Plain_old_data (Address tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV)))]
          interpreting:
          code:  [(Core (Access 0)); (Domain_specific_operation Contract_opt); (Core Grab);
            (Core (Access 0));
            (Adt
               (MatchVariant
                  [|("Some", [(Core Grab); (Core (Access 0))]);
                    ("None",
                     [(Core Grab); (Plain_old_data (String "Not a contract"));
                       (Control_flow Failwith)])
                    |]));
            (Core EndLet); (Core Grab); (Plain_old_data (String "my string"));
            (Plain_old_data
               (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav));
            (Core (Access 0)); (Plain_old_data (Mutez 10)); (Adt (MakeRecord 0));
            (Domain_specific_operation MakeTransaction); (Adt (MakeRecord 3));
            (Core Return)]
          env:   [(Z (Plain_old_data (Address tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV)))]
          stack: []
          interpreting:
          code:  [(Domain_specific_operation Contract_opt); (Core Grab); (Core (Access 0));
            (Adt
               (MatchVariant
                  [|("Some", [(Core Grab); (Core (Access 0))]);
                    ("None",
                     [(Core Grab); (Plain_old_data (String "Not a contract"));
                       (Control_flow Failwith)])
                    |]));
            (Core EndLet); (Core Grab); (Plain_old_data (String "my string"));
            (Plain_old_data
               (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav));
            (Core (Access 0)); (Plain_old_data (Mutez 10)); (Adt (MakeRecord 0));
            (Domain_specific_operation MakeTransaction); (Adt (MakeRecord 3));
            (Core Return)]
          env:   [(Z (Plain_old_data (Address tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV)))]
          stack: [(Z (Plain_old_data (Address tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV)))]
          interpreting:
          code:  [(Core Grab); (Core (Access 0));
            (Adt
               (MatchVariant
                  [|("Some", [(Core Grab); (Core (Access 0))]);
                    ("None",
                     [(Core Grab); (Plain_old_data (String "Not a contract"));
                       (Control_flow Failwith)])
                    |]));
            (Core EndLet); (Core Grab); (Plain_old_data (String "my string"));
            (Plain_old_data
               (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav));
            (Core (Access 0)); (Plain_old_data (Mutez 10)); (Adt (MakeRecord 0));
            (Domain_specific_operation MakeTransaction); (Adt (MakeRecord 3));
            (Core Return)]
          env:   [(Z (Plain_old_data (Address tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV)))]
          stack: [(Variant ("Some",
              (NonliteralValue (Contract ["tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV",null]))
              ))
            ]
          interpreting:
          code:  [(Core (Access 0));
            (Adt
               (MatchVariant
                  [|("Some", [(Core Grab); (Core (Access 0))]);
                    ("None",
                     [(Core Grab); (Plain_old_data (String "Not a contract"));
                       (Control_flow Failwith)])
                    |]));
            (Core EndLet); (Core Grab); (Plain_old_data (String "my string"));
            (Plain_old_data
               (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav));
            (Core (Access 0)); (Plain_old_data (Mutez 10)); (Adt (MakeRecord 0));
            (Domain_specific_operation MakeTransaction); (Adt (MakeRecord 3));
            (Core Return)]
          env:   [(Variant ("Some",
              (NonliteralValue (Contract ["tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV",null]))
              ));
            (Z (Plain_old_data (Address tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV)))]
          stack: []
          interpreting:
          code:  [(Adt
              (MatchVariant
                 [|("Some", [(Core Grab); (Core (Access 0))]);
                   ("None",
                    [(Core Grab); (Plain_old_data (String "Not a contract"));
                      (Control_flow Failwith)])
                   |]));
            (Core EndLet); (Core Grab); (Plain_old_data (String "my string"));
            (Plain_old_data
               (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav));
            (Core (Access 0)); (Plain_old_data (Mutez 10)); (Adt (MakeRecord 0));
            (Domain_specific_operation MakeTransaction); (Adt (MakeRecord 3));
            (Core Return)]
          env:   [(Variant ("Some",
              (NonliteralValue (Contract ["tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV",null]))
              ));
            (Z (Plain_old_data (Address tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV)))]
          stack: [(Variant ("Some",
              (NonliteralValue (Contract ["tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV",null]))
              ))
            ]
          interpreting:
          code:  [(Core Grab); (Core (Access 0)); (Core EndLet); (Core Grab);
            (Plain_old_data (String "my string"));
            (Plain_old_data
               (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav));
            (Core (Access 0)); (Plain_old_data (Mutez 10)); (Adt (MakeRecord 0));
            (Domain_specific_operation MakeTransaction); (Adt (MakeRecord 3));
            (Core Return)]
          env:   [(Variant ("Some",
              (NonliteralValue (Contract ["tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV",null]))
              ));
            (Z (Plain_old_data (Address tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV)))]
          stack: [(NonliteralValue (Contract ["tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV",null]))]
          interpreting:
          code:  [(Core (Access 0)); (Core EndLet); (Core Grab);
            (Plain_old_data (String "my string"));
            (Plain_old_data
               (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav));
            (Core (Access 0)); (Plain_old_data (Mutez 10)); (Adt (MakeRecord 0));
            (Domain_specific_operation MakeTransaction); (Adt (MakeRecord 3));
            (Core Return)]
          env:   [(NonliteralValue (Contract ["tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV",null]));
            (Variant ("Some",
               (NonliteralValue
                  (Contract ["tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV",null]))
               ));
            (Z (Plain_old_data (Address tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV)))]
          stack: []
          interpreting:
          code:  [(Core EndLet); (Core Grab); (Plain_old_data (String "my string"));
            (Plain_old_data
               (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav));
            (Core (Access 0)); (Plain_old_data (Mutez 10)); (Adt (MakeRecord 0));
            (Domain_specific_operation MakeTransaction); (Adt (MakeRecord 3));
            (Core Return)]
          env:   [(NonliteralValue (Contract ["tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV",null]));
            (Variant ("Some",
               (NonliteralValue
                  (Contract ["tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV",null]))
               ));
            (Z (Plain_old_data (Address tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV)))]
          stack: [(NonliteralValue (Contract ["tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV",null]))]
          interpreting:
          code:  [(Core Grab); (Plain_old_data (String "my string"));
            (Plain_old_data
               (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav));
            (Core (Access 0)); (Plain_old_data (Mutez 10)); (Adt (MakeRecord 0));
            (Domain_specific_operation MakeTransaction); (Adt (MakeRecord 3));
            (Core Return)]
          env:   [(Variant ("Some",
              (NonliteralValue (Contract ["tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV",null]))
              ));
            (Z (Plain_old_data (Address tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV)))]
          stack: [(NonliteralValue (Contract ["tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV",null]))]
          interpreting:
          code:  [(Plain_old_data (String "my string"));
            (Plain_old_data
               (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav));
            (Core (Access 0)); (Plain_old_data (Mutez 10)); (Adt (MakeRecord 0));
            (Domain_specific_operation MakeTransaction); (Adt (MakeRecord 3));
            (Core Return)]
          env:   [(NonliteralValue (Contract ["tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV",null]));
            (Variant ("Some",
               (NonliteralValue
                  (Contract ["tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV",null]))
               ));
            (Z (Plain_old_data (Address tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV)))]
          stack: []
          interpreting:
          code:  [(Plain_old_data (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav));
            (Core (Access 0)); (Plain_old_data (Mutez 10)); (Adt (MakeRecord 0));
            (Domain_specific_operation MakeTransaction); (Adt (MakeRecord 3));
            (Core Return)]
          env:   [(NonliteralValue (Contract ["tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV",null]));
            (Variant ("Some",
               (NonliteralValue
                  (Contract ["tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV",null]))
               ));
            (Z (Plain_old_data (Address tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV)))]
          stack: [(Z (Plain_old_data (String "my string")))]
          interpreting:
          code:  [(Core (Access 0)); (Plain_old_data (Mutez 10)); (Adt (MakeRecord 0));
            (Domain_specific_operation MakeTransaction); (Adt (MakeRecord 3));
            (Core Return)]
          env:   [(NonliteralValue (Contract ["tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV",null]));
            (Variant ("Some",
               (NonliteralValue
                  (Contract ["tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV",null]))
               ));
            (Z (Plain_old_data (Address tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV)))]
          stack: [(Z
              (Plain_old_data
                 (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav)));
            (Z (Plain_old_data (String "my string")))]
          interpreting:
          code:  [(Plain_old_data (Mutez 10)); (Adt (MakeRecord 0));
            (Domain_specific_operation MakeTransaction); (Adt (MakeRecord 3));
            (Core Return)]
          env:   [(NonliteralValue (Contract ["tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV",null]));
            (Variant ("Some",
               (NonliteralValue
                  (Contract ["tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV",null]))
               ));
            (Z (Plain_old_data (Address tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV)))]
          stack: [(NonliteralValue (Contract ["tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV",null]));
            (Z
               (Plain_old_data
                  (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav)));
            (Z (Plain_old_data (String "my string")))]
          interpreting:
          code:  [(Adt (MakeRecord 0)); (Domain_specific_operation MakeTransaction);
            (Adt (MakeRecord 3)); (Core Return)]
          env:   [(NonliteralValue (Contract ["tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV",null]));
            (Variant ("Some",
               (NonliteralValue
                  (Contract ["tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV",null]))
               ));
            (Z (Plain_old_data (Address tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV)))]
          stack: [(Z (Plain_old_data (Mutez 10)));
            (NonliteralValue (Contract ["tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV",null]));
            (Z
               (Plain_old_data
                  (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav)));
            (Z (Plain_old_data (String "my string")))]
          interpreting:
          code:  [(Domain_specific_operation MakeTransaction); (Adt (MakeRecord 3));
            (Core Return)]
          env:   [(NonliteralValue (Contract ["tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV",null]));
            (Variant ("Some",
               (NonliteralValue
                  (Contract ["tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV",null]))
               ));
            (Z (Plain_old_data (Address tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV)))]
          stack: [(Record [||]); (Z (Plain_old_data (Mutez 10)));
            (NonliteralValue (Contract ["tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV",null]));
            (Z
               (Plain_old_data
                  (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav)));
            (Z (Plain_old_data (String "my string")))]
          interpreting:
          code:  [(Adt (MakeRecord 3)); (Core Return)]
          env:   [(NonliteralValue (Contract ["tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV",null]));
            (Variant ("Some",
               (NonliteralValue
                  (Contract ["tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV",null]))
               ));
            (Z (Plain_old_data (Address tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV)))]
          stack: [(NonliteralValue
              (Chain_operation
                 (Transaction (10, ["tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV",null]))));
            (Z
               (Plain_old_data
                  (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav)));
            (Z (Plain_old_data (String "my string")))]
          interpreting:
          code:  [(Core Return)]
          env:   [(NonliteralValue (Contract ["tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV",null]));
            (Variant ("Some",
               (NonliteralValue
                  (Contract ["tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV",null]))
               ));
            (Z (Plain_old_data (Address tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV)))]
          stack: [(Record
              [|(NonliteralValue
                   (Chain_operation
                      (Transaction (10, ["tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV",null]))));
                (Z
                   (Plain_old_data
                      (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav)));
                (Z (Plain_old_data (String "my string")))|])
            ]
          (Success (
             [(NonliteralValue (Contract ["tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV",null]));
               (Variant ("Some",
                  (NonliteralValue
                     (Contract ["tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV",null]))
                  ));
               (Z (Plain_old_data (Address tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV)))],
             [(Record
                 [|(NonliteralValue
                      (Chain_operation
                         (Transaction (10,
                            ["tz1TGu6TN5GSez2ndXXeDX6LgUDvLzPLqgYV",null]))));
                   (Z
                      (Plain_old_data
                         (Key edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav)));
                   (Z (Plain_old_data (String "my string")))|])
               ]
             )) |}]
