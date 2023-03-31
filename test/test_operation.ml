(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2023 OcamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

let id = "test-operation"

module Lens = Bitpack_serializer.Lens

type t =
  | Var of string
  | Const of int
  | Add of t * t
  | Mul of t * t
  | Sub of t * t

let rec pp fmt = function
  | Var v -> Format.pp_print_string fmt v
  | Const i -> Format.pp_print_int fmt i
  | Add (i, j) -> Format.fprintf fmt "(%a + %a)" pp i pp j
  | Mul (i, j) -> Format.fprintf fmt "(%a * %a)" pp i pp j
  | Sub (i, j) -> Format.fprintf fmt "(%a - %a)" pp i pp j

let lens = Lens.(
    mu @@ fun self ->
    disj  [|
      case
        ~destruct:(function Var v -> Some v | _ -> None)
        ~construct:(fun v -> Var v)
        string;
      case
        ~destruct:(function Const i -> Some i | _ -> None)
        ~construct:(fun i -> Const i)
        int;
      case
        ~destruct:(function Add (o1, o2) -> Some (o1, o2) | _ -> None)
        ~construct:(fun (o1, o2) -> Add (o1, o2))
        (conj self self);
      case
        ~destruct:(function Mul (o1, o2) -> Some (o1, o2) | _ -> None)
        ~construct:(fun (o1, o2) -> Mul (o1, o2))
        (conj self self);
      case
        ~destruct:(function Sub (o1, o2) -> Some (o1, o2) | _ -> None)
        ~construct:(fun (o1, o2) -> Sub (o1, o2))
        (conj self self);
    |]
  )

let to_test = [
  Var "toto",
  Lib.{lens; dictionary = `NoDictionary };

  Const 2,
  Lib.{lens; dictionary = `NoDictionary };

  Add ((Var "toto"), (Const 2)),
  Lib.{lens; dictionary = `NoDictionary };

  Mul ((Var "toto"), (Add ((Const 2), (Var "foo")))),
  Lib.{lens; dictionary = `NoDictionary };

  Add(
    Add (Var "foo", Const 4),
    Mul (
      Sub (Var "foo", Var "foo"),
      Const 0
    )
  ),
  Lib.{lens; dictionary = `Dictionary (Some 1) };
]

let eq = (=)
