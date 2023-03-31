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

(* Simulates a simple HTML component. *)
type t = {
  name: string;
  id: string;
  cls: string list;
  children: t list;
}

let rec pp fmt {name; id; cls; children} =
  Format.fprintf fmt
    "<%s id='%s' class='%a'>%a</%s>"
    name
    id
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
       Format.pp_print_string)
    cls
    (Format.pp_print_list pp)
    children
    name

let eq = (=)

let make name ~cls ~id children = {name; cls; id; children}
let div = make "div"
let p = make "p"
let span = make "span"

let to_tup t = (t.name, t.id), (t.cls, t.children)
let of_tup ((name, id), (cls, children)) = make name ~cls ~id children

let lens = Lens.(
    mu (fun self ->
        trans
          of_tup
          to_tup
          (conj
             (conj string string)
             (conj (list string) (list self))
          )
      ))

let to_test = [
  div ~cls:["main-class"; "bordered"] ~id:"main-div" [
    p ~cls:["editable"] ~id:"p1" [];
    span ~cls:["red-text"; "foo"; "bordered"] ~id:"s1" [];
    p ~cls:["non-editable"; "bordered"; "foo"; "main-class"] ~id:"p2" [];
  ]
  ,
  Lib.{lens; dictionary = `NoDictionary};
]
