(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2019-2022 OCamlPro                                      *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

type 'a config = {
  lens: 'a Bitpack_serializer.Lens.t;
  dictionary: Bitpack_serializer.Buffer.dictionary_config
}

(** [write_read ?eq config elt]
    Serializes [elt] with the configuration parameters set in [config]
    and deserializes the result.
    Returns the result as well as the size of the serialization buffer. *)
val write_read :
  'a config ->
  'a ->
  'a * int
