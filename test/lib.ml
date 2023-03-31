(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2023 OCamlPro                                           *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

module Buffer = Bitpack_serializer.Buffer
module Lens = Bitpack_serializer.Lens

type 'a config = {
  lens: 'a Lens.t;
  dictionary: Buffer.dictionary_config
}

let write_read
    (config : 'a config)
    (elt : 'a) : 'a * int =
  (* Creates a writer. *)
  let writer =
    Buffer.initialize_writer
      ~with_dict:config.dictionary
      ()
  in
  (* Writes on the buffer. *)
  let () = Lens.write config.lens writer elt in
  (* Concludes the writing. *)
  let buff = Buffer.finalize writer in
  (* Creates a reader. *)
  let reader =
    Buffer.initialize_reader
      ~with_dict:config.dictionary
      buff
  in
  (* Reads the buffer. *)
  let elt' = Lens.read config.lens reader in
  let size = Bytes.length buff in
  elt', size
