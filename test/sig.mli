(**************************************************************************)
(*                                                                        *)
(*  Copyright (c) 2019-2022 OcamlPro                                      *)
(*                                                                        *)
(*  All rights reserved.                                                  *)
(*  This file is distributed under the terms of the GNU Lesser General    *)
(*  Public License version 2.1, with the special exception on linking     *)
(*  described in the LICENSE.md file in the root directory.               *)
(*                                                                        *)
(*                                                                        *)
(**************************************************************************)

module type S = sig
  val id : string

  type t

  val to_test: (t * t Lib.config) list

  val pp : Format.formatter -> t -> unit

  val eq : t -> t -> bool
end
