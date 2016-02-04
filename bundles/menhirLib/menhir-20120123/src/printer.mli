(**************************************************************************)
(*                                                                        *)
(*  Menhir                                                                *)
(*                                                                        *)
(*  François Pottier, INRIA Rocquencourt                                  *)
(*  Yann Régis-Gianas, PPS, Université Paris Diderot                      *)
(*                                                                        *)
(*  Copyright 2005-2008 Institut National de Recherche en Informatique    *)
(*  et en Automatique. All rights reserved. This file is distributed      *)
(*  under the terms of the Q Public License version 1.0, with the change  *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(**************************************************************************)

(* A pretty-printer for [IL]. *)

module Make (X : sig

  (* This is the channel that is being written to. *)

  val f: out_channel

  (* If [raw_stretch_action] is set, then we print the semantic actions 
     as they are found into the original source code. *)
  val raw_stretch_action: bool

  (* This controls the way we print Objective Caml stretches (types and
     semantic actions). We either surround them with #line directives
     (for better error reports if the generated code is ill-typed) or
     don't (for better readability). The value is either [None] -- do
     not provide #line directives -- or [Some filename] -- do provide
     them. [filename] is the name of the file that is being written. *)

  val locate_stretches: string option

end) : sig

  val program: IL.program -> unit

  val expr: IL.expr -> unit

  val interface: IL.interface -> unit

end

