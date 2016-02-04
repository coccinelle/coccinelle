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

(* This module deals with a few details regarding the definition of
   the [token] type. In particular, if [--only-tokens] was specified,
   it emits the type definition and exits. *)

open Grammar

(* This is the conventional name of the [token] type, for use by
   the code generator. *)

val tctoken: string
val ttoken: IL.typ

(* This is the type of lexers. It refers to the [token] type,
   which is why it is defined here. *)

val tlexer: IL.typ

(* This is the definition of the type of tokens, for use by the
   code generator. *)

val tokentypedef: IL.typedef list

(* This function prefixes the name of a token with an appropriate
   Objective Caml module name, if necessary. *)

val tokenprefix: string -> string

