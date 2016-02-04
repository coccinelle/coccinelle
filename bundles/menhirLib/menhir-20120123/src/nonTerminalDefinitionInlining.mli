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

(* $Id: nonTerminalDefinitionInlining.mli,v 1.5 2005/12/01 16:20:06 regisgia Exp $ *)

(** [inline g] traverses the rules of [g] and inlines the non terminal
    definitions that are marked with [%inline]. It returns a pair of the transformed
    grammar and a flag that tells whether any inlining was actually done. *)
val inline: UnparameterizedSyntax.grammar -> UnparameterizedSyntax.grammar * bool
