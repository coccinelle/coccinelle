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

(* This module provides a number of tiny functions that help produce
   [IL] code. *)

open IL

(* Standard types. *)

val tunit: typ
val tint: typ
val tstring: typ
val texn: typ
val tposition: typ
val tlexbuf: typ
val tobj : typ

(* Building a type variable. *)

val tvar: string -> typ

(* Building a type scheme. *)

val scheme: string list -> typ -> typescheme
val type2scheme: typ -> typescheme

(* Projecting out of a [PVar] pattern. *)

val pat2var: pattern -> string

(* Building a [let] construct, with on-the-fly simplification. *)

val blet: (pattern * expr) list * expr -> expr
val mlet: pattern list -> expr list -> expr -> expr

(* [bottom] is an expression that has every type. Its semantics is
   irrelevant. *)

val bottom: expr

(* Boolean constants. *)

val etrue: expr
val efalse: expr
val eboolconst: bool -> expr

(* These help build function types. *)

val arrow: typ -> typ -> typ
val arrowif: bool -> typ -> typ -> typ
val marrow: typ list -> typ -> typ

(* These functions are used to generate names in menhir's namespace. *)
val prefix: string -> string
val dataprefix: string -> string
val tvprefix: string -> string
