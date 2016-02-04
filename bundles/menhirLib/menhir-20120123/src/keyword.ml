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

(* This module provides some type and function definitions
   that help deal with the keywords that we recognize within
   semantic actions. *)

(* ------------------------------------------------------------------------- *)
(* Types. *)

(* The user can request position information either at type
   [int] (a simple offset) or at type [Lexing.position]. *)

type flavor =
  | FlavorOffset
  | FlavorPosition

(* The user can request position information about the
   start or end of a symbol. *)

type where =
  | WhereStart
  | WhereEnd

(* The user can request position information about a production's
   left-hand side or about one of the symbols in its right-hand
   side, which he can refer to by position or by name. *)

type subject =
  | Left
  | RightDollar of int
  | RightNamed of string

(* Keywords inside semantic actions. They allow access to semantic
   values or to position information. *)

type keyword =
  | Dollar of int
  | Position of subject * where * flavor
  | PreviousError
  | SyntaxError

(* ------------------------------------------------------------------------- *)
(* These auxiliary functions help map a [Position] keyword to the
   name of the variable that the keyword is replaced with. *)

let where = function
  | WhereStart ->
      "start"
  | WhereEnd ->
      "end"

let subject = function
  | Left ->
      ""
  | RightDollar i ->
      Printf.sprintf "__%d_" i
  | RightNamed id ->
      Printf.sprintf "_%s_" id

let flavor = function
  | FlavorPosition ->
      "pos"
  | FlavorOffset ->
      "ofs"

let posvar s w f =
  Printf.sprintf "_%s%s%s" (where w) (flavor f) (subject s)

(* ------------------------------------------------------------------------- *)
(* Sets of keywords. *)

module KeywordSet = 
  struct 
    include Set.Make (struct
			type t = keyword
			let compare = compare
		      end)

    (* This converts a list of keywords with positions into a set of keywords. *)
    let from_list keywords =
      List.fold_right add keywords empty

  end

