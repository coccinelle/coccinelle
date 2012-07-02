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

(* This maps a [Position] keyword to the name of the variable that the
   keyword is replaced with. *)

val posvar: subject -> where -> flavor -> string

(* Sets of keywords. *)
module KeywordSet : 
  sig 
    include Set.S

    (* This converts a list of keywords with positions into a set of keywords. *)
    val from_list: elt list -> t

  end with type elt = keyword

(* (\* TEMPORARY These functions are replaced by the ones found  *)
(*    in [Action]. *\) *)
(* (\* These iterate over a list of keywords with positions, disregarding *)
(*    the positions, and making sure that duplicate elements are not *)
(*    presented. *\) *)

(* val iter: (keyword -> unit) -> (keyword * 'a * 'b) list -> unit *)
(* val fold: (keyword -> 'c -> 'c) -> (keyword * 'a * 'b) list -> 'c -> 'c *)

(* (\* These tell whether a list of keywords with positions contains *)
(*    a certain keyword. *\) *)
  
(* val has_previouserror: (keyword * 'a * 'b) list -> bool *)
(* val has_syntaxerror: (keyword * 'a * 'b) list -> bool *)
(* val has_leftstart: (keyword * 'a * 'b) list -> bool *)
(* val has_leftend: (keyword * 'a * 'b) list -> bool *)
(* val has_dollar: int -> (keyword * 'a * 'b) list -> bool *)
