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

(* The unit type. *)

let tunit =
  TypApp ("unit", [])

(* The integer type. *)

let tint =
  TypApp ("int", [])

(* The string type. *)

let tstring =
  TypApp ("string", [])

(* The exception type. *)

let texn =
  TypApp ("exn", [])

(* The type of lexer positions. *)

let tposition =
  TypApp ("Lexing.position", [])

(* The type of lexer buffers. *)

let tlexbuf =
  TypApp ("Lexing.lexbuf", [])

(* The type of untyped semantic values. *)

let tobj =
  TypApp ("Obj.t", [])

(* Building a type variable. *)

let tvar x : typ =
  TypVar x

(* Building a type scheme. *)

let scheme qs t =
  {
    quantifiers = qs;
    body = t
  } 

(* Building a type scheme with no quantifiers out of a type. *)

let type2scheme t =
  scheme [] t

let pat2var = function
  | PVar x ->
      x
  | _ ->
      assert false

(* [simplify] removes bindings of the form [let v = v in ...] and
   [let _ = v in ...]. *)

let rec simplify = function
  | [] ->
      []
  | (PVar v1, EVar v2) :: bindings when v1 = v2 ->
      (* Avoid a useless let binding. *)
      simplify bindings
  | (PWildcard, EVar _) :: bindings ->
      (* Avoid a useless let binding. *)
      simplify bindings
  | binding :: bindings ->
      binding :: simplify bindings

(* Building a [let] construct, with on-the-fly simplification. *)

let rec blet (bindings, body) =
  match simplify bindings with
  | [] ->
      body
  | bindings ->
      ELet (bindings, body)

let mlet formals actuals body =
  blet (List.combine formals actuals, body)

(* [bottom] is an expression that has every type. Its semantics is
   irrelevant. *)

let bottom =
  ERaise (EData ("Not_found", []))

(* Boolean constants. *)

let efalse : expr =
  EData ("false", [])

let etrue : expr =
  EData ("true", [])

let eboolconst b =
  if b then etrue else efalse

(* These help build function types. *)

let arrow typ body : typ =
  TypArrow (typ, body)

let arrowif flag typ body : typ =
  if flag then
    arrow typ body
  else
    body

let marrow typs body : typ =
  List.fold_right arrow typs body

(* ------------------------------------------------------------------------ *)
(* Here is a bunch of naming conventions. Our names are chosen to minimize
   the likelihood that a name in a semantic action is captured. In other
   words, all global definitions as well as the parameters to [reduce]
   are given far-fetched names, unless [--no-prefix] was specified. Note
   that the prefix must begin with '_'. This allows avoiding warnings
   about unused variables with ocaml 3.09 and later. *)

let prefix name =
  if Settings.noprefix then
    name
  else
    "_menhir_" ^ name

let dataprefix name =
  if Settings.noprefix then
    name
  else
    "Menhir" ^ name

let tvprefix name =
  if Settings.noprefix then
    name
  else
    "ttv_" ^ name
