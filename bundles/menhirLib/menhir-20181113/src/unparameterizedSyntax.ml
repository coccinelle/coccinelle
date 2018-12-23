(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU General Public License version 2, as described in the    *)
(*  file LICENSE.                                                             *)
(*                                                                            *)
(******************************************************************************)

open Syntax

(* This is the abstract syntax for an unparameterized grammar, that is,
   a grammar that does not have any parameterized nonterminal symbols.
   Such a grammar is obtained as the result of an expansion phase, which
   is implemented in [ParameterizedGrammar]. *)

(* In an unparameterized grammar, %attribute declarations can be desugared
   away. This is also done during the above-mentioned expansion phase.

   Thus, in an unparameterized grammar, attributes can be attached in the
   following places:

   - with the grammar:          field [gr_attributes] of [grammar]
   - with a terminal symbol:    field [tk_attributes] of [token_properties]
   - with a nonterminal symbol: field [attributes] of [rule]
   - with a producer:           field [producer_attributes] of [producer]   *)

(* ------------------------------------------------------------------------ *)

(* A producer is a pair of identifier and a symbol. In concrete syntax, it
   could be [e = expr], for instance. It carries a number of attributes. *)

type producer = {
    producer_identifier : identifier;
    producer_symbol     : symbol;
    producer_attributes : attributes;
  }

type producers =
  producer list

(* ------------------------------------------------------------------------ *)

(* A branch contains a series of producers and a semantic action. It is the
   same as in the surface syntax; see [Syntax]. *)

type branch = {
    branch_position         : Positions.t;
    producers               : producers;
    action                  : action;
    branch_prec_annotation  : branch_prec_annotation;
    branch_production_level : branch_production_level
  }

(* ------------------------------------------------------------------------ *)

(* A rule consists mainly of several brahches. In contrast with the surface
   syntax, it has no parameters. *)

(* The [%inline] flag is no longer relevant after [NonTerminalInlining]. *)

type rule = {
    branches    : branch list;
    positions   : Positions.t list;
    inline_flag : bool;
    attributes  : attributes;
  }

(* ------------------------------------------------------------------------ *)

(* A grammar is essentially the same as in the surface syntax; see [Syntax].
   The main difference is that [%attribute] declarations, represented by
   the field [p_symbol_attributes] in the surface syntax, have disappeared. *)

type grammar =  {
    preludes        : Stretch.t list;
    postludes       : Syntax.postlude list;
    parameters      : Stretch.t list;
    start_symbols   : StringSet.t;
    types           : Stretch.ocamltype StringMap.t;
    tokens          : Syntax.token_properties StringMap.t;
    on_error_reduce : on_error_reduce_level StringMap.t;
    gr_attributes   : attributes;
    rules           : rule StringMap.t;
  }

(* -------------------------------------------------------------------------- *)

(* Accessors for the type [producer]. *)

let producer_identifier { producer_identifier } = producer_identifier
let producer_symbol { producer_symbol } = producer_symbol
let producer_attributes { producer_attributes } = producer_attributes

(* -------------------------------------------------------------------------- *)

(* [tokens grammar] is a list of all (real) tokens in the grammar
   [grammar]. The special tokens "#" and "error" are not included.
   Pseudo-tokens (used in %prec declarations, but never declared
   using %token) are filtered out. *)

let tokens grammar =
  StringMap.fold (fun token properties tokens ->
    if properties.tk_is_declared then token :: tokens else tokens
  ) grammar.tokens []

(* [typed_tokens grammar] is analogous, but includes the OCaml type
   of each token. *)

let typed_tokens grammar =
  StringMap.fold (fun token properties tokens ->
    if properties.tk_is_declared then (token, properties.tk_ocamltype) :: tokens else tokens
  ) grammar.tokens []

(* [nonterminals grammar] is a list of all nonterminal symbols in the
   grammar [grammar]. *)

let nonterminals grammar : nonterminal list =
  StringMap.fold (fun nt _ rules -> nt :: rules) grammar.rules []

(* [ocamltype_of_symbol grammar symbol] produces the OCaml type
   of the symbol [symbol] in the grammar [grammar], if it is known. *)

let ocamltype_of_symbol grammar symbol : Stretch.ocamltype option =
  try
    Some (StringMap.find symbol grammar.types)
  with Not_found ->
    None

(* [ocamltype_of_start_symbol grammar symbol] produces the OCaml type
   of the start symbol [symbol] in the grammar [grammar]. *)

let ocamltype_of_start_symbol grammar symbol : Stretch.ocamltype =
  try
    StringMap.find symbol grammar.types
  with Not_found ->
    (* Every start symbol should have a type. *)
    assert false
