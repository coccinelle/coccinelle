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

open Syntax
open UnparameterizedSyntax
open IL
open CodeBits

(* This is the conventional name of the [token] type, with no
   prefix. A prefix is possibly appended to it below, where
   [tctoken] is redefined before being exported. *)

let tctoken =
  "token"

(* This is the definition of the type of tokens. *)

let tokentypedef =
  let datadefs =
    StringMap.fold (fun token properties defs ->

      (* Pseudo-tokens (used in %prec declarations, but never
	 declared using %token) are filtered out. *)

      if properties.tk_is_declared then
	let params =
	  match properties.tk_ocamltype with
	  | None ->
	      []
	  | Some t ->
	      [ TypTextual t ]
	in
	{
	  dataname = token;
	  datavalparams = params;
	  datatypeparams = None
	} :: defs
      else
	defs
    ) PreFront.grammar.tokens []
  in
  {
    typename = tctoken;
    typeparams = [];
    typerhs = TDefSum datadefs;
    typeconstraint = None
  }

(* Consult the command line options to determine what to do.
   If we were asked to only produce a type definition, then
   do so and stop. Otherwise, tell the code generator whether
   it should produce a type definition as part of the code. *)

let tokentypedef, tokenprefix =
  match Settings.token_type_mode with
  | Settings.TokenTypeOnly ->

      (* Create both an .mli file and an .ml file. This is made
	 necessary by the fact that the two can be different
	 when there are functor parameters. *)

      let module P = 
	Printer.Make (struct 
			let f = open_out (Settings.base ^ ".mli")
			let raw_stretch_action = false
			let locate_stretches = None 
			let parenthesize_let_lhs = false
		      end) 
      in
      P.interface {
        paramdecls = PreFront.grammar.parameters;
        excdecls = [];
	typedecls = [ tokentypedef ];
	valdecls = []
      };
      let module P = 
	Printer.Make (struct 
			let f = open_out (Settings.base ^ ".ml")
			let raw_stretch_action = false
			let locate_stretches = None 
		      end) 
      in
      P.program {
        paramdefs = PreFront.grammar.parameters;
        prologue = [];
        excdefs = [];
	typedefs = [ tokentypedef ];
        nonrecvaldefs = [];
	valdefs = [];
	moduledefs = [];
        postlogue = [];
      };
      exit 0
  | Settings.CodeOnly m ->
      [],
      (fun id -> m ^ "." ^ id)
  | Settings.TokenTypeAndCode ->
      [ tokentypedef ],
      (fun id -> id)

(* Redefine the name of the [token] type to take a possible
   prefix into account. *)

let tctoken =
  tokenprefix tctoken

let ttoken =
  TypApp (tctoken, [])

(* The type of lexers. *)

let tlexer =
  TypArrow (tlexbuf, ttoken)

