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

open Grammar
open Cst

(* Set up all of the information required by the LR engine. Everything is
   read directly from [Grammar] and [Lr1]. *)

module T = struct

  type state =
      Lr1.node
   
  type token =
      Terminal.t

  type terminal =
      Terminal.t

  type semantic_value =
      cst

  let token2terminal (token : token) : terminal =
    token

  let token2value (token : token) : semantic_value =
    CstTerminal token

  let error_terminal =
    Terminal.error

  let error_value =
    CstError

  type production =
      Production.index

  let default_reduction (s : state) defred nodefred env =
    match Invariant.has_default_reduction s with
    | Some (prod, _) ->
	defred env prod
    | None ->
	nodefred env

  let action (s : state) (tok : terminal) value shift reduce fail env =

    (* Check whether [s] has an outgoing shift transition along [tok]. *)

    try

      let s' : state = SymbolMap.find (Symbol.T tok) (Lr1.transitions s) in

      (* There is such a transition. Return either [ShiftDiscard] or
	 [ShiftNoDiscard], depending on the existence of a default
	 reduction on [#] at [s']. *)

      match Invariant.has_default_reduction s' with
      | Some (_, toks) when TerminalSet.mem Terminal.sharp toks ->
	  shift env false tok value s'
      | _ ->
	  shift env true tok value s'
	  
    (* There is no such transition. Look for a reduction. *)

    with Not_found ->
      try

	let prod = Misc.single (TerminalMap.find tok (Lr1.reductions s)) in
	reduce env prod

      (* There is no reduction either. Fail. *)

      with Not_found ->
	fail env

  let goto (s : state) (prod : production) : state =
    try
      SymbolMap.find (Symbol.N (Production.nt prod)) (Lr1.transitions s)
    with Not_found ->
      assert false

  open MenhirLib.EngineTypes

  exception Accept of semantic_value
  exception Error

  type semantic_action =
      (state, semantic_value, token) env -> unit

  let semantic_action (prod : production) : semantic_action =
    fun env ->
      
      (* Check whether [prod] is a start production. *)

      match Production.classify prod with

      (* If it is one, accept. Start productions are of the form S' ->
	 S, where S is a non-terminal symbol, so the desired semantic
	 value is found within the top cell of the stack. *)

      | Some _ ->
	  raise (Accept env.stack.semv)

      (* If it is not, reduce. Pop a suffix of the stack, and use it
	 to construct a new concrete syntax tree node. *)

      | None ->

	  let n = Production.length prod in
	  let values : semantic_value array =
	    Array.make n CstError (* dummy *)
	  and startp : Lexing.position ref =
	    ref Lexing.dummy_pos
	  and endp : Lexing.position ref =
	    ref Lexing.dummy_pos
	  in

	  (* The auxiliary function [pop k stack] pops [k] stack cells
	     and returns a truncated stack. It also updates the automaton's
	     current state, and fills in [values], [startp], and [endp]. *)

	  let rec pop k stack =

	    if k = 0 then

	      (* There are no more stack cells to pop. *)

	      stack

	    else begin

	      (* Fetch a semantic value. *)

	      values.(k - 1) <- stack.semv;

	      (* Pop one cell. The stack must be non-empty. As we pop a cell,
	         change the automaton's current state to the one stored within
		 the cell. (It is sufficient to do this only when [k] is 1.)
	         If this is the first (last) cell that we pop, update [endp]
	         ([startp]). *)

	      let next = stack.next in
	      assert (stack != next);
	      if k = n then begin
		endp := stack.endp
	      end;
	      if k = 1 then begin
		env.current <- stack.state;
		startp := stack.startp
	      end;
	      pop (k - 1) next

	    end

	  in
	  let stack = pop n env.stack in

	  (* Construct and push a new stack cell. The associated semantic
	     value is a new concrete syntax tree. *)

	  env.stack <- {
	    state = env.current;
	    semv = CstNonTerminal (prod, values);
	    startp = !startp;
	    endp = !endp;
	    next = stack
	  }

  (* The reference interpreter performs error recovery if and only if this
     is requested via [--recovery]. *)

  let recovery =
    Settings.recovery

  module Log = struct

    open Printf

    (* I use a reference as a quick and dirty form of parameter passing. *)

    let log =
      ref false

    let maybe action =
      if !log then begin
	action();
	prerr_newline()
      end

    let state s =
      maybe (fun () ->
	fprintf stderr "State %d:" (Lr1.number s)
      )

    let shift tok s' =
      maybe (fun () ->
	fprintf stderr "Shifting (%s) to state %d" (Terminal.print tok) (Lr1.number s')
      )

    let reduce_or_accept prod =
      maybe (fun () ->
	match Production.classify prod with
	| Some _ ->
	    fprintf stderr "Accepting"
	| None ->
	    fprintf stderr "Reducing production %s" (Production.print prod)
      )

    let lookahead_token lexbuf tok =
      maybe (fun () ->
	fprintf stderr "Lookahead token is now %s (%d-%d)"
	  (Terminal.print tok)
	  lexbuf.Lexing.lex_start_p.Lexing.pos_cnum
	  lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum
      )

    let initiating_error_handling () =
      maybe (fun () ->
	fprintf stderr "Initiating error handling"
      )

    let resuming_error_handling () =
      maybe (fun () ->
	fprintf stderr "Resuming error handling"
      )

    let handling_error s =
      maybe (fun () ->
	fprintf stderr "Handling error in state %d" (Lr1.number s)
      )

    let discarding_last_token tok =
      maybe (fun () ->
	fprintf stderr "Discarding last token read (%s)" (Terminal.print tok)
      )

  end

end

(* Instantiate the LR engine with this information. *)

module E =
  MenhirLib.Engine.Make (T)

(* Define a palatable user entry point. *)

let interpret log nt lexer lexbuf =

  (* Find the start state that corresponds to [nt] in the automaton. *)

  let s : Lr1.node =
    try
      ProductionMap.find (Production.startsymbol2startprod nt) Lr1.entry
    with Not_found ->
      assert false
  in

  (* Run the engine. *)
  
  try
    T.Log.log := log;
    Some (E.entry s lexer lexbuf)
  with T.Error ->
    None

