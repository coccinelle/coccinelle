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

(* This module defines many internal naming conventions for use by the
   two code generators, [CodeBackend] and [TableBackend]. It also offers
   a few code generation facilities. *)

open IL
open CodeBits
open Grammar

(* ------------------------------------------------------------------------ *)

(* Naming conventions. *)

(* The type variable associated with a nonterminal [nt]. *)

let ntvar nt =
  Infer.ntvar (Nonterminal.print true nt)

(* The variable that holds the environment. This is a parameter to all
   functions. We do not make it a global variable because we wish to
   preserve re-entrancy. *)

let env =
   prefix "env"

(* A variable used to hold a semantic value. *)

let semv =
  "_v"

(* A variable used to hold a stack. *)

let stack =
  prefix "stack"

(* A variable used to hold a state. *)

let state =
  prefix "s"

(* A variable used to hold a token. *)

let token =
  "_tok"

(* Variables used to hold start and end positions. Do not change these
   names! They are chosen to coincide with the $startpos and $endpos
   keywords, which the lexer rewrites to _startpos and _endpos, so
   that binding these variables before executing a semantic action is
   meaningful. *)

let startp =
  "_startpos"

let endp =
  "_endpos"

(* ------------------------------------------------------------------------ *)

(* Types for semantic values. *)

(* [semvtypent nt] is the type of the semantic value associated
   with nonterminal [nt]. *)

let semvtypent nt =
  match Nonterminal.ocamltype nt with
  | None ->

      (* [nt] has unknown type. If we we have run [Infer], then this
	 can't happen. However, running type inference is only an
	 option, so we still have to deal with that case. *)

      TypVar (ntvar nt)

  | Some ocamltype ->

      (* [nt] has known type. *)

      TypTextual ocamltype

(* [semvtypetok tok] is the type of the semantic value associated with
   token [tok]. There is no such type if the token does not have a
   semantic value. *)

let semvtypetok tok =
  match Terminal.ocamltype tok with
  | None ->

      (* Token has unit type and is omitted in stack cell. *)

      []

  | Some ocamltype ->

      (* Token has known type. *)

      [ TypTextual ocamltype ]

(* [semvtype symbol] is the type of the semantic value associated with
   [symbol]. *)

let semvtype = function
  | Symbol.T tok ->
      semvtypetok tok
  | Symbol.N nt ->
      [ semvtypent nt ]

(* [symvalt] returns the empty list if the symbol at hand carries no
   semantic value and the singleton list [[f t]] if it carries a
   semantic value of type [t]. *)

let symvalt symbol f =
  match semvtype symbol with
  | [] ->
      []
  | [ t ] ->
      [ f t ]
  | _ ->
      assert false

(* [symval symbol x] returns either the empty list or the singleton
   list [[x]], depending on whether [symbol] carries a semantic
   value. *)

let symval symbol x =
  match semvtype symbol with
  | [] ->
      []
  | [ t ] ->
      [ x ]
  | _ ->
      assert false

(* [tokval] is a version of [symval], specialized for terminal symbols. *)

let tokval tok x =
  symval (Symbol.T tok) x

(* ------------------------------------------------------------------------ *)

(* Patterns for tokens. *)

(* [tokpat tok] is a pattern that matches the token [tok], without binding
   its semantic value. *)

let tokpat tok =
  PData (TokenType.tokenprefix (Terminal.print tok), tokval tok PWildcard)

(* [tokpatv tok] is a pattern that matches the token [tok], and binds
   its semantic value, if it has one, to the variable [semv]. *)

let tokpatv tok =
  PData (TokenType.tokenprefix (Terminal.print tok), tokval tok (PVar semv))

(* [tokspat toks] is a pattern that matches any token in the set [toks],
   without binding its semantic value. *)

let tokspat toks =
  POr (
    TerminalSet.fold (fun tok pats ->
      tokpat tok :: pats
    ) toks []
  )

(* [destructuretokendef name codomain bindsemv branch] generates the
   definition of a function that destructures tokens. [name] is the
   name of the function that is generated. [codomain] is its return
   type. [bindsemv] tells whether the variable [semv] should be
   bound. [branch] is applied to each (non-pseudo) terminal and must
   produce code for each branch. *)

let destructuretokendef name codomain bindsemv branch = {
  valpublic = false;
  valpat = PVar name;
  valval =
    EAnnot (
      EFun ([ PVar token ],
	EMatch (EVar token,
	  Terminal.fold (fun tok branches ->
	    if Terminal.pseudo tok then
	      branches
	    else
	      { branchpat = (if bindsemv then tokpatv else tokpat) tok;
		branchbody = branch tok } :: branches
	  ) []
	)
      ),
      type2scheme (arrow TokenType.ttoken codomain)
    )
}

(* ------------------------------------------------------------------------ *)

(* Bindings for exotic keywords. *)

(* [extrabindings fpreviouserror action] provides definitions for the
   [$startofs], [$endofs], and [$previouserror] keywords, if required
   by a semantic action. The parameter [fpreviouserror] is the name of
   the [previouserror] field in the environment -- the table-based and
   code-based back-ends use different names. The parameter [action] is
   the semantic action within which these keywords might be used. *)

(* The [ofs] keyword family is defined in terms of the [pos] family by
   accessing the [pos_cnum] field. The [$previouserror] keyword simply
   provides access to the current value of [env.previouserror]. *)

let extrabindings fpreviouserror action =
  Keyword.KeywordSet.fold (fun keyword bindings ->
    match keyword with
    | Keyword.Dollar _
    | Keyword.Position (_, _, Keyword.FlavorPosition)
    | Keyword.SyntaxError ->
	bindings
    | Keyword.Position (s, w, (Keyword.FlavorOffset as f)) ->
	(PVar (Keyword.posvar s w f),
	 ERecordAccess (EVar (Keyword.posvar s w Keyword.FlavorPosition), "Lexing.pos_cnum")) :: bindings
    | Keyword.PreviousError ->
	(PVar "_previouserror", ERecordAccess (EVar env, fpreviouserror)) :: bindings
  ) (Action.keywords action) []

(* ------------------------------------------------------------------------ *)

(* A global variable holds the exception [Error]. *)

(* We preallocate the [Error] exception and store it into a global
   variable. This allows saving code at the sites where the exception
   is raised. Don't change the conventional name [_eRR], it is shared
   with the lexer, which replaces occurrences of the [$syntaxerror]
   keyword with [(raise _eRR)]. *)

let parse_error =
  "_eRR"

let errorval =
  EVar parse_error

let excvaldef = {
  valpublic = false;
  valpat = PVar parse_error;
  valval = EData (Interface.excname, [])
} 

