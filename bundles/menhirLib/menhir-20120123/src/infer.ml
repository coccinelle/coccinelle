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

open Syntax
open Stretch
open UnparameterizedSyntax
open IL
open CodeBits
open TokenType

(* ------------------------------------------------------------------------- *)
(* Naming conventions. *)

(* The type variable associated with a nonterminal symbol. Its name
   begins with a prefix which ensures that it cannot clash with
   Objective Caml keywords. *)

let ntvar symbol =
  Printf.sprintf "tv_%s" (Misc.normalize symbol)

(* The name of the temporary file. *)

let base =
  Settings.base

let mlname =
  base ^ ".ml"

let mliname =
  base ^ ".mli"

(* ------------------------------------------------------------------------- *)
(* Code production. *)

(* [nttype nt] is the type of the nonterminal [nt], as currently
   known. *)

let nttype grammar nt =
   try
     TypTextual (StringMap.find nt grammar.types)
   with Not_found ->
     TypVar (ntvar nt)

(* [is_standard] determines whether a branch derives from a standard
   library definition. The method, based on a file name, is somewhat
   fragile. *)

let is_standard branch =
  List.for_all (fun x -> x = Settings.stdlib_filename) (Action.filenames branch.action)

(* [actiondef] turns a branch into a function definition. *)

let actiondef grammar symbol branch =

  (* Construct a list of the semantic action's formal parameters that
     depend on the production's right-hand side. *)

  let _, formals =
    List.fold_left (fun (i, formals) (symbol, ido) ->
      let id, startp, endp, starto, endo =
	match ido with
	| None ->
	    (* Symbols for which no name was chosen will be represented
	       by variables named _1, _2, etc. *)
	    Printf.sprintf "_%d" (i + 1),
	    Printf.sprintf "_startpos__%d_" (i + 1),
	    Printf.sprintf "_endpos__%d_" (i + 1),
	    Printf.sprintf "_startofs__%d_" (i + 1),
	    Printf.sprintf "_endofs__%d_" (i + 1)
        | Some id ->
	    (* Symbols for which a name was explicitly chosen will be
	       known by that name in semantic actions. *)
	    id,
	    Printf.sprintf "_startpos_%s_" id,
	    Printf.sprintf "_endpos_%s_" id,
	    Printf.sprintf "_startofs_%s_" id,
	    Printf.sprintf "_endofs_%s_" id
      in
      let t =
	try
	  let props = StringMap.find symbol grammar.tokens in
	  (* Symbol is a terminal. *)
	  match props.tk_ocamltype with
	  | None ->
	      tunit
	  | Some ocamltype ->
	      TypTextual ocamltype
	with Not_found ->
	  (* Symbol is a nonterminal. *)
	  nttype grammar symbol
      in
      i + 1,
      PAnnot (PVar id, t) ::
      PAnnot (PVar startp, tposition) ::
      PAnnot (PVar endp, tposition) ::
      PAnnot (PVar starto, tint) ::
      PAnnot (PVar endo, tint) ::
      formals
    ) (0, []) branch.producers
  in

  (* Extend the list with parameters that do not depend on the
     right-hand side. *)

  let formals =
    PAnnot (PVar "_previouserror", tint) ::
    PAnnot (PVar "_eRR", texn) ::
    PAnnot (PVar "_startpos", tposition) ::
    PAnnot (PVar "_endpos", tposition) ::
    PAnnot (PVar "_startofs", tint) ::
    PAnnot (PVar "_endofs", tint) ::
    formals
  in

  (* Construct a function definition out of the above bindings and the
     semantic action. *)

  let body =
    EAnnot (
      Action.to_il_expr branch.action,
      type2scheme (nttype grammar symbol)
    )
  in

  match formals with
  | [] ->
      body
  | _ ->
      EFun (formals, body)

(* [program] turns an entire grammar into a test program. *)

let program grammar =

  (* Turn the grammar into a bunch of function definitions. Grammar
     productions that derive from the standard library are reflected
     first, so that type errors are not reported in them. *)

  let bindings1, bindings2 = 
    StringMap.fold (fun symbol rule (bindings1, bindings2) ->
      List.fold_left (fun (bindings1, bindings2) branch ->
	if is_standard branch then
	  (PWildcard, actiondef grammar symbol branch) :: bindings1, bindings2
	else
	  bindings1, (PWildcard, actiondef grammar symbol branch) :: bindings2
      ) (bindings1, bindings2) rule.branches
    ) grammar.rules ([], [])
  in

  (* Create entry points whose types are the unknowns that we are
     looking for. *)

  let ps, ts =
    StringMap.fold (fun symbol _ (ps, ts) ->
      PVar (Misc.normalize symbol) :: ps,
      nttype grammar symbol :: ts
    ) grammar.rules ([], [])
  in

  let def = {
    valpublic = true;
    valpat = PTuple ps;
    valval = ELet (bindings1 @ bindings2, EAnnot (bottom, type2scheme (TypTuple ts)))
  }
  in

  (* Insert markers to delimit the part of the file that we are
     interested in. These markers are recognized by [Lexmli]. This
     helps skip the values, types, exceptions, etc. that might be
     defined by the prologue or postlogue. *)

  let begindef = {
    valpublic = true;
    valpat = PVar "menhir_begin_marker";
    valval = EIntConst 0
  }
  and enddef = {
    valpublic = true;
    valpat = PVar "menhir_end_marker";
    valval = EIntConst 0
  } in

  (* Issue the test program. We include the definition of the type of
     tokens, because, in principle, the semantic actions may refer to
     it or to its data constructors. *)

  {
    paramdefs = PreFront.grammar.parameters;
    prologue = PreFront.grammar.preludes;
    excdefs = [];
    typedefs = tokentypedef;
    nonrecvaldefs = [ begindef; def; enddef ];
    moduledefs = [];
    valdefs = [];
    postlogue = PreFront.grammar.postludes
  }

(* ------------------------------------------------------------------------- *)
(* Writing the program associated with a grammar to a file. *)

let write grammar () =
  let ml = open_out mlname in
  let module P = Printer.Make (struct
    let f = ml
    let locate_stretches = Some mlname
    let raw_stretch_action = false
  end) in
  P.program (program grammar);
  close_out ml

let remove filename () =
  Sys.remove filename

(* ------------------------------------------------------------------------- *)
(* Moving away and restoring a file. *)

let mover filename =
  if Sys.file_exists filename then
    let newname =
      filename ^ ".moved_by_menhir"
    in
    let moveaway () =
      Sys.rename filename newname
    and restore () =
      Sys.rename newname filename
    in
    moveaway, restore
  else
    let nothing () = () in
    nothing, nothing

(* ------------------------------------------------------------------------- *)
(* Running ocamldep on the program. *)

type entry =
    string (* basename *) * string (* filename *)

type line =
    entry (* target *) * entry list (* dependencies *)

let depend grammar =

  (* Create an [.ml] file and an [.mli] file, then invoke ocamldep to
     compute dependencies for us. *)

  (* If an old [.ml] or [.mli] file exists, we are careful to preserve
     it. We temporarily move it out of the way and restore it when we
     are done. There is no reason why dependency analysis should
     destroy existing files. *)

  let moveml, restoreml =
    mover mlname
  and movemli, restoremli =
    mover mliname
  in

  let output =
    IO.winvoke
      [ moveml; movemli; write grammar; Interface.write ]
      (Printf.sprintf "%s %s %s" Settings.ocamldep (Filename.quote mlname) (Filename.quote mliname))
      [ remove mlname; remove mliname; restoreml; restoremli ]
  in

  (* Echo ocamldep's output. *)

  print_string output;

  (* If [--raw-depend] was specified on the command line, stop here.
     This option is used by omake, which performs its own
     postprocessing of [ocamldep]'s output. For normal [make] users,
     who use [--depend], some postprocessing is required, which is
     performed below. *)

  begin match Settings.depend with
  | Settings.OMNone ->
      assert false (* we wouldn't be here in the first place *)
  | Settings.OMRaw ->
      ()
  | Settings.OMPostprocess ->

      (* Make sense out of ocamldep's output. *)

      let lexbuf = Lexing.from_string output in
      let lines : line list = Lexdep.main lexbuf in

      (* Look for the line that concerns the [.cmo] target, and echo a
	 modified version of this line, where the [.cmo] target is
	 replaced with [.ml] and [.mli] targets, and where the dependency
	 over the [.cmi] file is dropped.

	 In doing so, we assume that the user's [Makefile] supports
	 bytecode compilation, so that it makes sense to request [bar.cmo]
	 to be built, as opposed to [bar.cmx]. This is not optimal, but
	 will do. [camldep] exhibits the same behavior. *)

      (* TEMPORARY allow ocamldep to be called with flag -native. *)

      List.iter (fun ((_, target_filename), dependencies) ->
	if Filename.check_suffix target_filename ".cmo" then
	  let dependencies = List.filter (fun (basename, _) ->
	    basename <> base
	  ) dependencies in
	  if List.length dependencies > 0 then begin
	    Printf.printf "%s.ml %s.mli:" base base;
	    List.iter (fun (basename, filename) ->
	      Printf.printf " %s" filename
	    ) dependencies;
	    Printf.printf "\n%!"
	  end
      ) lines

  end;

  (* Stop. *)

  exit 0

(* ------------------------------------------------------------------------- *)
(* Inferring types for a grammar's nonterminals. *)

let infer grammar =

  (* Invoke ocamlc to do type inference for us. *)

  let output =
    IO.winvoke
      [ write grammar ]
      (Printf.sprintf "%s -c -i %s" Settings.ocamlc (Filename.quote mlname))
      [ remove mlname ]
  in

  (* Make sense out of ocamlc's output. *)

  let env : (string * int * int) list =
    Lexmli.main (Lexing.from_string output)
  in

  let env : (string * ocamltype) list =
    List.map (fun (id, openingofs, closingofs) ->
      id, Inferred (String.sub output openingofs (closingofs - openingofs))
    ) env
  in

  (* Augment the grammar with new %type declarations. *)

  let types =
    StringMap.fold (fun symbol _ types ->
      let ocamltype =
	try
	  List.assoc (Misc.normalize symbol) env
	with Not_found ->
	  assert false
      in
      if StringMap.mem symbol grammar.types then
	(* If there was a declared type, keep it. *)
	types
      else
	(* Otherwise, insert the inferred type. *)
	StringMap.add symbol ocamltype types
    ) grammar.rules grammar.types
  in

  { grammar with types = types }

