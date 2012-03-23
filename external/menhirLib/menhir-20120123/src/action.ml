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

(* $Id: action.ml,v 1.10 2006/06/26 09:41:33 regisgia Exp $ *)

open Keyword

type t = 
    {
      expr	: IL.expr;
      keywords  : Keyword.KeywordSet.t;
      filenames : string list;
      pkeywords : Keyword.keyword Positions.located list
    }

let from_stretch s = 
  { 
    expr      = IL.ETextual s;
    filenames = [ s.Stretch.stretch_filename ];
    keywords  = Keyword.KeywordSet.from_list (List.map Positions.value s.Stretch.stretch_keywords);
    pkeywords = s.Stretch.stretch_keywords;
  }

let parenthesize s = 
  if String.length s < 2 || s.[0] <> '(' || s.[String.length s - 1] <> ')' then
    "(" ^ s ^ ")"
  else 
    s

let rec parenthesize_stretch = function
  | IL.ETextual s ->
      IL.ETextual { s with Stretch.stretch_raw_content = parenthesize s.Stretch.stretch_raw_content }
  | IL.ELet (es, e) ->
      IL.ELet (List.map (fun (p, e) -> (p, parenthesize_stretch e)) es, parenthesize_stretch e)
  | x -> x

let compose x a1 a2 = 
  {
    expr      = IL.ELet ([ IL.PVar x, parenthesize_stretch a1.expr ], a2.expr);
    keywords  = Keyword.KeywordSet.union a1.keywords a2.keywords;
    filenames = a1.filenames @ a2.filenames;
    pkeywords = a1.pkeywords @ a2.pkeywords;
  }

let rename_inlined_psym (psym, first_prod, last_prod) phi l =
  List.fold_left
    (fun (l, phi, (used1, used2)) pk ->
       match pk.Positions.value with
	 | Position (subject, where, flavor) ->
	     let (subject', where'), (used1, used2) = 
	       match subject, where with
		 | RightNamed s, w  -> 
		     (* In the host rule, $startpos(x) is changed 
			to $startpos(first_prod) (same thing for $endpos). *)
		     if s = psym then
		       match w with
			 | WhereStart -> first_prod, (true, used2)
			 | WhereEnd -> last_prod, (used1, true)
		     else 
		       (* Otherwise, we just that the renaming into account. *)
		       let s' = try 
			 List.assoc s phi
		       with Not_found -> s 
		       in
			 (RightNamed s', w), (used1, used2)
		 | _ -> (subject, where), (used1, used2)
	     in
	     let from_pos = Keyword.posvar subject where flavor
	     and to_pos = Keyword.posvar subject' where' flavor in
	       (Positions.with_pos pk.Positions.position 
		  (Position (subject', where', flavor)) :: l,
		(if from_pos <> to_pos && not (List.mem_assoc from_pos phi) then 
		   (from_pos, to_pos) :: phi else phi),
		(used1, used2))

	 | _ -> pk :: l, phi, (used1, used2)
    )
    ([], phi, (false, false)) l

(* Rename the keywords related to position to handle the composition
   of semantic actions during non terminal inlining. 

   The first argument describes the context: 
   - [first_prod] is the first producer that starts the action's rule.
   - [last_prod] is the last one.
   For instance, if %inline rule r is A -> B C and rule r' is D -> E A F,
   then [first_prod] is B and [last_prod] is C. 
   If r is A -> and r' is unchanged. [first_prod] is E and [last_prod] is F.
   - [psym] is the producer that is being inlined.
   
*)
let rename_pkeywords (psym, first_prod, last_prod) phi l = 
  List.fold_left (fun (l, phi, (used1, used2)) pk -> match pk.Positions.value with
	    | Position (subject, where, flavor) ->
		let (subject', where'), (used1, used2) = 
		  match subject, where with
		      (* $startpos is changed to $startpos(first_prod) in the 
			 inlined rule. *)
		    | Left, WhereStart -> first_prod, (true, used2)
		      (* Similarly for $endpos. *)
		    | Left, WhereEnd   -> last_prod, (used1, true)
		      (* $i cannot be combined with inlining. *)
		    | RightDollar i, w -> assert false
		    | RightNamed s, w  -> 
			(* In the host rule, $startpos(x) is changed to 
			   to $startpos(first_prod) (same thing for $endpos). *)
			if s = psym then
			  match w with
			    | WhereStart -> first_prod, (true, used2)
			    | WhereEnd -> last_prod, (used1, true)
			else 
			  (* Otherwise, we just that the renaming into account. *)
			  let s' = try List.assoc s phi with Not_found -> s in
			    (RightNamed s', w), (used1, used2)
		in
		let from_pos = Keyword.posvar subject where flavor
		and to_pos = Keyword.posvar subject' where' flavor in
		  (Positions.with_pos pk.Positions.position 
		     (Position (subject', where', flavor)) :: l,
		   (if from_pos <> to_pos && not (List.mem_assoc from_pos phi) then 
		      (from_pos, to_pos) :: phi else phi), 
		   (used1, used2))

	    | x -> pk :: l, phi, (used1, used2))

    ([], phi, (false, false)) l
		
let rename renaming_fun renaming_env phi a = 
  let pkeywords, phi, used_fg = renaming_fun renaming_env phi a.pkeywords in
  { a with 
      (* We use the let construct to rename without modification of the semantic
	 action code. *)
      expr = 
      IL.ELet (List.map (fun (x, x') -> (IL.PVar x, IL.EVar x')) phi, 
	       a.expr);

      (* Keywords related to positions are updated too. *)
      keywords = 
      List.fold_left 
	(fun acu pk -> Keyword.KeywordSet.add pk.Positions.value acu) 
	Keyword.KeywordSet.empty
	pkeywords;

      pkeywords = pkeywords
  }, used_fg

let rename_inlined_psym =
  rename rename_inlined_psym

let rename =
  rename rename_pkeywords

let to_il_expr action = 
  action.expr

let filenames action = 
  action.filenames

let keywords action = 
  action.keywords

let pkeywords action = 
  action.pkeywords

let rec print f action = 
  let module P = Printer.Make (struct let f = f 
				      let locate_stretches = None 
				      let raw_stretch_action = true
			       end) 
  in
    P.expr action.expr

let has_previouserror action =
  KeywordSet.mem PreviousError (keywords action)

let has_syntaxerror action =
  KeywordSet.mem SyntaxError (keywords action)

let has_leftstart action =
  KeywordSet.exists (function
    | Position (Left, WhereStart, _) ->
	true
    | _ ->
	false
  ) (keywords action)

let has_leftend action =
  KeywordSet.exists (function
    | Position (Left, WhereEnd, _) ->
	true
    | _ ->
	false
  ) (keywords action)

let has_dollar i action =
  KeywordSet.exists (function
    | Dollar j when i = j ->
	true
    | _ ->
	false
  ) (keywords action)

let use_dollar action = 
  KeywordSet.exists (function
    | Dollar _ ->
	true
    | _ ->
	false
  ) (keywords action)
    
    


