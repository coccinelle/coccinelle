(**************************************************************************)
(*                                                                        *)
(*  Menhir                                                                *)
(*                                                                        *)
(*  Fran�ois Pottier, INRIA Rocquencourt                                  *)
(*  Yann R�gis-Gianas, PPS, Universit� Paris Diderot                      *)
(*                                                                        *)
(*  Copyright 2005-2008 Institut National de Recherche en Informatique    *)
(*  et en Automatique. All rights reserved. This file is distributed      *)
(*  under the terms of the Q Public License version 1.0, with the change  *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(**************************************************************************)

(* $Id: partialGrammar.ml,v 1.63 2006/10/22 14:07:57 fpottier Exp $ *)
open Misc
open Syntax
open Stretch
open ConcreteSyntax
open InternalSyntax
open Positions
open Keyword

(* ------------------------------------------------------------------------- *)
(* This adds one declaration [decl], as found in file [filename], to
   the grammar [grammar]. *)

let join_declaration filename (grammar : grammar) decl = 
  match decl.value with

  (* Preludes are stored in an arbitrary order. The order of
     preludes within a single source file is preserved. Same
     treatment for functor parameters. *)

  | DCode code -> 
      { grammar with p_preludes = grammar.p_preludes @ [ code ] }
  | DParameter (Stretch.Declared stretch) -> 
      { grammar with p_parameters = grammar.p_parameters @ [ stretch ] }
  | DParameter (Stretch.Inferred _) ->
      assert false

  (* Token declarations are recorded. Things are made somewhat
     difficult by the fact that %token and %left-%right-%nonassoc
     declarations are independent. *)

  | DToken (ocamltype, terminal) ->
      let token_property = 
	try

	  (* Retrieve any previous definition for this token. *)

	  let token_property =
	    StringMap.find terminal grammar.p_tokens
	  in

	  (* If the previous definition was actually a %token declaration
	     (as opposed to a %left, %right, or %nonassoc specification),
	     signal an error. *)

	  if token_property.tk_is_declared then
	    Error.errorp decl
	      (Printf.sprintf "the token %s has multiple definitions." terminal)

	  (* Otherwise, update the previous definition. *)

	  else 
	    { token_property with 
	      tk_is_declared = true;
	      tk_ocamltype   = ocamltype;
	      tk_filename    = filename;
	      tk_position    = decl.position;
	    }

	with Not_found -> 

	  (* If no previous definition exists, create one. *)

	  { 
	    tk_filename      = filename; 
	    tk_ocamltype     = ocamltype;
	    tk_associativity = UndefinedAssoc;
	    tk_priority      = UndefinedPrecedence;
	    tk_position      = decl.position;
	    tk_is_declared   = true
	  } 

      in
      { grammar with
	p_tokens = StringMap.add terminal token_property grammar.p_tokens }

  (* Start symbols. *)

  | DStart nonterminal ->
      { grammar with
        p_start_symbols = StringMap.add nonterminal decl.position grammar.p_start_symbols }

  (* Type declarations for nonterminals. *)

  | DType (ocamltype, nonterminal) ->
      { grammar with
          p_types = (nonterminal, with_pos (position decl) ocamltype)::grammar.p_types }

  (* Token associativity and precedence. *)

  | DTokenProperties (terminal, assoc, prec) ->

      (* Retrieve the property record for this token, creating one
	 if none existed (but without deeming the token to have been
	 declared). *)

      let token_properties, grammar = 
	try 
	  StringMap.find terminal grammar.p_tokens, grammar
	with Not_found -> 
	  let p = { 
	    tk_filename      = filename; 
	    tk_ocamltype     = None;
	    tk_associativity = UndefinedAssoc;
	    tk_priority      = prec;
	    tk_is_declared   = false;
	    (* Will be updated later. *)
	    tk_position	     = decl.position;
	  } in 
	  p, { grammar with 
	       p_tokens = StringMap.add terminal p grammar.p_tokens }
      in

      (* Reject duplicate precedence declarations. *)

      if token_properties.tk_associativity <> UndefinedAssoc then 
	Error.error
	  [ decl.position; token_properties.tk_position ]
	  (Printf.sprintf "there are multiple precedence declarations for token %s." terminal);

      (* Record the new declaration. *)

      token_properties.tk_priority <- prec;
      token_properties.tk_associativity <- assoc;
      grammar

(* ------------------------------------------------------------------------- *)
(* This stores an optional trailer into a grammar. 
   Trailers are stored in an arbitrary order. *)

let join_trailer trailer grammar = 
  match trailer with
  | None -> 
      grammar 
  | Some trailer -> 
      { grammar with p_postludes = trailer :: grammar.p_postludes }

(* ------------------------------------------------------------------------- *)
(* We rewrite definitions when nonterminals are renamed. The
   renaming [phi] is an association list of names to names. *)

type renaming =
   (nonterminal * nonterminal) list

let identity_renaming = 
  [] 

let rewrite_nonterminal (phi : renaming) nonterminal =
  Misc.support_assoc phi nonterminal

let rewrite_parameter phi parameter = 
  Parameters.map (Positions.map (Misc.support_assoc phi)) parameter

let rewrite_element phi (ido, parameter) =
  ido, rewrite_parameter phi parameter

let rewrite_branch phi ({ pr_producers = producers } as branch) =
  { branch with pr_producers = List.map (rewrite_element phi) producers }

let rewrite_branches phi branches =
  match phi with
  | [] ->
      branches
  | _ ->
      List.map (rewrite_branch phi) branches

let fresh_counter = ref 0 

let names = ref StringSet.empty

let use_name name = 
  names := StringSet.add name !names

let used_name name = 
  StringSet.mem name !names

let rec fresh ?(hint = "v") () = 
  let name = 
    incr fresh_counter;
    hint ^ string_of_int !fresh_counter
  in
    if used_name name then
      fresh ~hint ()
    else (
      use_name name;
      name
    )
      
(* Alpha conversion of [prule]. We rename bound parameters using
   fresh names. *)
let alphaconvert_rule parameters prule = 
  let phi = 
    List.combine parameters (List.map (fun x -> fresh ~hint:x ()) parameters)
  in
    { prule with
	pr_parameters  = List.map (Misc.support_assoc phi) prule.pr_parameters;
	pr_branches    = rewrite_branches phi prule.pr_branches
    }

(* Rewrite a rule taking bounded names into account. We rename parameters
   to avoid capture. *)
let rewrite_rule phi prule = 
  let ids = 
    List.fold_left (fun acu (f, d) -> StringSet.add f (StringSet.add d acu)) 
      StringSet.empty phi 
  in
  let captured_parameters = 
    List.filter (fun p -> StringSet.mem p ids) prule.pr_parameters
  in
  let prule = 
    alphaconvert_rule captured_parameters prule
  in
    { prule with
	pr_nt = rewrite_nonterminal phi prule.pr_nt;
	pr_branches = rewrite_branches phi prule.pr_branches }
      
let rewrite_rules phi rules =
  List.map (rewrite_rule phi) rules

let rewrite_grammar phi grammar =
  (* We assume that [phi] affects only private symbols, so it does
     not affect the start symbols. *)
  if phi = identity_renaming then 
    grammar
  else 
    { grammar with pg_rules = rewrite_rules phi grammar.pg_rules }

(* ------------------------------------------------------------------------- *)
(* To rename (internalize) a nonterminal, we prefix it with its filename.
   This guarantees that names are unique. *)

let is_valid_nonterminal_character = function
  | 'A' .. 'Z' 
  | 'a' .. 'z'
  | '_'
  | '\192' .. '\214'
  | '\216' .. '\246'
  | '\248' .. '\255'
  | '0' .. '9' ->
      true
  | _ ->
      false

let restrict filename =
  let m = String.copy (Filename.chop_suffix filename (if Settings.coq then ".vy" else ".mly")) in
  for i = 0 to String.length m - 1 do
    if not (is_valid_nonterminal_character m.[i]) then
      m.[i] <- '_'
  done;
  m

let rename nonterminal filename = 
  let name = restrict filename ^ "_" ^ nonterminal in
    if used_name name then
      fresh ~hint:name ()
    else 
      (use_name name; name)

(* ------------------------------------------------------------------------- *)
(* A nonterminal is considered public if it is declared using %public
   or %start. *)

let is_public grammar prule =
  prule.pr_public_flag || StringMap.mem prule.pr_nt grammar.p_start_symbols

(* ------------------------------------------------------------------------- *)
type symbol_kind =
    
  (* The nonterminal is declared public at a particular position. *)
  | PublicNonTerminal of Positions.t

  (* The nonterminal is not declared public at a particular position. *)
  | PrivateNonTerminal of Positions.t

  (* The symbol is a token. *)
  | Token of token_properties

  (* We do not know yet what does the symbol means. 
     This is defined in the sequel or it is free in the partial grammar. *)
  | DontKnow of Positions.t

type symbol_table =
    (symbol, symbol_kind) Hashtbl.t

let find_symbol symbols symbol =
  Hashtbl.find symbols symbol

let add_in_symbol_table symbols symbol kind =
  use_name symbol;
  Hashtbl.add symbols symbol kind;
  symbols

let replace_in_symbol_table symbols symbol kind =
  Hashtbl.replace symbols symbol kind;
  symbols

let empty_symbol_table () =
  Hashtbl.create 13

let store_symbol symbols symbol kind = 
  try
    let sym_info = find_symbol symbols symbol in
      match sym_info, kind with
	  
	(* There are two definitions of the same symbol in one
	   particular unit. This is forbidden. *)
	| (PublicNonTerminal p | PrivateNonTerminal p),
	  (PublicNonTerminal p' | PrivateNonTerminal p') ->
	    Error.error [ p; p'] 
	      (Printf.sprintf 
		 "the nonterminal symbol %s is multiply defined."
		 symbol)

	(* The symbol is known to be a token but declared as a non terminal.*)
	| (Token tkp, (PrivateNonTerminal p | PublicNonTerminal p)) 
	| ((PrivateNonTerminal p | PublicNonTerminal p), Token tkp) ->
	    Error.error [ p; tkp.tk_position ]
	      (Printf.sprintf 
		 "The identifier %s is a reference to a token."
		 symbol)

	(* We do not gain any piece of information. *)
	| _, DontKnow _ | Token _, Token _ ->
	    symbols 

	(* We learn that the symbol is a non terminal or a token. *)
	| DontKnow _, _ ->
	    replace_in_symbol_table symbols symbol kind

  with Not_found ->
    add_in_symbol_table symbols symbol kind

let store_used_symbol position tokens symbols symbol =
  try
    store_symbol symbols symbol (Token (StringMap.find symbol tokens))
  with Not_found ->
    store_symbol symbols symbol (DontKnow position)

let non_terminal_is_not_reserved symbol positions = 
  if symbol = "error" then
    Error.error positions
      (Printf.sprintf "%s is reserved and thus cannot be used \
                       as a non-terminal symbol." symbol)

let non_terminal_is_not_a_token tokens symbol positions = 
  try
    let tkp = StringMap.find symbol tokens in
      Error.error (positions @ [ tkp.tk_position ])
      (Printf.sprintf 
	 "The identifier %s is a reference to a token."
	 symbol)
  with Not_found -> ()

let store_public_nonterminal tokens symbols symbol positions =
  non_terminal_is_not_reserved symbol positions;
  non_terminal_is_not_a_token tokens symbol positions;
  store_symbol symbols symbol (PublicNonTerminal (List.hd positions))
      
let store_private_nonterminal tokens symbols symbol positions =
  non_terminal_is_not_reserved symbol positions;
  non_terminal_is_not_a_token tokens symbol positions;
  store_symbol symbols symbol (PrivateNonTerminal (List.hd positions))

let string_of_kind = function
  | PublicNonTerminal p ->
      Printf.sprintf "public (%s)" (Positions.string_of_pos p)

  | PrivateNonTerminal p ->
      Printf.sprintf "private (%s)" (Positions.string_of_pos p)

  | Token tk ->
      Printf.sprintf "token (%s)" tk.tk_filename

  | DontKnow p ->
      Printf.sprintf "only used at (%s)" (Positions.string_of_pos p)

let string_of_symbol_table t = 
  let b = Buffer.create 13 in
  let m = 1 + Hashtbl.fold (fun k v acu -> max (String.length k) acu) t 0 in
  let fill_blank s =
    let s' = String.make m ' ' in
      String.blit s 0 s' 0 (String.length s);
      s'
  in
    Hashtbl.iter (fun k v -> Buffer.add_string b 
		    (Printf.sprintf "%s: %s\n" 
		       (fill_blank k) (string_of_kind v))) t;
    Buffer.contents b

let is_private_symbol t x = 
  try
    match Hashtbl.find t x with
      | PrivateNonTerminal _ ->
	  true
	    
      | _ ->
	  false
  with Not_found -> 
    false

let is_public_symbol t x = 
  try
    match Hashtbl.find t x with
      | PublicNonTerminal _ ->
	  true
	    
      | _ ->
	  false
  with Not_found -> 
    false

let fold_on_private_symbols f init t = 
  Hashtbl.fold 
    (fun k -> function PrivateNonTerminal _ -> (fun acu -> f acu k)
       | _ -> (fun acu -> acu))
    t init

let fold_on_public_symbols f init t = 
  Hashtbl.fold 
    (fun k -> function PublicNonTerminal _ -> (fun acu -> f acu k)
       | _ -> (fun acu -> acu))
    t init

let iter_on_only_used_symbols f t = 
  Hashtbl.iter 
    (fun k -> function DontKnow pos -> f k pos
       | _ -> ())
    t 

let symbols_of grammar (pgrammar : ConcreteSyntax.grammar) = 
  let tokens = grammar.p_tokens in
  let symbols_of_rule symbols prule = 
    let rec store_except_rule_parameters = 
      fun symbols (symbol, parameters) ->
	(* Rule parameters are bound locally, so they are not taken into
	   account. *)
	if List.mem symbol.value prule.pr_parameters then
	  symbols
	else 
	  (* Otherwise, mark this symbol as being used and analyse its
	     parameters. *)
	  List.fold_left 
	    (fun symbols -> function 
	       | ParameterApp (symbol, parameters) -> 
		   store_except_rule_parameters symbols (symbol, parameters)
	       | ParameterVar symbol ->
		   store_except_rule_parameters symbols (symbol, [])
	    )
	    (store_used_symbol symbol.position tokens symbols symbol.value) parameters
    in
      
    (* Analyse each branch. *)
    let symbols = List.fold_left (fun symbols branch ->
      List.fold_left (fun symbols (_, p) -> 
	let symbol, parameters = Parameters.unapp p in
	store_except_rule_parameters symbols (symbol, parameters)
      ) symbols branch.pr_producers
    ) symbols prule.pr_branches
    in
      (* Store the symbol declaration. *)
      if prule.pr_public_flag 
	|| StringMap.mem prule.pr_nt grammar.p_start_symbols then 
	store_public_nonterminal tokens symbols prule.pr_nt prule.pr_positions
      else
	store_private_nonterminal tokens symbols prule.pr_nt prule.pr_positions
  in
    List.fold_left symbols_of_rule (empty_symbol_table ()) pgrammar.pg_rules

let merge_rules tokens symbols pgs = 

  (* Retrieve all the public symbols. *)
  let public_symbols =
    List.fold_left (fold_on_public_symbols (fun s k -> StringSet.add k s)) 
      (StringSet.singleton "error")
      symbols
  in

  (* We check the references in each grammar can be bound to 
     a public symbol. *)
  let _ = 
    List.iter 
      (iter_on_only_used_symbols 
	 (fun k pos -> if not (StringSet.mem k public_symbols) then
	    Error.error [ pos ]
	      (Printf.sprintf "%s is undefined." k)))
      symbols
  in
  (* Detect private symbol clashes and rename them if necessary. *)
  let detect_private_symbol_clashes = 
    fold_on_private_symbols 
      (fun (defined, clashes) symbol ->
	 if StringSet.mem symbol defined 
	   || StringSet.mem symbol public_symbols then
	   (defined, StringSet.add symbol clashes)
	 else 
	   (StringSet.add symbol defined, clashes))
  in 
  let private_symbols, clashes = 
    List.fold_left detect_private_symbol_clashes (StringSet.empty, StringSet.empty) symbols
  in 
  let rpgs = List.map 
    (fun (symbol_table, pg) ->
       let renaming = 
	 StringSet.fold 
	   (fun x phi ->
	      if is_private_symbol symbol_table x then begin
		  let x' = rename x pg.pg_filename in
		    Printf.fprintf stderr
		      "Note: the nonterminal symbol %s (from %s) is renamed %s.\n"
		      x pg.pg_filename x';
		    (x, x') :: phi
		end
	      else phi)
	   clashes []    
       in
	 rewrite_grammar renaming pg)
    pgs
  in
    
    (* Merge public nonterminal definitions 
       and copy private nonterminal definitions. Since the clash between
       private symbols have already been resolved, these copies are safe. *)
    List.fold_left 
      (fun rules rpg -> List.fold_left 
	 (fun rules r -> 
	    let r = 
	      try
		let r' = StringMap.find r.pr_nt rules in
		let positions = r.pr_positions @ r'.pr_positions in
		let ra, ra' = 
		  List.length r.pr_parameters, 
		  List.length r'.pr_parameters 		  
		in
		  (* The arity of the parameterized symbols must be constant.*)
		  if ra <> ra' then 
		    Error.error positions 
		      (Printf.sprintf "symbol %s is defined with arities %d and %d."
			 r.pr_nt ra ra')
		  else if r.pr_inline_flag <> r'.pr_inline_flag then
		    Error.error positions
		      (Printf.sprintf 
			 "not all definitions of %s are marked %%inline." r.pr_nt)
		  else 
		    (* We combine the different branches. The parameters 
		       could have different names, we rename them with
		       the fresh names assigned earlier (see the next 
		       comment). *)
		    let phi = List.combine r.pr_parameters r'.pr_parameters in
		    let rbr = rewrite_branches phi r.pr_branches in
		      { r' with 
			  pr_positions = positions;
			  pr_branches  = rbr @ r'.pr_branches 
		      } 
	      with Not_found ->
		(* We alphaconvert the rule in order to avoid the capture of 
		   private symbols coming from another unit. *)
		alphaconvert_rule r.pr_parameters r
	    in
	      StringMap.add r.pr_nt r rules) rules rpg.pg_rules)
      StringMap.empty rpgs

let empty_grammar =
  {
    p_preludes                = [];
    p_postludes               = [];
    p_parameters              = [];
    p_start_symbols           = StringMap.empty;
    p_types                   = [];
    p_tokens                  = StringMap.empty;
    p_rules                   = StringMap.empty
  }

let join grammar pgrammar =
  let filename = pgrammar.pg_filename in
    List.fold_left (join_declaration filename) grammar pgrammar.pg_declarations
    $$ join_trailer pgrammar.pg_trailer

(* Check that the $i's are consistent, that is, that they are within
   bounds and that they are not used when symbols are explicitly
   named. Check also that no two symbols carry the same name. *)

let check_keywords grammar producers action =
  let length = List.length producers in
    List.iter
      (function keyword ->
	 match Positions.value keyword with
	   | Dollar i
	   | Position (RightDollar i, _, _) ->
	       if i < 1 || i > length then
		 Error.errorp keyword
		   (Printf.sprintf "$%d refers to a nonexistent symbol." i);
	       let ido, param = List.nth producers (i - 1) in
	       begin
		 match ido with
		   | Some { value = id } ->
		       Error.errorp keyword
			 (Printf.sprintf 
			    "please do not say: $%d. Instead, say: %s." i id)
		   | None ->
		       ()
	       end
	   | Position (RightNamed id, _, _) ->
	       let found =
		 ref false 
	       in
	       List.iter (fun (ido, param) ->
		 match ido with
		 | Some { value = id' } when id = id' ->
		     found := true
		 | _ ->
		     ()
	       ) producers;
	       if not !found then
		 Error.errorp keyword
		   (Printf.sprintf "%s refers to a nonexistent symbol." id)
	   | Position (Left, _, _)
	   | PreviousError
	   | SyntaxError ->
	       ()
      ) (Action.pkeywords action)

let check_parameterized_grammar_is_well_defined grammar =

  (* Every start symbol is defined and has a %type declaration. *)
  StringMap.iter 
    (fun nonterminal p ->
       if not (StringMap.mem nonterminal grammar.p_rules) then
	 Error.error [p] (Printf.sprintf "the start symbol %s is undefined." 
			   nonterminal);
       if not (List.exists (function 
                            | ParameterVar { value = id }, _ -> id = nonterminal
                            | _ -> false) grammar.p_types) then
	 Error.error [p]
	   (Printf.sprintf 
	      "the type of the start symbol %s is unspecified." nonterminal);
    ) grammar.p_start_symbols;

  let rec parameter_head_symb = function
    | ParameterVar id -> id
    | ParameterApp (id, _) -> id
  in

  List.iter (fun (symbol, _) ->
    let head_symb = parameter_head_symb symbol in
    if not (StringMap.mem (value head_symb) grammar.p_rules) then
      Error.errorp (Parameters.with_pos symbol)
	(Printf.sprintf
	   "this is a terminal symbol.\n\
             %%type declarations are applicable only to nonterminal symbols."))
    grammar.p_types;

  (* Every reference to a symbol is well defined. *)
  let reserved = [ "error" ] in
  let used_tokens = ref StringSet.empty in
  let mark_token_as_used token = 
    used_tokens := StringSet.add token !used_tokens
  in
  let check_identifier_reference grammar prule s p = 
    (* Mark the symbol as a used token if this is a token. *)
    if StringMap.mem s grammar.p_tokens then
      mark_token_as_used s;
    
    if not (StringMap.mem s grammar.p_rules
	   || StringMap.mem s grammar.p_tokens
	   || List.mem s prule.pr_parameters
	   || List.mem s reserved) then
      Error.error [ p ] (Printf.sprintf "%s is undefined." s)
  in
    StringMap.iter
      (fun k prule -> List.iter

	 (* Check each branch. *)
	 (fun { pr_producers = producers; 
	       pr_branch_shift_precedence = sprec;
	       pr_action = action 
	     } -> ignore (List.fold_left
			    
	    (* Check the producers. *)
            (fun already_seen (id, p) ->
	       let symbol, parameters = Parameters.unapp p in
	       let s = symbol.value and p = symbol.position in
	       let already_seen = 
		 match id with
		     None -> already_seen
		   | Some id -> 
		       (* Check the producer id is unique. *)
		       if StringSet.mem id.value already_seen then
			 Error.error [ id.position ]
			   (Printf.sprintf
			      "there are multiple producers named %s in this sequence." 
			      id.value);
		       StringSet.add id.value already_seen
	       in

		 (* Check that the producer is defined somewhere. *)
		 check_identifier_reference grammar prule s p;
		 StringMap.iter (check_identifier_reference grammar prule) 
		   (List.fold_left Parameters.identifiers StringMap.empty parameters);
		 
		 (* Check the %prec is a valid reference to a token. *)
		 (try
                    if not ((StringMap.find s grammar.p_tokens).tk_is_declared
                           || List.mem s reserved) then 
		      Error.errorp symbol
			(Printf.sprintf "%s has not been declared as a token." s)
		  with Not_found -> ());
		 already_seen
		 		 
            ) StringSet.empty producers);

	    check_keywords grammar producers action;

            match sprec with

              | None -> ()

              | Some terminal ->
		  check_identifier_reference grammar prule 
		    terminal.value terminal.position;

		  (* It is forbidden to use the %prec directive with %inline. *)
		  if prule.pr_inline_flag then
		    Error.errorp terminal
		      "use of %prec is forbidden in an %inlined nonterminal definition.";

		  (* Furthermore, the symbol following %prec must be a valid
		     token identifier. *)  
                  if not (StringMap.mem terminal.value grammar.p_tokens) then
		    Error.errorp terminal
		      (Printf.sprintf "%s is undefined." terminal.value))

	 prule.pr_branches;

	 (* It is forbidden to use %inline on a %start symbol. *)
	 if (prule.pr_inline_flag 
	     && StringMap.mem k grammar.p_start_symbols) then
	   Error.error prule.pr_positions 
	     (Printf.sprintf 
		"%s cannot be both a start symbol and inlined." k);

      ) grammar.p_rules;
    
  (* Check that every token is used. *)
  begin match Settings.token_type_mode with
  | Settings.TokenTypeOnly ->
      ()
  | Settings.TokenTypeAndCode
  | Settings.CodeOnly _ ->
      StringMap.iter (fun token { tk_position = p } -> 
	if not (StringSet.mem token !used_tokens) then
	  Error.warning [p] 
	    (Printf.sprintf "the token %s is unused." token)
      ) grammar.p_tokens
  end;
    
  grammar

let join_partial_grammars pgs =
  let grammar = List.fold_left join empty_grammar pgs in
  let symbols = List.map (symbols_of grammar) pgs in
  let tpgs = List.combine symbols pgs in
  let rules = merge_rules grammar.p_tokens symbols tpgs in 
  check_parameterized_grammar_is_well_defined { grammar with p_rules = rules }
