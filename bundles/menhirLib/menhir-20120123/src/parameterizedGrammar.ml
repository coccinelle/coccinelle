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

open Positions
open Syntax
open UnparameterizedSyntax
open InternalSyntax
open Misc

(* Inference for non terminals. *)

(* Unification variables convey [variable_info] to describe
   the multi-equation they take part of. *)
type variable_info = 
    {
      mutable structure : nt_type option;
      mutable name      : string option;
      mutable mark      : Mark.t
    }

(* [UnionFind] is used to improve the union and the equality test
   between multi-equations. *)
and variable = variable_info UnionFind.point

(* Types are simple types. 
   [star] denotes the type of ground symbol (non terminal or terminal).
   [Arrow] describes the type of a parameterized non terminal. *)
and nt_type = 
    Arrow of variable list 

let star =
  Arrow []

(* [var_name] is a name generator for unification variables. *)
let var_name =
  let name_counter = ref (-1) in
  let next_name () = 
    incr name_counter;
    String.make 1 (char_of_int (97 + !name_counter mod 26))
    ^ let d = !name_counter / 26 in if d = 0 then "" else string_of_int d
  in
    fun v -> 
      let repr = UnionFind.find v in 
	match repr.name with
	    None -> let name = next_name () in repr.name <- Some name; name
	  | Some x -> x

(* [string_of_nt_type] is a simple pretty printer for types (they can be 
   recursive). *)

(* 2011/04/05: types can no longer be recursive, but I won't touch the printer -fpottier *)

let string_of paren_fun ?paren ?colors t : string = 
  let colors = 
    match colors with 
	None    -> (Mark.fresh (), Mark.fresh ()) 
      | Some cs -> cs 
  in
  let s, p = paren_fun colors t in
    if paren <> None && p = true then 
      "("^ s ^")"
    else s

let rec paren_nt_type ((white, black) as colors) = function
    
    Arrow [] -> 
      "*", false

  | Arrow ins ->
      let args = separated_list_to_string 
	(string_of paren_var ~paren:true ~colors) ", " ins 
      in
      let args = 
	if List.length ins > 1 then
	  "("^ args ^ ")"
	else 
	  args
      in
	args^" -> *", true
	
and paren_var (white, black) x = 
  let descr = UnionFind.find x in
    if Mark.same descr.mark white then begin
      descr.mark <- black;
      var_name x, false
    end 
    else begin 
      descr.mark <- white;
      let s, p = match descr.structure with
	  None -> var_name x, false
	| Some t -> paren_nt_type (white, black) t
      in
	if Mark.same descr.mark black then
	  (var_name x ^ " = " ^ s, true)
	else 
	  (s, p)
    end

let string_of_nt_type ?paren ?colors t = 
  string_of ?colors paren_nt_type t

let string_of_var ?paren ?colors v = 
  string_of ?colors paren_var v

(* [print_env env] returns a string description of the typing environment. *)
let print_env = 
  List.iter (fun (k, (_, v)) -> 
	       Printf.eprintf "%s: %s\n" k (string_of_var v))

(* [occurs_check x y] checks that [x] does not occur within [y]. *)

let dfs action x =

  let black = Mark.fresh () in

  let rec visit_var x =
    let descr = UnionFind.find x in 
    if not (Mark.same descr.mark black) then begin
      descr.mark <- black;
      action x;
      match descr.structure with
      | None ->
	  ()
      | Some t -> 
	  visit_term t
    end

  and visit_term (Arrow ins) =
    List.iter visit_var ins

  in
  visit_var x

exception OccursError of variable * variable

let occurs_check x y =
  dfs (fun z -> if UnionFind.equivalent x z then raise (OccursError (x, y))) y

(* First order unification. *)

(* 2011/04/05: perform an eager occurs check and prevent the construction
   of any cycles. *)

let fresh_flexible_variable () = 
  UnionFind.fresh { structure = None; name = None; mark = Mark.none }

let fresh_structured_variable t = 
  UnionFind.fresh { structure = Some t; name = None; mark = Mark.none }

let star_variable =
  fresh_structured_variable star

exception UnificationError of nt_type * nt_type
exception BadArityError of int * int

let rec unify_var toplevel x y =
  if not (UnionFind.equivalent x y) then
    let reprx, repry = UnionFind.find x, UnionFind.find y in
      match reprx.structure, repry.structure with
	  None, Some t    -> occurs_check x y; UnionFind.union x y
	| Some t, None    -> occurs_check y x; UnionFind.union y x
	| None, None      -> UnionFind.union x y
	| Some t, Some t' -> unify toplevel t t'; UnionFind.union x y
	    
and unify toplevel t1 t2 = 
  match t1, t2 with

    | Arrow ins, Arrow ins' ->
	let n1, n2 = List.length ins, List.length ins' in
	if n1 <> n2 then
	  if n1 = 0 || n2 = 0 || not toplevel then
	    raise (UnificationError (t1, t2))
	  else
	    (* the flag [toplevel] is used only here and influences which
	       exception is raised; BadArityError is raised only at toplevel *)
	    raise (BadArityError (n1, n2));
	List.iter2 (unify_var false) ins ins'

let unify_var x y =
  unify_var true x y

(* Typing environment. *)
type environment =
    (string * (Positions.t list * variable)) list

(* [lookup x env] returns the type related to [x] in the typing environment
   [env]. 
   By convention, identifiers that are not in [env] are terminals. They are
   given the type [Star]. *)
let lookup x (env: environment) = 
  try 
    snd (List.assoc x env)
  with Not_found -> star_variable

(* This function checks that the symbol [k] has the type [expected_type]. *)
let check positions env k expected_type =
  let inference_var = lookup k env in
  let checking_var = fresh_structured_variable expected_type in
    try
      unify_var inference_var checking_var
    with 
	UnificationError (t1, t2) ->
	  Error.error
	    positions
	    (Printf.sprintf 
	       "How is this symbol parameterized?\n\
	      It is used at sorts %s and %s.\n\
              The sort %s is not compatible with the sort %s."
	       (string_of_var inference_var) (string_of_var checking_var)
	       (string_of_nt_type t1) (string_of_nt_type t2))
	    
      | BadArityError (n1, n2) ->
	  Error.error
	    positions
	    (Printf.sprintf
	       "does this symbol expect %d or %d arguments?" 
	       (min n1 n2) (max n1 n2))

      | OccursError (x, y) ->
	  Error.error
	    positions
	    (Printf.sprintf 
	       "How is this symbol parameterized?\n\
	      It is used at sorts %s and %s.\n\
              The sort %s cannot be unified with the sort %s."
	       (string_of_var inference_var) (string_of_var checking_var)
	       (string_of_var x) (string_of_var y))
	  


(* An identifier can be used either in a total application or as a
   higher-order non terminal (no partial application is allowed). *)
let rec parameter_type env = function
  | ParameterVar x ->
      lookup x.value env

  | ParameterApp (x, args) ->
      assert (args <> []);
      let expected_type =
	(* [x] is applied, it must be to the exact number 
	   of arguments. *)
	Arrow (List.map (parameter_type env) args) 
      in
	(* Check the well-formedness of the application. *)
	check [x.position] env x.value expected_type;

	(* Similarly, if it was a total application the result is 
	   [Star] otherwise it is the flexible variable. *)
	star_variable

let check_grammar p_grammar = 
  (* [n] is the grammar size. *)
  let n        = StringMap.cardinal p_grammar.p_rules in

  (* The successors of the non terminal [N] are its producers. It 
     induce a graph over the non terminals and its successor function
     is implemented by [successors]. Non terminals are indexed using
     [nt].
  *) 
  let nt, conv, iconv = index_map p_grammar.p_rules in
  let parameters, name, branches, positions = 
    (fun n -> (nt n).pr_parameters), (fun n -> (nt n).pr_nt),
    (fun n -> (nt n).pr_branches), (fun n -> (nt n).pr_positions)
  in
    
  (* The successors function is implemented as an array using the 
     indexing previously created. *)
  let successors = 
    Array.init n (fun node -> 
      (* We only are interested by parameterized non terminals. *)
      if parameters node <> [] then
	List.fold_left (fun succs { pr_producers = symbols } ->
	  List.fold_left (fun succs -> function (_, p) -> 
	    let symbol, _ = Parameters.unapp p in
	    try 
	      let symbol_node = conv symbol.value in
		(* [symbol] is a parameterized non terminal, we add it 
		   to the successors set. *)
		if parameters symbol_node <> [] then
		  IntSet.add symbol_node succs
		else 
		  succs
	    with Not_found -> 
	      (* [symbol] is a token, it is not interesting for type inference
		 purpose. *)
	      succs
	  ) succs symbols
        ) IntSet.empty (branches node)
      else
	Misc.IntSet.empty
    )
  in

  (* The successors function and the indexing induce the following graph 
     module. *)
  let module RulesGraph = 
      struct

	type node = int

	let n = n

	let index node = 
	  node

	let successors f node = 
	  IntSet.iter f successors.(node)

	let iter f = 
	  for i = 0 to n - 1 do 
	    f i
	  done

      end
  in
  let module ConnectedComponents = Tarjan.Run (RulesGraph) in
    (* We check that:
       - all the parameterized definitions of a particular component
       have the same number of parameters.
       - every parameterized non terminal definition always uses
       parameterized definitions of the same component with its
       formal parameters.
    
       Components are marked during the traversal:
       -1 means unvisited
       n with n > 0 is the number of parameters of the clique.
    *)
  let unseen = -1 in
  let marked_components = Array.create n unseen in
    
  let flexible_arrow args =
    let ty = Arrow (List.map (fun _ -> fresh_flexible_variable ()) args) in
      fresh_structured_variable ty 
  in

  (* [nt_type i] is the type of the i-th non terminal. *)
  let nt_type i =
    match parameters i with
      | [] -> 
	  star_variable
	    
      | x -> 
	  flexible_arrow x
  in

  (* [actual_parameters_as_formal] is the well-formedness checker for 
     parameterized non terminal application. *)
  let actual_parameters_as_formal actual_parameters formal_parameters = 
    List.for_all2 (fun y -> (function ParameterVar x -> x.value = y 
			      | _ -> false)) 
      formal_parameters actual_parameters
  in

  (* The environment is initialized. *)
  let env : environment = StringMap.fold  
    (fun k r acu -> 
       (k, (r.pr_positions, nt_type (conv k))) 
       :: acu)
    p_grammar.p_rules []
  in

    (* We traverse the graph checking each parameterized non terminal
       definition is well-formed. *)
    RulesGraph.iter 
      (fun i ->
	 let params    = parameters i 
	 and iname     = name i 
	 and repr      = ConnectedComponents.representative i 
	 and positions = positions i
	 in

	 (* The environment is augmented with the parameters whose types are
	    unknown. *)
	 let env' = List.map 
	   (fun k -> (k, (positions, fresh_flexible_variable ()))) params
	 in
	 let env = env' @ env in
	   
	 (* The type of the parameterized non terminal is constrained to be
	    [expected_ty]. *)
	 let check_type () = 
	   check positions env iname (Arrow (List.map (fun (_, (_, t)) -> t) env'))
	 in

	 (* We check the number of parameters. *)
	 let check_parameters () = 
	   let parameters_len = List.length params in
	     (* The component is visited for the first time. *)
	     if marked_components.(repr) = unseen then
	       marked_components.(repr) <- parameters_len
	     else (* Otherwise, we check that the arity is homogeneous 
		     in the component. *) 
	       if marked_components.(repr) <> parameters_len then 
		 Error.error positions
		   (Printf.sprintf 
		      "Mutually recursive definitions must have the same parameters.\n\
                       This is not the case for %s and %s."
			 (name repr) iname)
	 in

	(* In each production rule, the parameterized non terminal
	   of the same component must be instantiated with the same
	   formal arguments. *)
	 let check_producers () =
	   List.iter 
	     (fun { pr_producers = symbols } -> List.iter 
		(function (_, p) ->
		   let symbol, actuals = Parameters.unapp p in
		   (* We take the use of each symbol into account. *)
		     check [ symbol.position ] env symbol.value 
		       (if actuals = [] then star else 
			  Arrow (List.map (parameter_type env) actuals));
		   (* If it is in the same component, check in addition that
		      the arguments are the formal arguments. *)
		   try 
		     let idx = conv symbol.value in 
		       if ConnectedComponents.representative idx = repr then
			 if not (actual_parameters_as_formal actuals params)
			 then
			   Error.error [ symbol.position ]
			     (Printf.sprintf
				"Mutually recursive definitions must have the same \
                                 parameters.\n\
				 This is not the case for %s."
				 (let name1, name2 = (name idx), (name i) in
				    if name1 <> name2 then name1 ^ " and "^ name2
				    else name1))
		   with _ -> ())
		    symbols) (branches i)
	 in
	   check_type ();
	   check_parameters ();
	   check_producers ())

      
let rec subst_parameter subst = function
  | ParameterVar x ->
      (try 
	List.assoc x.value subst 
      with Not_found ->
	ParameterVar x)

  | ParameterApp (x, ps) -> 
      (try 
	match List.assoc x.value subst with
	  | ParameterVar y ->
	      ParameterApp (y, List.map (subst_parameter subst) ps)

	  | ParameterApp _ ->
	      (* Type-checking ensures that we cannot do partial
		 application. Consequently, if an higher-order non terminal
		 is an actual argument, it cannot be the result of a 
		 partial application. *)
	      assert false

      with Not_found -> 
	  ParameterApp (x, List.map (subst_parameter subst) ps))

let subst_parameters subst = 
  List.map (subst_parameter subst)

let names_of_p_grammar p_grammar = 
  StringMap.fold (fun tok _ acu -> StringSet.add tok acu) 
    p_grammar.p_tokens StringSet.empty 
    $$ (StringMap.fold (fun nt _ acu -> StringSet.add nt acu)
	  p_grammar.p_rules)

let expand p_grammar = 
  (* Check that it is safe to expand this parameterized grammar. *)
  check_grammar p_grammar;

  (* Set up a mechanism that ensures that names are unique -- and, in
     fact, ensures the stronger condition that normalized names are
     unique. *)

  let names =
    ref (StringSet.empty) 
  in
  let ensure_fresh name =
    let normalized_name = Misc.normalize name in
    if StringSet.mem normalized_name !names then
      Error.error []
	(Printf.sprintf "internal name clash over %s" normalized_name);
    names := StringSet.add normalized_name !names;
    name
  in 
  let expanded_rules = 
    Hashtbl.create 13 
  in
  let module InstanceTable = 
    Hashtbl.Make (Parameters)
  in
  let rule_names = 
    InstanceTable.create 13 
  in

  (* [mangle p] chooses a name for the new nonterminal symbol that corresponds
     to the parameter [p]. *)

  let rec mangle = function 
    | ParameterVar x
    | ParameterApp (x, []) ->
	Positions.value x
    | ParameterApp (x, ps) ->

	(* We include parentheses and commas in the names that we
	   assign to expanded nonterminals, because that is more
	   readable and acceptable in many situations. We replace them
	   with underscores in situations where these characters are
	   not valid. *)

	Printf.sprintf "%s(%s)"
	  (Positions.value x)
	  (separated_list_to_string mangle "," ps)

  in
  let name_of symbol parameters = 
    let param = ParameterApp (symbol, parameters) in
    try 
      InstanceTable.find rule_names param
    with Not_found ->
      let name = ensure_fresh (mangle param) in
      InstanceTable.add rule_names param name;
      name
  in
  (* Given the substitution [subst] from parameters to non terminal, we
     instantiate the parameterized branch. *)
  let rec expand_branch subst pbranch = 
    let new_producers = List.map 
      (function (ido, p) ->
	 let sym, actual_parameters = 
	   Parameters.unapp p in
	 let sym, actual_parameters = 	      
	   try 
	     match List.assoc sym.value subst with
	       | ParameterVar x ->
		   x, subst_parameters subst actual_parameters

	       | ParameterApp (x, ps) ->
		   assert (actual_parameters = []);
		   x, ps
		       
	   with Not_found -> 
	     sym, subst_parameters subst actual_parameters
	 in
	   (* Instantiate the definition of the producer. *)
	   (expand_branches subst sym actual_parameters, Option.map Positions.value ido))
      pbranch.pr_producers
    in
      {
        branch_position          = pbranch.pr_branch_position;
	producers		 = new_producers;
	action			 = pbranch.pr_action;
	branch_shift_precedence  = pbranch.pr_branch_shift_precedence;
	branch_reduce_precedence = pbranch.pr_branch_reduce_precedence;
      }

  (* Instantiate the branches of sym for a particular set of actual
     parameters. *)
  and expand_branches subst sym actual_parameters =
    let nsym = name_of sym actual_parameters in
      try
	if not (Hashtbl.mem expanded_rules nsym) then begin
	  let prule = StringMap.find (Positions.value sym) p_grammar.p_rules in
	  let subst = 
	    (* Type checking ensures that parameterized non terminal 
	       instantiations are well defined. *)
	    assert (List.length prule.pr_parameters 
		    = List.length actual_parameters);
	    List.combine prule.pr_parameters actual_parameters @ subst in
	    Hashtbl.add expanded_rules nsym 
	      { branches = []; positions = []; inline_flag = false };
	  let rules = List.map (expand_branch subst) prule.pr_branches in
	    Hashtbl.replace expanded_rules nsym
	      { 
		branches    = rules; 
		positions   = prule.pr_positions; 
		inline_flag = prule.pr_inline_flag;
	      }
	end;
	nsym
      (* If [sym] is a terminal, then it is not in [p_grammar.p_rules]. 
	 Expansion is not needed. *)
      with Not_found -> Positions.value sym 
  in
  let rec types_from_list = function
    | [] -> StringMap.empty
    | (nt, ty)::q ->
        let accu = types_from_list q in
        let mangled = mangle nt in
        if StringMap.mem mangled accu then
          Error.error [Positions.position (Parameters.with_pos nt)]
            (Printf.sprintf
               "There are multiple %%type definitions for nonterminal %s."
               mangled);
        StringMap.add mangled (Positions.value ty) accu
  in

  let start_symbols = StringMap.domain (p_grammar.p_start_symbols) in
  {
    preludes      = p_grammar.p_preludes;
    postludes	  = p_grammar.p_postludes;
    parameters    = p_grammar.p_parameters;
    start_symbols = start_symbols;
    types         = types_from_list p_grammar.p_types;
    tokens	  = p_grammar.p_tokens;
    rules	  = 
      let closed_rules = StringMap.fold 
	(fun k prule rules -> 
	   (* If [k] is a start symbol then it cannot be parameterized. *)
	   if prule.pr_parameters <> [] && StringSet.mem k start_symbols then
	     Error.error []
	       (Printf.sprintf "The start symbol `%s' cannot be parameterized."
		  k);

	   (* Entry points are the closed non terminals. *)
	   if prule.pr_parameters = [] then 
	     StringMap.add k { 
	       branches    = List.map (expand_branch []) prule.pr_branches;
	       positions   = prule.pr_positions;
	       inline_flag = prule.pr_inline_flag;
	     } rules
	   else rules)
	p_grammar.p_rules
	StringMap.empty
      in
	Hashtbl.fold StringMap.add expanded_rules closed_rules 
  }
