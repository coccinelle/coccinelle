(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

(* ---------------------------------------------------------------- *)
(* code to check for ambiguities *)

(* Idea: for each node that is said to be modified in any witness tree, we
  check that all backward paths end up at the root of some witness tree
  that says that the node should be modified.  We then give a warning, if
  the node itself appears more than once in such a path, because then there
  could be some instances that are modified and some that are not.  An
  example is as follows:

  f(); ... g(); ... - h();

  with C code: f(); while(E) { h(); g(); } g(); h();

  Then the h() in the while loop matches both the first ... and the - h();

  Concretely, if a node 47 is in the witness tree rooted at 1 and the
  witness tree rooted at 2, then we give an error if 47 is not in the set
  of nodes satisfying AF[1v2] and give a warning if 47 is in the set of
  nodes satisfying EXEF(47 & EXEF(1v2)). (Note that the root of a witness
  tree here is the node causing the pattern to match; there might not be
  any witnesses associated with this node.)

  Another try on the exists formula:
  !(1v2) & EXE[!(1v2) U 47]
  The first !(1v2) is to discard immediately cases where the beginning and
  end of the path are the same.  Afterwards, it would only seem necessary to
  serach up to the next occurrence of 47 (leaf), ensuring that there are not
  1s or 2s (starting points) along the way.  Then the second 47 would be in
  the path, but possible not transformed.
 *)

module CTL = Ast_ctl

(* Step 1: for each tree, make a mapping from the modified nodes to the root
of the tree *)

type nodei = Control_flow_c.G.key

let modified = (Hashtbl.create(25) : (nodei, nodei list ref) Hashtbl.t)

let build_modified (n,_,wits) =
  let rec loop = function
      CTL.Wit(st,[CTL.Subst(x,Wrapper_ctl.PredVal(CTL.Modif(v)))],anno,wit) ->
	let cell =
	  try Hashtbl.find modified st
	  with Not_found ->
	    let cell = ref [] in Hashtbl.add modified st cell; cell in
	cell := n :: !cell;
	List.iter loop wit
    |	CTL.Wit(st,_,anno,wit) -> List.iter loop wit
    |	CTL.NegWit(wit) -> () in
  List.iter loop wits

(* Step 2: For each node in the hash table, create the error and warning
   formulas *)

type 'a nodes = Node of 'a | After

let create_formulas _ =
  Hashtbl.fold
    (function node ->
      function roots ->
	function acc ->
	  (*let exef f =
	    wrap
	      (Ast_ctl.EX
		 (Ast_ctl.BACKWARD,wrap(Ast_ctl.EF(Ast_ctl.BACKWARD,f)))) in*)
	  let match_node = Ast_ctl.Pred(Node(node)) in
	  let match_roots =
	    List.map (function n -> Ast_ctl.Pred(Node(n)))
	      (List.sort compare !roots) in
	  let or_roots =
	    List.fold_left
	      (function prev -> function cur -> Ast_ctl.Or(prev,cur))
	      (List.hd match_roots) (List.tl match_roots) in
	  (* no point to search if no path, and the presence of after
	     in the AF formula can make things slow *)
	  if List.mem node !roots
	  then acc
	  else
	    (node,
	     Ast_ctl.AF(Ast_ctl.BACKWARD,Ast_ctl.NONSTRICT,
			Ast_ctl.Or(or_roots,Ast_ctl.Pred(After))),
	     Ast_ctl.And
	       (Ast_ctl.NONSTRICT,
		Ast_ctl.Not(or_roots),
		Ast_ctl.EX
		  (Ast_ctl.BACKWARD,
		   Ast_ctl.EU(Ast_ctl.BACKWARD,or_roots,match_node))))
	   (*exef
	      (wrap(Ast_ctl.And(Ast_ctl.NONSTRICT,match_node,exef(roots))))*)
	  :: acc)
    modified []

(* Step 3: check the formula on the control-flow graph *)

module PRED =
  struct
    type t = nodei nodes
    let print_predicate = function
	After -> Format.print_string "after"
      |	Node x -> Format.print_string (string_of_int x)
  end

module ENV =
  struct
    type value = unit
    type mvar = unit
    let eq_mvar x x'   = failwith "should not be invoked"
    let eq_val v v'    = failwith "should not be invoked"
    let merge_val v v' = failwith "should not be invoked"

    let print_mvar s   = failwith "should not be invoked"
    let print_value x  = failwith "should not be invoked"
  end


module CFG =
  struct
    type node = nodei
    type cfg = Control_flow_c.node Control_flow_c.G.ograph_mutable
    let predecessors cfg n =
      List.map fst (Control_flow_c.KeyEdgeSet.elements (cfg#predecessors n))
    let successors   cfg n =
      List.map fst (Control_flow_c.KeyEdgeSet.elements (cfg#successors n))
    let extract_is_loop cfg n =
      Control_flow_c.extract_is_loop (Control_flow_c.KeyMap.find n cfg#nodes)
    let print_node i = Format.print_string (string_of_int i)
    let size cfg = Control_flow_c.KeyMap.cardinal cfg#nodes
    let print_graph cfg label border_nodes fill_nodes filename = ()
  end

module ENGINE = Ctl_engine.CTL_ENGINE (ENV) (CFG) (PRED)

let test_formula state formula cfg =
    let label = function
	Node pred -> [(pred,[],[])]
      |	After ->
	  List.concat
	    (List.map
	       (fun (nodei, node) ->
		 match Control_flow_c.unwrap node with
		   Control_flow_c.AfterNode _ -> [(nodei,[],[])]
		 | _ -> [])
	       (Control_flow_c.KeyMap.bindings cfg#nodes)) in
    let preproc _ = true in
    let verbose = !Flag_ctl.verbose_ctl_engine in
    let pm = !Flag_ctl.partial_match in
(*     let gt = !Flag_ctl.graphical_trace in *)
    Flag_ctl.verbose_ctl_engine := false;
    Flag_ctl.partial_match := false;
    Flag_ctl.checking_reachability := true;
(*     Flag_ctl.graphical_trace := ""; *)
    let res =
      ENGINE.sat (cfg,label,preproc,List.map fst (Control_flow_c.KeyMap.bindings cfg#nodes))
	(CTL.And(CTL.NONSTRICT,CTL.Pred(Node(state)),formula))
	[[Node(state)]] in
    Flag_ctl.verbose_ctl_engine := verbose;
    Flag_ctl.partial_match := pm;
    Flag_ctl.checking_reachability := false;
(*     Flag_ctl.graphical_trace := gt; *)
    match res with [] -> false | _ -> true

(* ---------------------------------------------------------------- *)
(* Entry point *)

(* The argument is a list of triples with a node name, an empty environment
and a witness tree *)

type witness =
    (nodei, unit,
     (nodei, unit, unit) Ast_ctl.generic_ctl list)
      Ast_ctl.generic_witnesstree

type ('a,'b,'c,'d,'e) triples =
    (nodei * 'a *
     (nodei,
      ('b, ('c, 'd) Wrapper_ctl.wrapped_binding) CTL.generic_subst list, 'e)
     CTL.generic_witnesstree list) list

let check_reachability rulename triples cfg =
  Hashtbl.clear modified;
  List.iter build_modified triples;
  let formulas = create_formulas() in
  List.iter
    (function (node,af_formula,ef_formula) ->
      if test_formula node af_formula cfg
      then
	if test_formula node ef_formula cfg
	then
	  let n = Control_flow_c.KeyMap.find node cfg#nodes in
	  Printf.printf
	    "warning: %s, node %d: %s in %s may be inconsistently modified\n"
	    rulename node (snd n) !Flag.current_element
	else ()
      else
	let n = Control_flow_c.KeyMap.find node cfg#nodes in
	failwith
	  (Printf.sprintf
	     "%s: node %d: %s in %s reachable by inconsistent control-flow paths"
	     rulename node (snd n) !Flag.current_element))
    formulas
