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
  any witnesses associated with this node.) *)

module G = Ograph_extended
module CTL = Ast_ctl

(* Step 1: for each tree, make a mapping from the modified nodes to the root
of the tree *)

let modified = (Hashtbl.create(25) : (G.nodei, G.nodei list ref) Hashtbl.t)

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
    |	CTL.NegWit(st,_,anno,wit) -> List.iter loop wit in
  loop wits
    
  (* Step 2: For each node in the hash table, create the error and warning
     formulas *)
    
let create_formulas _ =
  let wrap x = (x,0) in
  Hashtbl.fold
    (function node ->
      function roots ->
	function acc ->
	  let exef f =
	    wrap
	      (Ast_ctl.EX
		 (Ast_ctl.BACKWARD,wrap(Ast_ctl.EF(Ast_ctl.BACKWARD,f)))) in
	  let match_node =
	    wrap (Ast_ctl.Pred(Lib_engine.Node node,Ast_ctl.Control)) in
	  let match_roots =
	    List.map
	      (function n ->
		wrap (Ast_ctl.Pred(Lib_engine.Node n,Ast_ctl.Control)))
	      (List.sort compare !roots) in
	  let roots =
	    List.fold_left
	      (function prev -> function cur -> wrap(Ast_ctl.Or(prev,cur)))
	      (List.hd match_roots) (List.tl match_roots) in
	  (node,
	   wrap (Ast_ctl.AF(Ast_ctl.BACKWARD,roots)),
	   exef(wrap(Ast_ctl.And(match_node,exef(roots)))))
	  :: acc)
    modified []
