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
  List.iter loop wits
    
(* Step 2: For each node in the hash table, create the error and warning
   formulas *)
    
let create_formulas _ =
  let wrap x = (x,()) in
  Hashtbl.fold
    (function node ->
      function roots ->
	function acc ->
	  let exef f =
	    wrap
	      (Ast_ctl.EX
		 (Ast_ctl.BACKWARD,wrap(Ast_ctl.EF(Ast_ctl.BACKWARD,f)))) in
	  let match_node = wrap (Ast_ctl.Pred(6)) in
	  let match_roots =
	    List.map (function n -> wrap (Ast_ctl.Pred(n)))
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

(* Step 3: check the formula on the control-flow graph *)

module PRED = 
  struct
    type t = Ograph_extended.nodei
    let print_predicate x = Printf.printf "%d\n" x
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
    type node = Ograph_extended.nodei
    type cfg = 
        (Control_flow_c.node, Control_flow_c.edge) 
          Ograph_extended.ograph_extended
    let predecessors cfg n = List.map fst ((cfg#predecessors n)#tolist)
    let successors   cfg n = List.map fst ((cfg#successors n)#tolist)
    let print_node i = Format.print_string (string_of_int i)
  end

module ENGINE = Ctl_engine.CTL_ENGINE (ENV) (CFG) (PRED)

let tested =
  (Hashtbl.create(25) :
     ((int,unit,unit) Ast_ctl.generic_ctl, Ograph_extended.nodei list)
     Hashtbl.t)

let test_formula state formula cfg =
  try
    let seen_before = Hashtbl.find tested formula in
    List.mem state seen_before
  with Not_found ->
    let label pred = [(pred,[],[])] in
    let res =
      ENGINE.sat (cfg,label,List.map fst cfg#nodes#tolist) formula ([state],[])
	(function _ -> function _ -> function _ -> function _ -> ()) in
    let res = List.map (function (st,_,_) -> st) res in
    Hashtbl.add tested formula res;
    List.mem state res

(* ---------------------------------------------------------------- *)
(* Entry point *)

(* The argument is a list of triples with a node name, an empty environment
and a witness tree *)

type witness =
    (Ograph_extended.nodei, unit,
     (Ograph_extended.nodei, unit, unit) Ast_ctl.generic_ctl list)
      Ast_ctl.generic_witnesstree

type ('a,'b,'c,'d,'e) triples =
    (Ograph_extended.nodei * 'a *
     (Ograph_extended.nodei,
      ('b, ('c, 'd) Wrapper_ctl.wrapped_binding) CTL.generic_subst list, 'e)
     CTL.generic_witnesstree) list

let check_reachability triples cfg =
  Hashtbl.clear modified;
  Hashtbl.clear tested;
  List.iter build_modified triples;
  let formulas = create_formulas() in
  List.iter
    (function (node,af_formula,ef_formula) ->
      if test_formula node af_formula cfg
      then
	if test_formula node ef_formula cfg
	then
	  Printf.printf "warning: node %d may be inconsistently modified\n"
	    node
	else ()
      else
	failwith
	  (Printf.sprintf
	     "node %d reachable by inconsistent control-flow paths" node))
    formulas
