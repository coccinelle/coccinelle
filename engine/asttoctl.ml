(* true = don't see all matched nodes, only modified ones *)
let onlyModif = ref true
(* set to true for line numbers in the output of ctl_engine *)
let line_numbers = ref false
(* if true, only eg if header is included in not for ...s *)
let simple_get_end = ref false(*true*)

(* Question: where do we put the existential quantifier for or.  At the
moment, let it float inwards. *)

module Ast = Ast_cocci
module V = Visitor_ast
module CTL = Ast_ctl
module FV = Free_vars

let warning s = Printf.fprintf stderr "warning: %s\n" s

type cocci_predicate = Lib_engine.predicate * string Ast_ctl.modif

let aftpred = (Lib_engine.After,CTL.Control)
let retpred = (Lib_engine.Return,CTL.Control)
let exitpred = (Lib_engine.ErrorExit,CTL.Control)

let intersect l1 l2 = List.filter (function x -> List.mem x l2) l1
let subset l1 l2 = List.for_all (function x -> List.mem x l2) l1

(* --------------------------------------------------------------------- *)

let wrap n ctl = (ctl,n)

let aftret =
  wrap 0 (CTL.Or(wrap 0 (CTL.Pred aftpred),wrap 0 (CTL.Pred exitpred)))

let wrapImplies n (x,y) = wrap n (CTL.Implies(x,y))
let wrapExists n (x,y) = wrap n (CTL.Exists(x,y))
let wrapAnd n (x,y) = wrap n (CTL.And(x,y))
let wrapOr n (x,y) = wrap n (CTL.Or(x,y))
let wrapAU n (x,y) = wrap n (CTL.AU(CTL.FORWARD,x,y))
let wrapEU n (x,y) = wrap n (CTL.EU(CTL.FORWARD,x,y))
let wrapAX n (x) = wrap n (CTL.AX(CTL.FORWARD,1,x))
let wrapAXc n count (x) = wrap n (CTL.AX(CTL.FORWARD,count,x))
let wrapEX n (x) = wrap n (CTL.EX(CTL.FORWARD,1,x))
let wrapAG n (x) = wrap n (CTL.AG(CTL.FORWARD,x))
let wrapEG n (x) = wrap n (CTL.EG(CTL.FORWARD,x))
let wrapAF n (x) = wrap n (CTL.AF(CTL.FORWARD,x))
let wrapEF n (x) = wrap n (CTL.EF(CTL.FORWARD,x))
let wrapNot n (x) = wrap n (CTL.Not(x))
let wrapPred n (x) = wrap n (CTL.Pred(x))
let wrapLet n (x,y,z) = wrap n (CTL.Let(x,y,z))
let wrapRef n (x) = wrap n (CTL.Ref(x))

(* --------------------------------------------------------------------- *)

let get_option fn = function
    None -> None
  | Some x -> Some (fn x)

let get_list_option fn = function
    None -> []
  | Some x -> fn x

(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
(* Eliminate OptStm *)

(* for optional thing with nothing after, should check that the optional thing
never occurs.  otherwise the matchig stops before it occurs *)
let elim_opt =
  let mcode x = x in
  let donothing r k e = k e in

  let rec dots_list unwrapped wrapped =
    match (unwrapped,wrapped) with
      ([],_) -> []

    | (Ast.OptStm(stm)::(Ast.Dots(d,whencode,t) as u)::urest,_::d1::rest) ->
	let rw = Ast.rewrap stm in
	let rwd = Ast.rewrap stm in
	let rwu = Ast.rewrap d1 in
	let not_dots = Ast.Dots(d,whencode,stm::t) in
	[rw(Ast.Disj[rwd(Ast.DOTS(stm::(dots_list (u::urest) (d1::rest))));
		      rwd(Ast.DOTS(dots_list (not_dots::urest)
				     ((rwu not_dots)::rest)))])]

    | (Ast.OptStm(stm)::urest,_::rest) ->
	let rw = Ast.rewrap stm in
	let rwd = Ast.rewrap stm in
	let new_rest = dots_list urest rest in
	[rw(Ast.Disj[rwd(Ast.DOTS(stm::new_rest));rwd(Ast.DOTS(new_rest))])]

    | ([Ast.Dots(d,whencode,t);Ast.OptStm(stm)],[d1;_]) ->
	let rw = Ast.rewrap stm in
	let rwd = Ast.rewrap stm in
	[d1;rw(Ast.Disj[rwd(Ast.DOTS([stm]));rwd(Ast.DOTS([d1]))])]

    | ([Ast.Nest(sd);Ast.OptStm(stm)],[d1;_]) ->
	let rw = Ast.rewrap stm in
	let rwd = Ast.rewrap stm in
	let dots =
	  Ast.Dots(("...",{ Ast.line = 0; Ast.column = 0 },
		    Ast.CONTEXT(Ast.NOTHING)),
		   [],[]) in
	[d1;rw(Ast.Disj[rwd(Ast.DOTS([stm]));rwd(Ast.DOTS([rw dots]))])]

    | (_::urest,stm::rest) -> stm :: (dots_list urest rest)
    | _ -> failwith "not possible" in

  let stmtdotsfn r k d =
    let d = k d in
    Ast.rewrap d
      (match Ast.unwrap d with
	Ast.DOTS(l) -> Ast.DOTS(dots_list (List.map Ast.unwrap l) l)
      | Ast.CIRCLES(l) -> failwith "elimopt: not supported"
      | Ast.STARS(l) -> failwith "elimopt: not supported") in
  
  V.rebuilder
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    donothing donothing stmtdotsfn
    donothing donothing donothing donothing donothing donothing donothing
    donothing donothing donothing

(* --------------------------------------------------------------------- *)
(* Count depth of braces.  The translation of a closed brace appears deeply
nested within the translation of the sequence term, so the name of the
paren var has to take into account the names of the nested braces.  On the
other hand the close brace does not escape, so we don't have to take into
account other paren variable names. *)

let brace_table =
  (Hashtbl.create(50) :
     (Ast.statement (* always a seq/fundecl *),string (*paren var*)) Hashtbl.t)

let count_nested_braces =
  let bind x y = max x y in
  let option_default = 0 in
  let stmt_count r k s =
    match Ast.unwrap s with
      Ast.Seq(_,_,_) | Ast.FunDecl(_,_,_,_) ->
	let nested = k s in
	Hashtbl.add brace_table s ("p"^(string_of_int nested));
	nested + 1
    | _ -> k s in
  let donothing r k e = k e in
  let mcode r x = 0 in
  let recursor = V.combiner bind option_default
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      donothing donothing donothing
      donothing donothing donothing donothing donothing donothing
      donothing stmt_count donothing donothing in
  recursor.V.combiner_statement

(* --------------------------------------------------------------------- *)
(* Whenify *)
(* For A ... B, neither A nor B should occur in the code matched by the ...
We add these to any when code associated with the dots *)

let rec get_end ((_,extender,_) as fvinfo) f s =
  match Ast.unwrap s with
    Ast.Disj(stmt_dots_list) ->
      List.concat
	(List.map
	   (function x ->
	     match Ast.unwrap x with
	       Ast.DOTS(l) ->
		 (match l with [] -> [] | xs -> get_end fvinfo f (f xs))
	     | _ -> failwith "circles and stars not supported")
	   stmt_dots_list)
  | Ast.IfThen(header,_) (* take header only, for both first and last *)
  | Ast.IfThenElse(header,_,_,_)
  | Ast.While(header,_) -> 
      if !simple_get_end
      then
	let res = Ast.rewrap s (Ast.Atomic header) in
	let _ = extender res in
	[res]
      else [s]
  | Ast.Dots(_,_,_) -> []
  | Ast.OptStm(stm) -> [stm]
  | Ast.UniqueStm(stm) -> [stm]
  | Ast.MultiStm(stm) -> [stm]
  | _ -> [s]

and get_first fvinfo s =
  get_end fvinfo (function xs -> List.hd xs) s

and get_last fvinfo s =
  get_end fvinfo (function xs -> List.hd (List.rev xs)) s


(* --------------------------------------------------------------------- *)
(* Top-level code *)

let ctr = ref 0
let fresh_var _ =
  let c = !ctr in
  (*ctr := !ctr + 1;*)
  Printf.sprintf "v%d" c

let labctr = ref 0
let fresh_label_var s =
  let c = !labctr in
  labctr := !labctr + 1;
  Printf.sprintf "%s%d" s c

let lctr = ref 0
let fresh_let_var _ =
  let c = !lctr in
  lctr := !lctr + 1;
  Printf.sprintf "l%d" c

let sctr = ref 0
let fresh_metavar _ =
  let c = !sctr in
(*sctr := !sctr + 1;*)
  Printf.sprintf "_S%d" c

let get_unquantified quantified vars =
  List.filter (function x -> not (List.mem x quantified)) vars

let make_seq n first = function
    None -> first
  | Some rest -> wrapAnd n (first,wrapAX n rest)

let make_seq2 n first = function
    None -> first
  | Some rest -> wrapAnd n (first,wrapAXc n 2 rest)

let and_opt n first = function
    None -> first
  | Some rest -> wrapAnd n (first,rest)

let make_cond n branch re = wrapImplies n (branch,wrapAX n re)

let contains_modif =
  let bind x y = x or y in
  let option_default = false in
  let mcode r (_,_,kind) =
    match kind with
      Ast.MINUS(_) -> true
    | Ast.PLUS -> failwith "not possible"
    | Ast.CONTEXT(info) -> not (info = Ast.NOTHING) in
  let do_nothing r k e = k e in
  let recursor =
    V.combiner bind option_default
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      do_nothing do_nothing do_nothing
      do_nothing do_nothing do_nothing do_nothing do_nothing do_nothing
      do_nothing do_nothing do_nothing do_nothing in
  recursor.V.combiner_rule_elem

let make_match n unchecked free_table used_after code =
  if unchecked
  then wrapPred n (Lib_engine.Match(code),CTL.Control)
  else
    let v = fresh_var() in
    if contains_modif code
    then wrapExists n (v,wrapPred n (Lib_engine.Match(code),CTL.Modif v))
    else
      let any_used_after =
	let fvs = Hashtbl.find free_table (FV.Rule_elem code) in
	List.exists (function x -> List.mem x used_after) fvs in
      if !onlyModif && not any_used_after
      then wrapPred n (Lib_engine.Match(code),CTL.Control)
      else wrapExists n (v,wrapPred n (Lib_engine.Match(code),CTL.UnModif v))

let make_raw_match n code = wrapPred n (Lib_engine.Match(code),CTL.Control)

let seq_fvs free_table quantified term1 term2 =
  let t1fvs = get_unquantified quantified (Hashtbl.find free_table term1) in
  let t2fvs = get_unquantified quantified (Hashtbl.find free_table term2) in
  let bothfvs = Common.inter_set t1fvs t2fvs in
  let t1onlyfvs = Common.minus_set t1fvs bothfvs in
  let t2onlyfvs = Common.minus_set t2fvs bothfvs in
  (t1onlyfvs,bothfvs,t2onlyfvs)

let quantify n =
  List.fold_right (function cur -> function code -> wrapExists n (cur,code))

let intersectll lst nested_list =
  List.filter (function x -> List.exists (List.mem x) nested_list) lst

(* notbefore and notafter are the neighbors that should not be found in a ...*)
(* unchecked of true indicates that the neighbors are not taken into account *)
let rec dots_stmt ((free_table,_,_) as fvinfo) quantified l unchecked
    notbefore notafter after =
  let n = Ast.get_line l in
  let quantify = quantify n in
  match Ast.unwrap l with
    Ast.DOTS(x) ->
      let fvs =
	List.map (function x -> Hashtbl.find free_table (FV.Statement x)) x in
      let rec loop quantified notbefore notafter = function
	  ([],[]) -> (match after with Some x -> x | None -> wrap n (CTL.True))
	| ([x],[_]) ->
	    statement fvinfo quantified x unchecked notbefore notafter after
	| (x::((aft::_) as xs),fv::fvs) ->
	    let shared = intersectll fv fvs in
	    let unqshared = get_unquantified quantified shared in
	    let new_quantified = Common.union_set unqshared quantified in
	    let make_matches l =
(*	      if unchecked (* don't bother collecting info that won't be used*)
	      then []
	      else*)
		List.map
		  (function s ->
		    statement fvinfo new_quantified s true [] [] None)
		  l in
	    quantify unqshared
	      (statement fvinfo new_quantified x unchecked notbefore
		 (make_matches (get_first fvinfo aft))
		 (Some
		    (loop new_quantified (make_matches (get_last fvinfo x))
		       notafter (xs,fvs))))
	| _ -> failwith "not possible" in
      loop quantified notbefore notafter (x,fvs)
  | Ast.CIRCLES(x) -> failwith "not supported"
  | Ast.STARS(x) -> failwith "not supported"

and statement ((free_table,extender,used_after) as fvinfo) quantified stmt
    unchecked notbefore notafter after =

  let n = if !line_numbers then Ast.get_line stmt else 0 in
  let wrapExists = wrapExists n in
  let wrapAnd = wrapAnd n in
  let wrapOr = wrapOr n in
  let wrapAU = wrapAU n in
  let wrapAX = wrapAX n in
  let wrapEX = wrapEX n in
  let wrapAG = wrapAG n in
  let wrapAF = wrapAF n in
  let wrapNot = wrapNot n in
  let wrapPred = wrapPred n in
  let make_seq = make_seq n in
  let make_seq2 = make_seq2 n in
  let and_opt = and_opt n in
  let make_cond = make_cond n in
  let quantify = quantify n in
  let make_match = make_match n unchecked free_table used_after in
  let make_raw_match = make_raw_match n in

  let make_meta_rule_elem d =
    let re = Ast.make_meta_rule_elem (fresh_metavar()) d in
    let _ = extender (Ast.rewrap stmt (Ast.Atomic(re))) in
    re in

  match Ast.unwrap stmt with
    Ast.Atomic(ast) ->
      (match Ast.unwrap ast with
	Ast.MetaStmt((s,i,(Ast.CONTEXT(Ast.BEFOREAFTER(_,_)) as d)))
      |	Ast.MetaStmt((s,i,(Ast.CONTEXT(Ast.AFTER(_)) as d))) ->
	  let label_var = (*fresh_label_var*) "_lab" in
	  let label_pred = wrapPred(Lib_engine.Label(label_var),CTL.Control) in
	  let prelabel_pred =
	    wrapPred(Lib_engine.PrefixLabel(label_var),CTL.Control) in
	  let matcher d = make_match (make_meta_rule_elem d) in
	  let full_metamatch = matcher d in
	  let first_metamatch =
	    matcher
	      (match d with
		Ast.CONTEXT(Ast.BEFOREAFTER(bef,_)) ->
		  Ast.CONTEXT(Ast.BEFORE(bef))
	      |	Ast.CONTEXT(_) -> Ast.CONTEXT(Ast.NOTHING)
	      | Ast.MINUS(_) | Ast.PLUS -> failwith "not possible") in
	  let middle_metamatch =
	    matcher
	      (match d with
		Ast.CONTEXT(_) -> Ast.CONTEXT(Ast.NOTHING)
	      | Ast.MINUS(_) | Ast.PLUS -> failwith "not possible") in
	  let last_metamatch =
	    matcher
	      (match d with
		Ast.CONTEXT(Ast.BEFOREAFTER(_,aft)) ->
		  Ast.CONTEXT(Ast.AFTER(aft))
	      |	Ast.CONTEXT(_) -> d
	      | Ast.MINUS(_) | Ast.PLUS -> failwith "not possible") in

	  let left_or =
	    make_seq full_metamatch
	      (Some(and_opt (wrapNot(prelabel_pred)) after)) in
	  let right_or =
	    make_seq first_metamatch
	      (Some(wrapAU(wrapAnd(middle_metamatch,prelabel_pred),
			   make_seq (wrapAnd(last_metamatch,prelabel_pred))
			     (Some(and_opt (wrapNot(prelabel_pred))
				     after))))) in
	  quantify (label_var::get_unquantified quantified [s])
	    (wrapAnd(make_raw_match ast,
		     wrapAnd(label_pred,wrapOr(left_or,right_or))))

      |	Ast.MetaStmt((s,i,d)) ->
	  let label_var = (*fresh_label_var*) "_lab" in
	  let label_pred = wrapPred(Lib_engine.Label(label_var),CTL.Control) in
	  let prelabel_pred =
	    wrapPred(Lib_engine.PrefixLabel(label_var),CTL.Control) in
	  let matcher d = make_match (make_meta_rule_elem d) in
	  let first_metamatch = matcher d in
	  let rest_metamatch =
	    matcher
	      (match d with
		Ast.MINUS(_) -> Ast.MINUS([])
	      | Ast.CONTEXT(_) -> Ast.CONTEXT(Ast.NOTHING)
	      | Ast.PLUS -> failwith "not possible") in
	  (* first_nodea and first_nodeb are separated here and above to
	     improve let sharing - only first_nodea is unique to this site *)
	  let first_nodeb = wrapAnd(first_metamatch,label_pred) in
	  let rest_nodes = wrapAnd(rest_metamatch,prelabel_pred) in
	  let last_node = and_opt (wrapNot(prelabel_pred)) after in
	  quantify (label_var::get_unquantified quantified [s])
	    (wrapAnd(make_raw_match ast,
		     (make_seq first_nodeb
			(Some (wrapAU(rest_nodes,last_node))))))
      |	_ ->
	  let stmt_fvs = Hashtbl.find free_table (FV.Statement stmt) in
	  let fvs = get_unquantified quantified stmt_fvs in
	  make_seq (quantify fvs (make_match ast)) after)
  | Ast.Seq(lbrace,body,rbrace) ->
      let v = Hashtbl.find brace_table stmt in
      let paren_pred = wrapPred(Lib_engine.Paren v,CTL.Control) in
      let start_brace = wrapAnd(make_match lbrace,paren_pred) in
      let end_brace = wrapAnd(make_match rbrace,paren_pred) in
      let not_start_brace = wrapAnd(make_raw_match lbrace,paren_pred) in
      let not_end_brace = wrapAnd(make_raw_match rbrace,paren_pred) in
      wrapExists
	(v,make_seq start_brace
	   (Some(dots_stmt fvinfo quantified body unchecked
		   [not_start_brace] [not_end_brace]
		   (Some (make_seq end_brace after)))))
  | Ast.IfThen(ifheader,branch) ->

(* "if (test) thn" becomes:
    if(test) & AX((TrueBranch & AX thn) v FallThrough v After)

    "if (test) thn; after" becomes:
    if(test) & AX((TrueBranch & AX thn) v FallThrough v (After & AXAX after))
             & EX After
*)

       (* free variables *) 
       let (efvs,bfvs,_) =
	 seq_fvs free_table quantified
	   (FV.Rule_elem ifheader) (FV.Statement branch) in
       let new_quantified = Common.union_set bfvs quantified in
       (* if header *)
       let if_header = quantify efvs (make_match ifheader) in
       (* then branch and after *)
       let true_branch =
	 make_seq
	   (wrapPred(Lib_engine.TrueBranch,CTL.Control))
	   (Some(statement fvinfo new_quantified branch unchecked [] []
		   None)) in
       let fall_branch =  wrapPred(Lib_engine.FallThrough,CTL.Control) in
       let after_pred = wrapPred(Lib_engine.After,CTL.Control) in
       let after_branch = make_seq2 after_pred after in
       let or_cases = wrapOr(true_branch,wrapOr(fall_branch,after_branch)) in
       (* the code *)
       (match after with
	 Some _ ->
	   quantify bfvs
	     (wrapAnd (if_header, wrapAnd(wrapAX or_cases, wrapEX after_pred)))
       | None -> quantify bfvs (wrapAnd(if_header, wrapAX or_cases)))
	 
  | Ast.IfThenElse(ifheader,branch1,els,branch2) ->

(*  "if (test) thn else els" becomes:
    if(test) & AX((TrueBranch & AX thn) v
                  (FalseBranch & AX (else & AX els)) v After)
             & EX FalseBranch

    "if (test) thn else els; after" becomes:
    if(test) & AX((TrueBranch & AX thn) v
                  (FalseBranch & AX (else & AX els)) v
                  (After & AXAX after))
             & EX FalseBranch
             & EX After


 Note that we rely on the well-formedness of C programs.  For example, we
 do not use EX to check that there is at least one then branch, because
 there is always one.  And we do not check that there is only one then or
 else branch, because these again are always the case in a well-formed C
 program. *)
       (* free variables *)
       let (e1fvs,b1fvs,_) =
	 seq_fvs free_table quantified
	   (FV.Rule_elem ifheader) (FV.Statement branch1) in
       let (e2fvs,b2fvs,_) =
	 seq_fvs free_table quantified
	   (FV.Rule_elem ifheader) (FV.Statement branch2) in
       let bothfvs = Common.union_set b1fvs b2fvs in
       let exponlyfvs = Common.inter_set e1fvs e2fvs in
       let new_quantified = Common.union_set bothfvs quantified in
       (* if header *)
       let if_header = quantify exponlyfvs (make_match ifheader) in
       (* then and else branches *)
       let true_branch =
	 make_seq
	   (wrapPred(Lib_engine.TrueBranch,CTL.Control))
	   (Some
	      (statement fvinfo new_quantified branch1 unchecked [] []
		 None)) in
       let false_pred = wrapPred(Lib_engine.FalseBranch,CTL.Control) in
       let false_branch =
	 make_seq false_pred
	   (Some (statement fvinfo new_quantified branch2 unchecked
		    [] [] None))
	   (*Some to uncomment when there are else nodes in the CFG
	      (make_seq
		 (make_match els)
		 (Some (statement fvinfo new_quantified branch2 unchecked
	                  [] [] None)))*) in
       let after_pred = wrapPred(Lib_engine.After,CTL.Control) in
       let after_branch = make_seq2 after_pred after in
       let or_cases = wrapOr(true_branch,wrapOr(false_branch,after_branch)) in
       (* the code *)
       (match after with
	None ->
	  quantify bothfvs
	    (wrapAnd (if_header, wrapAnd(wrapAX or_cases, wrapEX false_pred)))
      |	Some after ->
	  quantify bothfvs
	    (wrapAnd
	       (if_header,
		 wrapAnd(wrapAX or_cases,
			 wrapAnd(wrapEX false_pred,wrapEX after_pred)))))

  | Ast.While(header,body) ->
   (* the translation in this case is similar to that of an if with no else *)
      let (efvs,bfvs,_) =
	seq_fvs free_table quantified
	  (FV.Rule_elem header) (FV.Statement body) in
      let new_quantified = Common.union_set bfvs quantified in
      let while_header = quantify efvs (make_match header) in
      let true_branch = wrapPred(Lib_engine.TrueBranch,CTL.Control) in
      let body_line =
	make_cond true_branch
	  (statement fvinfo new_quantified body unchecked [] [] None) in
      (match after with
	None -> quantify bfvs (make_seq while_header (Some body_line))
      | Some after ->
	  let after_branch = wrapPred(Lib_engine.After,CTL.Control) in
	  let after_line = make_cond after_branch after in
	  quantify bfvs
	    (wrapAnd
	       (while_header,
		(wrapAnd
		   (wrapAX(wrapAnd(body_line,after_line)),
		    wrapEX(after_branch))))))
  | Ast.Disj(stmt_dots_list) ->
      let add_nots l e =
	List.fold_left
	  (function rest ->
	    function cur ->
	      wrapAnd
		(wrapNot(dots_stmt fvinfo quantified cur true [] [] None),
		 rest))
	  e l in
      let rec loop after = function
	  [] -> failwith "disj shouldn't be empty" (*wrap n CTL.False*)
	| [(nots,cur)] ->
	    add_nots nots
	      (dots_stmt fvinfo quantified cur unchecked notbefore notafter
		 after)
	| (nots,cur)::rest ->
	    wrapOr(add_nots nots
		     (dots_stmt fvinfo quantified cur unchecked
			notbefore notafter after),
		   loop after rest) in
      loop after (preprocess_disj stmt_dots_list)
  | Ast.Nest(stmt_dots) ->
      let dots_pattern =
	dots_stmt fvinfo quantified stmt_dots unchecked [] [] None in
      let udots_pattern =
	dots_stmt fvinfo quantified stmt_dots true [] [] None in
      (match after with
	None -> wrapAG(wrapOr(dots_pattern,wrapNot udots_pattern))
      |	Some after ->
(*	  if unchecked
	  then
	    wrapAU(wrapOr(dots_pattern,wrapNot udots_pattern),
		   wrapOr(after,aftret))
	  else*)
	    let left = wrapOr(dots_pattern,wrapNot udots_pattern) in
	    let left =
	      match notbefore@notafter with
		[] -> left
	      |	x::xs ->
		  wrapAnd
		    (wrapNot
		       (List.fold_left
			  (function rest -> function cur -> wrapOr(cur,rest))
			  x xs),
		     left) in
	    wrapAU(left,wrapOr(after,aftret)))
  | Ast.Dots((_,i,d),whencodes,tmp_whencode) ->
      let dot_code =
	match d with
	  Ast.MINUS(_) ->
            (* no need for the fresh metavar, but ... is a bit wierd as a
	       variable name *)
	    Some(make_match (make_meta_rule_elem d))
	| _ -> None in
      let tmp_whencode =
(*	if unchecked
	(* not sure that this is safe, should be more general than pattern *)
	then []
	else*)
	  (List.map
	     (function s ->
	       statement fvinfo quantified s unchecked [] [] None)
	     tmp_whencode) @ notbefore @ notafter in
      let phi1 =
	match tmp_whencode with (* start with tmp_whencode *)
	  [] -> None
	| x::xs ->
	    Some
	      (List.fold_left
		 (function rest -> function cur -> wrapOr(cur,rest))
		 x xs) in
      let phi2 =
	let whencodes =
	  match whencodes with
	    [] -> None
	  | _ ->
	      let processed =
		List.map
		  (function w ->
		    wrapNot
		      (dots_stmt fvinfo quantified w unchecked [] [] None))
		  whencodes in
	      Some (List.fold_left (function x -> function y -> wrapAnd(x,y))
		      (List.hd processed) (List.tl processed)) in
	match (whencodes,phi1) with (* add whencode *)
	  (None,None) -> None
	| (Some whencodes,None) -> Some whencodes
	| (None,Some phi) -> Some (wrapNot(phi))
	| (Some whencodes,Some phi) ->
	    Some (wrapAnd(whencodes,wrapNot(phi))) in
      let phi3 =
	match (dot_code,phi2) with (* add - on dots, if any *)
	  (None,None) -> None
	| (Some dotcode,None) -> Some dotcode
	| (None,Some whencode) -> Some whencode
	| (Some dotcode,Some whencode) ->
	    Some(wrapAnd (dotcode,whencode)) in
      let exit = wrap n (CTL.Pred (Lib_engine.Exit,CTL.Control)) in
      (match (after,phi3) with (* add in the after code to make the result *)
	  (None,None) -> exit
	| (Some after,None) -> wrapAF(wrapOr(after,aftret))
	| (None,Some whencode) -> wrapAU(whencode,wrapOr(aftret,exit))
	| (Some after,Some whencode) -> wrapAU(whencode,wrapOr(after,aftret)))
  | Ast.FunDecl(header,lbrace,body,rbrace) ->
      let (hfvs,bfvs,_) =
	seq_fvs free_table quantified
	  (FV.Rule_elem header) (FV.StatementDots body) in
      let function_header = quantify hfvs (make_match header) in
      let new_quantified = Common.union_set bfvs quantified in
      let v = Hashtbl.find brace_table stmt in
      let paren_pred = wrapPred(Lib_engine.Paren v,CTL.Control) in
      let start_brace = wrapAnd(make_match lbrace,paren_pred) in
      let end_brace = wrapAnd(make_match rbrace,paren_pred) in
      let not_start_brace = wrapAnd(make_raw_match lbrace,paren_pred) in
      let not_end_brace = wrapAnd(make_raw_match rbrace,paren_pred) in
      quantify bfvs
	(make_seq function_header
	   (Some
	      (wrapExists
		 (v,(make_seq start_brace
		       (Some(dots_stmt fvinfo new_quantified body
			       unchecked [not_start_brace] [not_end_brace]
			       (Some(make_seq end_brace after)))))))))
  | Ast.OptStm(stm) ->
      failwith "OptStm should have been compiled away\n";
  | Ast.UniqueStm(stm) ->
      failwith "arities not yet supported"
  | Ast.MultiStm(stm) ->
      failwith "arities not yet supported"
  | _ -> failwith "not supported"

(* Returns a triple for each disj element.  The first element of the triple is
Some v if the triple element needs a name, and None otherwise.  The second
element is a list of names whose negations should be conjuncted with the
term.  The third element is the original term *)
and (preprocess_disj :
       Ast.statement Ast.dots list ->
	 (Ast.statement Ast.dots list * Ast.statement Ast.dots) list) =
  function
    [] -> []
  | [s] -> [([],s)]
  | cur::rest ->
      let template =
	List.map (function r -> Unify_ast.unify_statement_dots cur r) rest in
      let processed = preprocess_disj rest in
      if List.exists (function Unify_ast.MAYBE -> true | _ -> false) template
      then
	([], cur) ::
	(List.map2
	   (function ((nots,r) as x) ->
	     function
		 Unify_ast.MAYBE -> (cur::nots,r)
	       | Unify_ast.NO -> x)
	   processed template)
      else ([], cur) :: processed

(* --------------------------------------------------------------------- *)
(* Letify:
Phase 1: Use a hash table to identify formulas that appear more than once.
Phase 2: Replace terms by variables.
Phase 3: Drop lets to the point as close as possible to the uses of their
variables *)

let formula_table =
  (Hashtbl.create(50) :
     ((cocci_predicate,string,Wrapper_ctl.info) CTL.generic_ctl,
      int ref (* count *) * string ref (* name *) * bool ref (* processed *))
     Hashtbl.t)

let add_hash phi =
  let (cell,_,_) =
    try Hashtbl.find formula_table phi
    with Not_found ->
      let c = (ref 0,ref "",ref false) in
      Hashtbl.add formula_table phi c;
      c in
  cell := !cell + 1

let rec collect_duplicates f =
  add_hash f;
  match CTL.unwrap f with
    CTL.False -> ()
  | CTL.True -> ()
  | CTL.Pred(p) -> ()
  | CTL.Not(phi) -> collect_duplicates phi
  | CTL.Exists(v,phi) -> collect_duplicates phi
  | CTL.And(phi1,phi2) -> collect_duplicates phi1; collect_duplicates phi2
  | CTL.Or(phi1,phi2) -> collect_duplicates phi1; collect_duplicates phi2
  | CTL.Implies(phi1,phi2) -> collect_duplicates phi1; collect_duplicates phi2
  | CTL.AF(_,phi) -> collect_duplicates phi
  | CTL.AX(_,_,phi) -> collect_duplicates phi
  | CTL.AG(_,phi) -> collect_duplicates phi
  | CTL.AU(_,phi1,phi2) -> collect_duplicates phi1; collect_duplicates phi2
  | CTL.EF(_,phi) -> collect_duplicates phi
  | CTL.EX(_,_,phi) -> collect_duplicates phi
  | CTL.EG(_,phi) -> collect_duplicates phi
  | CTL.EU(_,phi1,phi2) -> collect_duplicates phi1; collect_duplicates phi2
  | _ -> failwith "not possible"

let assign_variables _ =
  Hashtbl.iter
    (function formula ->
      function (cell,str,_) -> if !cell > 1 then str := fresh_let_var())
    formula_table

let rec replace_formulas dec f =
  let (ct,name,treated) = Hashtbl.find formula_table f in
  let real_ct = !ct - dec in
  if real_ct > 1
  then
    if not !treated
    then
      begin
	treated := true;
	let (acc,new_f) = replace_subformulas (dec + (real_ct - 1)) f in
	((!name,new_f) :: acc, CTL.rewrap f (CTL.Ref !name))
      end
    else ([],CTL.rewrap f (CTL.Ref !name))
  else replace_subformulas dec f

and replace_subformulas dec f =
  match CTL.unwrap f with
    CTL.False -> ([],f)
  | CTL.True -> ([],f)
  | CTL.Pred(p) -> ([],f)
  | CTL.Not(phi) ->
      let (acc,new_phi) = replace_formulas dec phi in
      (acc,CTL.rewrap f (CTL.Not(new_phi)))
  | CTL.Exists(v,phi) -> 
      let (acc,new_phi) = replace_formulas dec phi in
      (acc,CTL.rewrap f (CTL.Exists(v,new_phi)))
  | CTL.And(phi1,phi2) ->
      let (acc1,new_phi1) = replace_formulas dec phi1 in
      let (acc2,new_phi2) = replace_formulas dec phi2 in
      (acc1@acc2,CTL.rewrap f (CTL.And(new_phi1,new_phi2)))
  | CTL.Or(phi1,phi2) ->
      let (acc1,new_phi1) = replace_formulas dec phi1 in
      let (acc2,new_phi2) = replace_formulas dec phi2 in
      (acc1@acc2,CTL.rewrap f (CTL.Or(new_phi1,new_phi2)))
  | CTL.Implies(phi1,phi2) -> 
      let (acc1,new_phi1) = replace_formulas dec phi1 in
      let (acc2,new_phi2) = replace_formulas dec phi2 in
      (acc1@acc2,CTL.rewrap f (CTL.Implies(new_phi1,new_phi2)))
  | CTL.AF(dir,phi) ->
      let (acc,new_phi) = replace_formulas dec phi in
      (acc,CTL.rewrap f (CTL.AF(dir,new_phi)))
  | CTL.AX(dir,count,phi) ->
      let (acc,new_phi) = replace_formulas dec phi in
      (acc,CTL.rewrap f (CTL.AX(dir,count,new_phi)))
  | CTL.AG(dir,phi) ->
      let (acc,new_phi) = replace_formulas dec phi in
      (acc,CTL.rewrap f (CTL.AG(dir,new_phi)))
  | CTL.AU(dir,phi1,phi2) ->
      let (acc1,new_phi1) = replace_formulas dec phi1 in
      let (acc2,new_phi2) = replace_formulas dec phi2 in
      (acc1@acc2,CTL.rewrap f (CTL.AU(dir,new_phi1,new_phi2)))
  | CTL.EF(dir,phi) ->
      let (acc,new_phi) = replace_formulas dec phi in
      (acc,CTL.rewrap f (CTL.EF(dir,new_phi)))
  | CTL.EX(dir,count,phi) ->
      let (acc,new_phi) = replace_formulas dec phi in
      (acc,CTL.rewrap f (CTL.EX(dir,count,new_phi)))
  | CTL.EG(dir,phi) -> 
      let (acc,new_phi) = replace_formulas dec phi in
      (acc,CTL.rewrap f (CTL.EG(dir,new_phi)))
  | CTL.EU(dir,phi1,phi2) ->
      let (acc1,new_phi1) = replace_formulas dec phi1 in
      let (acc2,new_phi2) = replace_formulas dec phi2 in
      (acc1@acc2,CTL.rewrap f (CTL.EU(dir,new_phi1,new_phi2)))
  | _ -> failwith "not possible"

let ctlfv_table =
  (Hashtbl.create(50) :
     ((cocci_predicate,string,Wrapper_ctl.info) CTL.generic_ctl,
      string list (* fvs *) *
	string list (* intersection of fvs of subterms *))
     Hashtbl.t)

let rec ctl_fvs f =
  try let (fvs,_) = Hashtbl.find ctlfv_table f in fvs
  with Not_found ->
    let ((fvs,_) as res) =
      match CTL.unwrap f with
	CTL.False | CTL.True | CTL.Pred(_) -> ([],[])
      | CTL.Not(phi) | CTL.Exists(_,phi)
      | CTL.AF(_,phi) | CTL.AX(_,_,phi) | CTL.AG(_,phi)
      | CTL.EF(_,phi) | CTL.EX(_,_,phi) | CTL.EG(_,phi) -> (ctl_fvs phi,[])
      | CTL.And(phi1,phi2) | CTL.Or(phi1,phi2) | CTL.Implies(phi1,phi2)
      | CTL.AU(_,phi1,phi2) | CTL.EU(_,phi1,phi2) ->
	  let phi1fvs = ctl_fvs phi1 in
	  let phi2fvs = ctl_fvs phi2 in
	  (Common.union_set phi1fvs phi2fvs,intersect phi1fvs phi2fvs)
      | CTL.Ref(v) -> ([v],[v])
      | CTL.Let(v,term,body) ->
	  let phi1fvs = ctl_fvs term in
	  let phi2fvs = Common.minus_set (ctl_fvs body) [v] in
	  (Common.union_set phi1fvs phi2fvs,intersect phi1fvs phi2fvs) in
    Hashtbl.add ctlfv_table f res;
    fvs

let rev_order_bindings b =
  let b =
    List.map
      (function (nm,term) ->
	let (fvs,_) = Hashtbl.find ctlfv_table term in (nm,fvs,term))
      b in
  let rec loop bound = function
      [] -> []
    | unbound ->
	let (now_bound,still_unbound) =
	  List.partition (function (_,fvs,_) -> subset fvs bound)
	    unbound in
	let get_names = List.map (function (x,_,_) -> x) in
	now_bound @ (loop ((get_names now_bound) @ bound) still_unbound) in
  List.rev(loop [] b)

let drop_bindings b f = (* innermost bindings first in b *)
  let process_binary f ffvs inter nm term fail =
    if List.mem nm inter
    then CTL.rewrap f (CTL.Let(nm,term,f))
    else CTL.rewrap f (fail()) in
  let find_fvs f =
    let _ = ctl_fvs f in Hashtbl.find ctlfv_table f in
  let rec drop_one nm term f =
    match CTL.unwrap f with
      CTL.False ->  f
    | CTL.True -> f
    | CTL.Pred(p) -> f
    | CTL.Not(phi) -> CTL.rewrap f (CTL.Not(drop_one nm term phi))
    | CTL.Exists(v,phi) -> CTL.rewrap f (CTL.Exists(v,drop_one nm term phi))
    | CTL.And(phi1,phi2) ->
	let (ffvs,inter) = find_fvs f in
	process_binary f ffvs inter nm term
	  (function _ -> CTL.And(drop_one nm term phi1,drop_one nm term phi2))
    | CTL.Or(phi1,phi2) ->
	let (ffvs,inter) = find_fvs f in
	process_binary f ffvs inter nm term
	  (function _ -> CTL.Or(drop_one nm term phi1,drop_one nm term phi2))
    | CTL.Implies(phi1,phi2) ->
	let (ffvs,inter) = find_fvs f in
	process_binary f ffvs inter nm term
	  (function _ ->
	    CTL.Implies(drop_one nm term phi1,drop_one nm term phi2))
    | CTL.AF(dir,phi) -> CTL.rewrap f (CTL.AF(dir,drop_one nm term phi))
    | CTL.AX(dir,count,phi) ->
	CTL.rewrap f (CTL.AX(dir,count,drop_one nm term phi))
    | CTL.AG(dir,phi) -> CTL.rewrap f (CTL.AG(dir,drop_one nm term phi))
    | CTL.AU(dir,phi1,phi2) ->
	let (ffvs,inter) = find_fvs f in
	process_binary f ffvs inter nm term
	  (function _ ->
	    CTL.AU(dir,drop_one nm term phi1,drop_one nm term phi2))
    | CTL.EF(dir,phi) -> CTL.rewrap f (CTL.EF(dir,drop_one nm term phi))
    | CTL.EX(dir,count,phi) ->
	CTL.rewrap f (CTL.EX(dir,count,drop_one nm term phi))
    | CTL.EG(dir,phi) -> CTL.rewrap f (CTL.EG(dir,drop_one nm term phi))
    | CTL.EU(dir,phi1,phi2) ->
	let (ffvs,inter) = find_fvs f in
	process_binary f ffvs inter nm term
	  (function _ ->
	    CTL.EU(dir,drop_one nm term phi1,drop_one nm term phi2))
    | (CTL.Ref(v) as x) -> process_binary f [v] [v] nm term (function _ -> x)
    | CTL.Let(v,term1,body) ->
	let (ffvs,inter) = find_fvs f in
	process_binary f ffvs inter nm term
	  (function _ ->
	    CTL.Let(v,drop_one nm term term1,drop_one nm term body)) in
  List.fold_left
    (function processed -> function (nm,_,term) -> drop_one nm term processed)
    f b

let pp = Format.print_string
let box f = Format.open_box 1; f(); Format.close_box ()

let char_and = "&"
let char_or  = "v" 
let char_not = "!" 


let letify f =
  Hashtbl.clear formula_table;
  Hashtbl.clear ctlfv_table;
  (* create a count of the number of occurrences of each subformula *)
  collect_duplicates f;
  (* give names to things that appear more than once *)
  assign_variables();
  (* replace duplicated formulas by their variables *)
  let (bindings,new_f) = replace_formulas 0 f in
  (* collect fvs of terms in bindings and new_f *)
  List.iter (function f -> let _ = ctl_fvs f in ())
    (new_f::(List.map (function (_,term) -> term) bindings));
  (* sort bindings with uses before defs *)
  let bindings = rev_order_bindings bindings in
  (* insert bindings as lets into the formula *)
  let res = drop_bindings bindings new_f in
  res

(* --------------------------------------------------------------------- *)
(* Function declaration *)

let top_level free_table extender used_after t =
  match Ast.unwrap t with
    Ast.DECL(decl) -> failwith "not supported decl"
  | Ast.INCLUDE(inc,s) -> failwith "not supported include"
  | Ast.FILEINFO(old_file,new_file) -> failwith "not supported fileinfo"
  | Ast.FUNCTION(stmt) ->
      let unopt = elim_opt.V.rebuilder_statement stmt in
      let _ = extender unopt in
      let _ = count_nested_braces unopt in
      letify
	(statement (free_table,extender,used_after) [] unopt false [] [] None)
  | Ast.CODE(stmt_dots) ->
      let unopt = elim_opt.V.rebuilder_statement_dots stmt_dots in
      List.iter
	(function x ->
	  let _ = extender x in
	  let _ = count_nested_braces x in ())
	(Ast.undots unopt);
      letify
	(dots_stmt (free_table,extender,used_after) [] unopt false [] [] None)
  | Ast.ERRORWORDS(exps) -> failwith "not supported errorwords"

(* --------------------------------------------------------------------- *)
(* Contains dots *)

let contains_dots =
  let bind x y = x or y in
  let option_default = false in
  let mcode r x = false in
  let statement r k s =
    match Ast.unwrap s with Ast.Dots(_,_,_) -> true | _ -> k s in
  let continue r k e = k e in
  let stop r k e = false in
  let res =
    V.combiner bind option_default
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      continue continue continue
      stop stop stop stop stop stop stop statement continue continue in
  res.V.combiner_top_level

(* --------------------------------------------------------------------- *)
(* Entry points *)

let asttoctl l free_table extender used_after =
  ctr := 0;
  lctr := 0;
  sctr := 0;
  let l =
    List.filter
      (function t ->
	match Ast.unwrap t with Ast.ERRORWORDS(exps) -> false | _ -> true)
      l in
  List.map (top_level free_table extender used_after) l

let pp_cocci_predicate (pred,modif) =
  Pretty_print_engine.pp_predicate pred

let cocci_predicate_to_string (pred,modif) =
  Pretty_print_engine.predicate_to_string pred
