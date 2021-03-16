(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

(* for MINUS and CONTEXT, pos is always None in this file *)
(*search for require*)
(* true = don't see all matched nodes, only modified ones *)
let onlyModif = ref true(*false*)

type ex = Exists | Forall
let exists = ref Forall

module Ast = Ast_cocci
module V = Visitor_ast
module CTL = Ast_ctl

let warning s = Printf.fprintf stderr "warning: %s\n" s

let this_rule = ref ""

type cocci_predicate = Lib_engine.predicate * Ast.meta_name Ast_ctl.modif
type formula = Lib_engine.ctlcocci
type top_formula = NONDECL of Lib_engine.ctlcocci | CODE of Lib_engine.ctlcocci

let union = Common.union_set
let intersect l1 l2 = List.filter (function x -> List.mem x l2) l1
let subset l1 l2 = List.for_all (function x -> List.mem x l2) l1

let foldl1 f xs = List.fold_left f (List.hd xs) (List.tl xs)
let foldr1 f xs =
  let xs = List.rev xs in List.fold_left f (List.hd xs) (List.tl xs)

let used_after = ref ([] : Ast.meta_name list)
let guard_to_strict guard = if guard then CTL.NONSTRICT else CTL.STRICT

let saved = ref ([] : Ast.meta_name list)

let string2var x = ("",x)

(* --------------------------------------------------------------------- *)
(* predicates matching various nodes in the graph *)

let ctl_and s x y    =
  match (x,y) with
    (CTL.False,_) | (_,CTL.False) -> CTL.False
  | (CTL.True,a) | (a,CTL.True) -> a
  | _ -> CTL.And(s,x,y)

let ctl_or x y     =
  match (x,y) with
    (CTL.True,_) | (_,CTL.True) -> CTL.True
  | (CTL.False,a) | (a,CTL.False) -> a
  | _ -> CTL.Or(x,y)

let ctl_or_fl x y     =
  match (x,y) with
    (CTL.True,_) | (_,CTL.True) -> CTL.True
  | (CTL.False,a) | (a,CTL.False) -> a
  | _ -> CTL.Or(y,x)

let ctl_seqor x y     =
  match (x,y) with
    (* drop x or true case because x might have side effects *)
    (CTL.True,_) (* | (_,CTL.True) *) -> CTL.True
  | (CTL.False,a) | (a,CTL.False) -> a
  | _ -> CTL.SeqOr(x,y)

let ctl_not = function
    CTL.True -> CTL.False
  | CTL.False -> CTL.True
  | x -> CTL.Not(x)

let ctl_ax s = function
    CTL.True -> CTL.True
  | CTL.False -> CTL.False
  | x ->
      match !exists with
        Exists -> CTL.EX(CTL.FORWARD,x)
      |	Forall -> CTL.AX(CTL.FORWARD,s,x)

let ctl_ax_absolute s = function
    CTL.True -> CTL.True
  | CTL.False -> CTL.False
  | x -> CTL.AX(CTL.FORWARD,s,x)

let ctl_ex = function
    CTL.True -> CTL.True
  | CTL.False -> CTL.False
  | x -> CTL.EX(CTL.FORWARD,x)

(* This stays being AX even for sgrep_mode, because it is used to identify
the structure of the term, not matching the pattern. *)
let ctl_back_ag = function
    CTL.True -> CTL.True
  | CTL.False -> CTL.False
  | x -> CTL.AG(CTL.BACKWARD,CTL.NONSTRICT,x)

let ctl_back_ax = function
    CTL.True -> CTL.True
  | CTL.False -> CTL.False
  | x -> CTL.AX(CTL.BACKWARD,CTL.NONSTRICT,x)

let ctl_back_ex = function
    CTL.True -> CTL.True
  | CTL.False -> CTL.False
  | x -> CTL.EX(CTL.BACKWARD,x)

let ctl_ef = function
    CTL.True -> CTL.True
  | CTL.False -> CTL.False
  | x -> CTL.EF(CTL.FORWARD,x)

let ctl_ag s = function
    CTL.True -> CTL.True
  | CTL.False -> CTL.False
  | x -> CTL.AG(CTL.FORWARD,s,x)

let ctl_au s x y =
  match (x,!exists) with
    (CTL.True,Exists) -> CTL.EF(CTL.FORWARD,y)
  | (CTL.True,Forall) -> CTL.AF(CTL.FORWARD,s,y)
  | (_,Exists) -> CTL.EU(CTL.FORWARD,x,y)
  | (_,Forall) -> CTL.AU(CTL.FORWARD,s,x,y)

let ctl_anti_au s x y = (* only for ..., where the quantifier is changed *)
  CTL.XX
    (match (x,!exists) with
      (CTL.True,Exists) -> CTL.AF(CTL.FORWARD,s,y)
    | (CTL.True,Forall) -> CTL.EF(CTL.FORWARD,y)
    | (_,Exists) -> CTL.AU(CTL.FORWARD,s,x,y)
    | (_,Forall) -> CTL.EU(CTL.FORWARD,x,y))

let ctl_uncheck = function
    CTL.True -> CTL.True
  | CTL.False -> CTL.False
  | x -> CTL.Uncheck x

let label_pred_maker = function
    None -> CTL.True
  | Some (label_var,used) ->
      used := true;
      CTL.Pred(Lib_engine.PrefixLabel(label_var),CTL.Control)

let bclabel_pred_maker = function
    None -> CTL.True
  | Some (label_var,used) ->
      used := true;
      CTL.Pred(Lib_engine.BCLabel(label_var),CTL.Control)

(* label used to be used here, but it is not used; label is only needed after
and within dots *)
let predmaker guard pred label = CTL.Pred pred

let aftpred     = predmaker false (Lib_engine.After,       CTL.Control)
let retpred     = predmaker false (Lib_engine.Return,      CTL.Control)
let funpred     = predmaker false (Lib_engine.FunHeader,   CTL.Control)
let unsbrpred   = predmaker false (Lib_engine.UnsafeBrace, CTL.Control)
let toppred     = predmaker false (Lib_engine.Top,         CTL.Control)
let exitpred    = predmaker false (Lib_engine.ErrorExit,   CTL.Control)
let endpred     = predmaker false (Lib_engine.Exit,        CTL.Control)
let gotopred    = predmaker false (Lib_engine.Goto,        CTL.Control)
let inlooppred  = predmaker false (Lib_engine.InLoop,      CTL.Control)
let truepred    = predmaker false (Lib_engine.TrueBranch,  CTL.Control)
let esctruepred = predmaker false (Lib_engine.EscTrueBranch,  CTL.Control)
let falsepred   = predmaker false (Lib_engine.FalseBranch, CTL.Control)
let fallpred    = predmaker false (Lib_engine.FallThrough, CTL.Control)
let loopfallpred = predmaker false (Lib_engine.LoopFallThrough, CTL.Control)
let gotoaftpred = predmaker false (Lib_engine.GotoAfter,   CTL.Control)

(*let aftret label_var =
  ctl_or (aftpred label_var)
    (ctl_or (loopfallpred label_var) (exitpred label_var))*)

let letctr = ref 0
let get_let_ctr _ =
  let cur = !letctr in
  letctr := cur + 1;
  Printf.sprintf "r%d" cur

(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
(* Eliminate OptStm *)

(* for optional thing with nothing after, should check that the optional thing
never occurs.  otherwise the matching stops before it occurs *)
let elim_opt =
  let mcode x = x in
  let donothing r k e = k e in

  let fvlist l =
    List.fold_left Common.union_set [] (List.map Ast.get_fvs l) in

  let mfvlist l =
    List.fold_left Common.union_set [] (List.map Ast.get_mfvs l) in

  let freshlist l =
    List.fold_left Common.union_set [] (List.map Ast.get_fresh l) in

  let inheritedlist l =
    List.fold_left Common.union_set [] (List.map Ast.get_inherited l) in

  let savedlist l =
    List.fold_left Common.union_set [] (List.map Ast.get_saved l) in

  let varlists l =
    (fvlist l, mfvlist l, freshlist l, inheritedlist l, savedlist l) in

  let rec dots_list unwrapped wrapped =
    match (unwrapped,wrapped) with
      ([],_) -> []

    | (Ast.Dots(_,_,_,_)::Ast.OptStm(stm)::(Ast.Dots(_,_,_,_) as u)::urest,
       d0::s::d1::rest)
    | (Ast.Nest(_,_,_,_,_,_,_)::Ast.OptStm(stm)::(Ast.Dots(_,_,_,_) as u)
       ::urest,
       d0::s::d1::rest) -> (* why no case for nest as u? *)
	 let l = Ast.get_line stm in
	 let new_rest1 = stm :: (dots_list (u::urest) (d1::rest)) in
	 let new_rest2 = dots_list urest rest in
	 let (fv_rest1,mfv_rest1,fresh_rest1,inherited_rest1,s1) =
	   varlists new_rest1 in
	 let (fv_rest2,mfv_rest2,fresh_rest2,inherited_rest2,s2) =
	   varlists new_rest2 in
	 [d0;
	   {(Ast.make_term
	       (Ast.Disj
		  [{(Ast.make_term new_rest1) with
		     Ast.node_line = l;
		     Ast.free_vars = fv_rest1;
		     Ast.minus_free_vars = mfv_rest1;
		     Ast.fresh_vars = fresh_rest1;
		     Ast.inherited = inherited_rest1;
		     Ast.saved_witness = s1};
		    {(Ast.make_term new_rest2) with
		      Ast.node_line = l;
		      Ast.free_vars = fv_rest2;
		      Ast.minus_free_vars = mfv_rest2;
		      Ast.fresh_vars = fresh_rest2;
		      Ast.inherited = inherited_rest2;
		      Ast.saved_witness = s2}])) with
	     Ast.node_line = l;
	     Ast.free_vars = fv_rest1;
	     Ast.minus_free_vars = mfv_rest1;
	     Ast.fresh_vars = fresh_rest1;
	     Ast.inherited = inherited_rest1;
	     Ast.saved_witness = s1}]

    | (Ast.OptStm(stm)::urest,_::rest) ->
	 let l = Ast.get_line stm in
	 let new_rest1 = dots_list urest rest in
	 let new_rest2 = stm::new_rest1 in
	 let (fv_rest1,mfv_rest1,fresh_rest1,inherited_rest1,s1) =
	   varlists new_rest1 in
	 let (fv_rest2,mfv_rest2,fresh_rest2,inherited_rest2,s2) =
	   varlists new_rest2 in
	 [{(Ast.make_term
	       (Ast.Disj
		  [{(Ast.make_term new_rest2) with
		      Ast.node_line = l;
		      Ast.free_vars = fv_rest2;
		      Ast.minus_free_vars = mfv_rest2;
		      Ast.fresh_vars = fresh_rest2;
		      Ast.inherited = inherited_rest2;
		      Ast.saved_witness = s2};
		    {(Ast.make_term new_rest1) with
		     Ast.node_line = l;
		     Ast.free_vars = fv_rest1;
		     Ast.minus_free_vars = mfv_rest1;
		     Ast.fresh_vars = fresh_rest1;
		     Ast.inherited = inherited_rest1;
		     Ast.saved_witness = s1}])) with
	     Ast.node_line = l;
	     Ast.free_vars = fv_rest2;
	     Ast.minus_free_vars = mfv_rest2;
	     Ast.fresh_vars = fresh_rest2;
	     Ast.inherited = inherited_rest2;
	     Ast.saved_witness = s2}]

    | ([Ast.Dots(_,_,_,_);Ast.OptStm(stm)],[d1;_]) ->
	let l = Ast.get_line stm in
	let fv_stm = Ast.get_fvs stm in
	let mfv_stm = Ast.get_mfvs stm in
	let fresh_stm = Ast.get_fresh stm in
	let inh_stm = Ast.get_inherited stm in
	let saved_stm = Ast.get_saved stm in
	let fv_d1 = Ast.get_fvs d1 in
	let mfv_d1 = Ast.get_mfvs d1 in
	let fresh_d1 = Ast.get_fresh d1 in
	let inh_d1 = Ast.get_inherited d1 in
	let saved_d1 = Ast.get_saved d1 in
	let fv_both = Common.union_set fv_stm fv_d1 in
	let mfv_both = Common.union_set mfv_stm mfv_d1 in
	let fresh_both = Common.union_set fresh_stm fresh_d1 in
	let inh_both = Common.union_set inh_stm inh_d1 in
	let saved_both = Common.union_set saved_stm saved_d1 in
	[d1;
	  {(Ast.make_term
	      (Ast.Disj
		 [{(Ast.make_term [stm]) with
		    Ast.node_line = l;
		    Ast.free_vars = fv_stm;
		    Ast.minus_free_vars = mfv_stm;
		    Ast.fresh_vars = fresh_stm;
		    Ast.inherited = inh_stm;
		    Ast.saved_witness = saved_stm};
		   {(Ast.make_term [d1]) with
		     Ast.node_line = l;
		     Ast.free_vars = fv_d1;
		     Ast.minus_free_vars = mfv_d1;
		     Ast.fresh_vars = fresh_d1;
		     Ast.inherited = inh_d1;
		     Ast.saved_witness = saved_d1}])) with
	     Ast.node_line = l;
	     Ast.free_vars = fv_both;
	     Ast.minus_free_vars = mfv_both;
	     Ast.fresh_vars = fresh_both;
	     Ast.inherited = inh_both;
	     Ast.saved_witness = saved_both}]

    | ([Ast.Nest(_,_,_,w,_,_,_);Ast.OptStm(stm)],[d1;_]) ->
	let l = Ast.get_line stm in
	let rw = Ast.rewrap stm in
	let rwd = Ast.rewrap stm in
	let dots = Ast.Dots(Ast.make_mcode "...",w,[],[]) in
	[d1;rw(Ast.Disj
		 [rwd [stm];
		   {(Ast.make_term [rw dots])
		   with Ast.node_line = l}])]

    | (_::urest,stm::rest) -> stm :: (dots_list urest rest)
    | _ -> failwith "asttoctl2: not possible 1" in

  let stmtdotsfn r k d =
    let d = k d in
    let l = Ast.unwrap d in
    Ast.rewrap d (dots_list (List.map Ast.unwrap l) l) in

  V.rebuilder
    mcode mcode mcode mcode mcode mcode mcode mcode mcode
    mcode mcode mcode mcode mcode
    donothing donothing stmtdotsfn donothing donothing donothing donothing
    donothing donothing donothing donothing donothing donothing
    donothing donothing donothing donothing donothing donothing donothing
    donothing donothing donothing donothing donothing donothing donothing
    donothing donothing donothing

(* --------------------------------------------------------------------- *)
(* after management *)
(* We need Guard for the following case:
<...
 a
 <...
  b
 ...>
...>
foo();

Here the inner <... b ...> should not go past foo.  But foo is not the
"after" of the body of the outer nest, because we don't want to search for
it in the case where the body of the outer nest ends in something other
than dots or a nest. *)

(* what is the difference between tail and end??? *)

type after = After of formula | Guard of formula | Tail | End | VeryEnd

type top = Top | NotTop

let a2n = function After x -> Guard x | a -> a

let print_ctl x =
  let pp_pred (x,_) = Pretty_print_engine.pp_predicate x in
  let pp_meta (_,x) = Common.pp x in
  Pretty_print_ctl.pp_ctl (pp_pred,pp_meta) false x;
  Format.print_newline()

let print_after = function
    After ctl -> Printf.printf "After:\n"; print_ctl ctl
  | Guard ctl -> Printf.printf "Guard:\n"; print_ctl ctl
  | Tail -> Printf.printf "Tail\n"
  | VeryEnd -> Printf.printf "Very End\n"
  | End -> Printf.printf "End\n"

(* --------------------------------------------------------------------- *)
(* Top-level code *)

let fresh_var _ = string2var "_v"
let fresh_pos _ = string2var "_pos" (* must be a constant *)

let fresh_metavar _ = "_S"

(* fvinfo is going to end up being from the whole associated statement.
   it would be better if it were just the free variables in d, but free_vars.ml
   doesn't keep track of free variables on + code *)
let make_meta_rule_elem n d cstr fvinfo =
  let nm = fresh_metavar() in
  Ast.make_meta_rule_elem (nm^n) d cstr fvinfo

let get_unquantified quantified vars =
  List.filter (function x -> not (List.mem x quantified)) vars

let make_seq guard l =
  let s = guard_to_strict guard in
  foldr1 (function rest -> function cur -> ctl_and s cur (ctl_ax s rest)) l

let make_seq_ex guard l =
  let s = guard_to_strict guard in
  foldr1 (function rest -> function cur -> ctl_and s cur (ctl_ex rest)) l

let make_seq_after2 guard first rest =
  let s = guard_to_strict guard in
  match rest with
    After rest -> ctl_and s first (ctl_ex (ctl_ax s rest))
  | _ -> first

let make_seq_after guard first rest =
  match rest with
    After rest -> make_seq guard [first;rest]
  | _ -> first

let make_seq_after_ex guard first rest =
  match rest with
    After rest -> make_seq_ex guard [first;rest]
  | _ -> first

let opt_and guard first rest =
  let s = guard_to_strict guard in
  match first with
    None -> rest
  | Some first -> ctl_and s first rest

let and_after guard first rest =
  let s = guard_to_strict guard in
  match rest with After rest -> ctl_and s first rest | _ -> first

let contains_modif =
  let bind x y = x || y in
  let option_default = false in
  let mcode r (_,_,kind,metapos) =
    match kind with
      Ast.MINUS(_,_,_,_) -> true
    | Ast.PLUS _ -> failwith "asttoctl2: not possible 2"
    | Ast.CONTEXT(_,info) -> not (info = Ast.NOTHING) in
  let do_nothing r k e = k e in
  let annotated_decl decl =
    match Ast.unwrap decl with
      Ast.DElem(bef,_,_) -> bef in
  let rule_elem r k re =
    let res = k re in
    match Ast.unwrap re with
      Ast.FunHeader(bef,_,fninfo,name,lp,params,va,rp) ->
	bind (mcode r ((),(),bef,[])) res
    | Ast.Decl decl -> bind (mcode r ((),(),annotated_decl decl,[])) res
    | Ast.ForHeader(fr,lp,Ast.ForDecl(decl),e2,sem2,e3,rp) ->
	bind (mcode r ((),(),annotated_decl decl,[])) res
    | _ -> res in
  let init r k i =
    let res = k i in
    match Ast.unwrap i with
      Ast.StrInitList(allminus,_,_,_,_) -> allminus || res
    | _ -> res in
  let recursor =
    V.combiner bind option_default
      mcode mcode mcode mcode mcode mcode mcode mcode mcode
      mcode mcode mcode mcode mcode
      do_nothing do_nothing do_nothing do_nothing do_nothing do_nothing
      do_nothing do_nothing do_nothing do_nothing do_nothing
      do_nothing do_nothing do_nothing do_nothing init do_nothing do_nothing
      do_nothing do_nothing do_nothing
      do_nothing do_nothing rule_elem do_nothing do_nothing do_nothing
      do_nothing do_nothing do_nothing in
  recursor.V.combiner_rule_elem

let contains_pos =
  let bind x y = x || y in
  let option_default = false in
  let mcode r (_,_,kind,metapos) = not (metapos = []) in
  let do_nothing r k e = k e in
  let annotated_decl decl =
    match Ast.unwrap decl with
      Ast.DElem(bef,_,_) -> bef in
  let rule_elem r k re =
    let res = k re in
    match Ast.unwrap re with
      Ast.FunHeader(bef,_,fninfo,name,lp,params,va,rp) ->
	bind (mcode r ((),(),bef,[])) res
    | Ast.Decl decl -> bind (mcode r ((),(),annotated_decl decl,[])) res
    | Ast.ForHeader(fr,lp,Ast.ForDecl(decl),e2,sem2,e3,rp) ->
	bind (mcode r ((),(),annotated_decl decl,[])) res
    | _ -> res in
  let recursor =
    V.combiner bind option_default
      mcode mcode mcode mcode mcode mcode mcode mcode mcode
      mcode mcode mcode mcode mcode
      do_nothing do_nothing do_nothing do_nothing do_nothing do_nothing
      do_nothing do_nothing do_nothing do_nothing do_nothing do_nothing
      do_nothing do_nothing do_nothing do_nothing do_nothing do_nothing
      do_nothing do_nothing do_nothing
      do_nothing do_nothing rule_elem do_nothing do_nothing do_nothing
      do_nothing do_nothing do_nothing in
  recursor.V.combiner_rule_elem

(* code is not a DisjRuleElem *)
let make_match label guard code =
  let v = fresh_var() in
  let matcher = Lib_engine.Match(code) in
  if contains_modif code && not guard
  then CTL.Exists(true,v,predmaker guard (matcher,CTL.Modif v) label)
  else
    let iso_info = !Flag.track_iso_usage && not (Ast.get_isos code = []) in
    (match (iso_info,!onlyModif,guard,
	    intersect !used_after (Ast.get_fvs code)) with
      (false,true,_,[]) | (_,_,true,_) ->
	predmaker guard (matcher,CTL.Control) label
    | _ -> CTL.Exists(true,v,predmaker guard (matcher,CTL.UnModif v) label))

let make_raw_match label guard code =
  match intersect !used_after (Ast.get_fvs code) with
    [] -> predmaker guard (Lib_engine.Match(code),CTL.Control) label
  | _ ->
      let v = fresh_var() in
    CTL.Exists(true,v,predmaker guard (Lib_engine.Match(code),CTL.UnModif v)
		 label)

let rec seq_fvs quantified = function
    [] -> []
  | fv1::fvs ->
      let t1fvs = get_unquantified quantified fv1 in
      let termfvs =
	List.fold_left Common.union_set []
	  (List.map (get_unquantified quantified) fvs) in
      let bothfvs = Common.inter_set t1fvs termfvs in
      let t1onlyfvs = Common.minus_set t1fvs bothfvs in
      let new_quantified = Common.union_set bothfvs quantified in
      (t1onlyfvs,bothfvs)::(seq_fvs new_quantified fvs)

let get_constraints fvs code =
  let fv_constraints =
    List.fold_left
      (fun prev cur ->
	try
	  !(Hashtbl.find Data.non_local_script_constraints (!this_rule,cur)) @
	  prev
	with Not_found -> prev)
      [] fvs in
  match fv_constraints with
    [] -> code
  | ((rl,nm),cstr)::_ ->
      failwith
	(Printf.sprintf "constraint on %s.%s floats out beyond a rule elem"
	   rl nm)
      (* We would like to have a constructor Constraints, as follows, that
      would be like Exists, but would drop out environments that don't
      satisfy the constraints.  Unfortunately, the abstractness of the CTL
      engine and all the wrapping makes it hard to figure out how to add a
      new function eval_constraint, analogous to label.  So for the moment,
      we allow only constraints that can be evaluated at rule elems. *)
      (*CTL.Constraints(fv_constraints,code)*)

let quantify guard fvs rest =
  List.fold_right
    (function cur ->
      function code ->
	CTL.Exists (not guard && List.mem cur !saved,cur,code))
    fvs (get_constraints fvs rest)

let non_saved_quantify fvs rest =
  List.fold_right
    (function cur -> function code -> CTL.Exists (false,cur,code))
    fvs (get_constraints fvs rest)

let intersectll lst nested_list =
  List.filter (function x -> List.exists (List.mem x) nested_list) lst

(* --------------------------------------------------------------------- *)
(* Count depth of braces.  The translation of a closed brace appears deeply
nested within the translation of the sequence term, so the name of the
paren var has to take into account the names of the nested braces.  On the
other hand the close brace does not escape, so we don't have to take into
account other paren variable names. *)

(* called repetitively, which is inefficient, but less trouble than adding a
new field to Seq and FunDecl *)
let count_nested_braces s =
  let bind x y = max x y in
  let option_default = 0 in
  let stmt_count r k s =
    match Ast.unwrap s with
      Ast.Seq(_,_,_) | Ast.FunDecl(_,_,_,_,_) -> (k s) + 1
    | _ -> k s in
  let donothing r k e = k e in
  let mcode r x = 0 in
  let recursor = V.combiner bind option_default
      mcode mcode mcode mcode mcode mcode mcode mcode mcode
      mcode mcode mcode mcode mcode
      donothing donothing donothing donothing donothing donothing donothing
      donothing donothing donothing donothing donothing donothing donothing
      donothing donothing donothing donothing donothing donothing donothing
      donothing donothing donothing stmt_count donothing donothing donothing
      donothing donothing in
  let res = string_of_int (recursor.V.combiner_statement s) in
  string2var ("p"^res)

let labelctr = ref 0
let get_label_ctr _ =
  let cur = !labelctr in
  labelctr := cur + 1;
  string2var (Printf.sprintf "l%d" cur)

(* --------------------------------------------------------------------- *)
(* annotate dots with before and after neighbors *)

let print_bef_aft = function
    Ast.WParen (re,n) ->
      Printf.printf "bef/aft\n";
      Pretty_print_cocci.rule_elem "" re;
      Format.print_newline()
  | Ast.Other s ->
      Printf.printf "bef/aft\n";
      Pretty_print_cocci.statement "" s;
      Format.print_newline()
  | Ast.Other_dots d ->
      Printf.printf "bef/aft\n";
      Pretty_print_cocci.statement_dots d;
      Format.print_newline()

(* [] can only occur if we are in a disj, where it comes from a ?  In that
case, we want to use a, which accumulates all of the previous patterns in
their entirety. *)
let rec get_before_elem sl a =
  let x = Ast.unwrap sl in
  let rec loop sl a =
    match sl with
      [] -> ([],Common.Right a)
    | [e] ->
	let (e,ea) = get_before_e e a in
	([e],Common.Left ea)
    | e::sl ->
	let (e,ea) = get_before_e e a in
	let (sl,sla) = loop sl ea in
	(e::sl,sla) in
  let (l,a) = loop x a in
  (Ast.rewrap sl l,a)

and get_before sl a =
  match get_before_elem sl a with
    (term,Common.Left x) -> (term,x)
  | (term,Common.Right x) -> (term,x)

and get_before_whencode wc =
  List.map
    (function
	Ast.WhenNot w -> let (w,_) = get_before w [] in Ast.WhenNot w
      | Ast.WhenAlways w -> let (w,_) = get_before_e w [] in Ast.WhenAlways w
      |	Ast.WhenModifier(x) -> Ast.WhenModifier(x)
      | Ast.WhenNotTrue w -> Ast.WhenNotTrue w
      | Ast.WhenNotFalse w -> Ast.WhenNotFalse w)
    wc

and get_before_e s a =
  match Ast.unwrap s with
    Ast.Dots(d,w,_,aft) ->
      (Ast.rewrap s (Ast.Dots(d,get_before_whencode w,a,aft)),a)
  | Ast.Nest(starter,stmt_dots,ender,w,multi,_,aft) ->
      let w = get_before_whencode w in
      let (sd,_) = get_before stmt_dots a in
      (*let a =
	got rid of this, don't want to let nests overshoot
	List.filter
	  (function
	      Ast.Other a ->
		let unifies =
		  Unify_ast.unify_statement_dots
		    (Ast.rewrap s [a]) stmt_dots in
		(match unifies with
		  Unify_ast.MAYBE -> false
		| _ -> true)
	    | Ast.Other_dots a ->
		let unifies = Unify_ast.unify_statement_dots a stmt_dots in
		(match unifies with
		  Unify_ast.MAYBE -> false
		| _ -> true)
	    | _ -> true)
	  a in*)
      (Ast.rewrap s (Ast.Nest(starter,sd,ender,w,multi,a,aft)),
       [Ast.Other_dots stmt_dots])
  | Ast.Disj(stmt_dots_list) ->
      let (dsl,dsla) =
	List.split (List.map (function e -> get_before e a) stmt_dots_list) in
      (Ast.rewrap s (Ast.Disj(dsl)),List.fold_left Common.union_set [] dsla)
  | Ast.Conj(stmt_dots_list) ->
      let (dsl,dsla) =
	List.split (List.map (function e -> get_before e a) stmt_dots_list) in
      (Ast.rewrap s (Ast.Conj(dsl)),List.fold_left Common.union_set [] dsla)
  | Ast.Atomic(ast) ->
      (match Ast.unwrap ast with
	Ast.MetaStmt(_,_,_,_,_) -> (s,[])
      |	_ -> (s,[Ast.Other s]))
  | Ast.Seq(lbrace,body,rbrace) ->
      let index = count_nested_braces s in
      let (bd,_) = get_before body [Ast.WParen(lbrace,index)] in
      (Ast.rewrap s (Ast.Seq(lbrace,bd,rbrace)),[Ast.WParen(rbrace,index)])
  | Ast.Define(header,body) ->
      let (body,_) = get_before body [] in
      (Ast.rewrap s (Ast.Define(header,body)), [Ast.Other s])
  | Ast.AsStmt(stmt,asstmt) ->
      let (stmt,_) = get_before_e stmt [] in
      let (asstmt,_) = get_before_e asstmt [] in
      (Ast.rewrap s (Ast.AsStmt(stmt,asstmt)),[Ast.Other s])
  | Ast.IfThen(ifheader,branch,aft) ->
      let (br,_) = get_before_e branch [] in
      (Ast.rewrap s (Ast.IfThen(ifheader,br,aft)), [Ast.Other s])
  | Ast.IfThenElse(ifheader,branch1,els,branch2,aft) ->
      let (br1,_) = get_before_e branch1 [] in
      let (br2,_) = get_before_e branch2 [] in
      (Ast.rewrap s (Ast.IfThenElse(ifheader,br1,els,br2,aft)),[Ast.Other s])
  | Ast.While(header,body,aft) ->
      let (bd,_) = get_before_e body [] in
      (Ast.rewrap s (Ast.While(header,bd,aft)),[Ast.Other s])
  | Ast.For(header,body,aft) ->
      let (bd,_) = get_before_e body [] in
      (Ast.rewrap s (Ast.For(header,bd,aft)),[Ast.Other s])
  | Ast.Do(header,body,tail) ->
      let (bd,_) = get_before_e body [] in
      (Ast.rewrap s (Ast.Do(header,bd,tail)),[Ast.Other s])
  | Ast.Iterator(header,body,aft) ->
      let (bd,_) = get_before_e body [] in
      (Ast.rewrap s (Ast.Iterator(header,bd,aft)),[Ast.Other s])
  | Ast.Switch(header,lb,decls,cases,rb) ->
      let index = count_nested_braces s in
      let (de,dea) = get_before decls [Ast.WParen(lb,index)] in
      let cases =
	List.map
	  (function case_line ->
	    match Ast.unwrap case_line with
	      Ast.CaseLine(header,body) ->
		let (body,_) = get_before body [] in
		Ast.rewrap case_line (Ast.CaseLine(header,body))
	    | Ast.OptCase(case_line) -> failwith "not supported")
	  cases in
      (Ast.rewrap s (Ast.Switch(header,lb,de,cases,rb)),
       [Ast.WParen(rb,index)])
  | Ast.FunDecl(header,lbrace,body,rbrace,aft) ->
      let (bd,_) = get_before body [] in
      (Ast.rewrap s (Ast.FunDecl(header,lbrace,bd,rbrace,aft)),[])
  | _ ->
      Pretty_print_cocci.statement "" s; Format.print_newline();
      failwith "get_before_e: not supported"

let rec get_after sl a =
  let x = Ast.unwrap sl in
  let rec loop sl =
    match sl with
      [] -> ([],a)
    | e::sl ->
	let (sl,sla) = loop sl in
	let (e,ea) = get_after_e e sla in
	(e::sl,ea) in
  let (l,a) = loop x in
  (Ast.rewrap sl l,a)

and get_after_whencode a wc =
  List.map
    (function
	Ast.WhenNot w -> let (w,_) = get_after w a (*?*) in Ast.WhenNot w
      | Ast.WhenAlways w -> let (w,_) = get_after_e w a in Ast.WhenAlways w
      |	Ast.WhenModifier(x) -> Ast.WhenModifier(x)
      | Ast.WhenNotTrue w -> Ast.WhenNotTrue w
      | Ast.WhenNotFalse w -> Ast.WhenNotFalse w)
    wc

and get_after_e s a =
  match Ast.unwrap s with
    Ast.Dots(d,w,bef,_) ->
      (Ast.rewrap s (Ast.Dots(d,get_after_whencode a w,bef,a)),a)
  | Ast.Nest(starter,stmt_dots,ender,w,multi,bef,_) ->
      let w = get_after_whencode a w in
      let (sd,_) = get_after stmt_dots a in
      (*let a =
	 got rid of this. don't want to let nests overshoot
	List.filter
	  (function
	      Ast.Other a ->
		let unifies =
		  Unify_ast.unify_statement_dots
		    (Ast.rewrap s [a]) stmt_dots in
		(match unifies with
		  Unify_ast.MAYBE -> false
		| _ -> true)
	    | Ast.Other_dots a ->
		let unifies = Unify_ast.unify_statement_dots a stmt_dots in
		(match unifies with
		  Unify_ast.MAYBE -> false
		| _ -> true)
	    | _ -> true)
	  a in*)
      (Ast.rewrap s (Ast.Nest(starter,sd,ender,w,multi,bef,a)),
       [Ast.Other_dots stmt_dots])
  | Ast.Disj(stmt_dots_list) ->
      let (dsl,dsla) =
	List.split (List.map (function e -> get_after e a) stmt_dots_list) in
      (Ast.rewrap s (Ast.Disj(dsl)),List.fold_left Common.union_set [] dsla)
  | Ast.Conj(stmt_dots_list) ->
      let (dsl,dsla) =
	List.split (List.map (function e -> get_after e a) stmt_dots_list) in
      (Ast.rewrap s (Ast.Conj(dsl)),List.fold_left Common.union_set [] dsla)
  | Ast.Atomic(ast) ->
      (match Ast.unwrap ast with
	Ast.MetaStmt(nm,cstr,keep,Ast.SequencibleAfterDots _,i) ->
	  (* check "after" information for metavar optimization *)
	  (* if the error is not desired, could just return [], then
	     the optimization (check for EF) won't take place *)
	  List.iter
	    (function
		Ast.Other x ->
		  (match Ast.unwrap x with
		    Ast.Dots(_,_,_,_) | Ast.Nest(_,_,_,_,_,_,_) ->
		      failwith
			"dots/nest not allowed before and after stmt metavar"
		  | _ -> ())
	      |	Ast.Other_dots x ->
		  (match Ast.unwrap x with
		    x::_ ->
		      (match Ast.unwrap x with
			Ast.Dots(_,_,_,_) | Ast.Nest(_,_,_,_,_,_,_) ->
			  failwith
			    ("dots/nest not allowed before and after stmt "^
			     "metavar")
		      | _ -> ())
		  | _ -> ())
	      |	_ -> ())
	    a;
	  (Ast.rewrap s
	     (Ast.Atomic
		(Ast.rewrap s
		   (Ast.MetaStmt
		      (nm,cstr,keep,Ast.SequencibleAfterDots a,i)))),[])
      |	Ast.MetaStmt(_,_,_,_,_) -> (s,[])
      |	_ -> (s,[Ast.Other s]))
  | Ast.Seq(lbrace,body,rbrace) ->
      let index = count_nested_braces s in
      let (bd,_) = get_after body [Ast.WParen(rbrace,index)] in
      (Ast.rewrap s (Ast.Seq(lbrace,bd,rbrace)),
       [Ast.WParen(lbrace,index)])
  | Ast.Define(header,body) ->
      let (body,_) = get_after body a in
      (Ast.rewrap s (Ast.Define(header,body)), [Ast.Other s])
  | Ast.AsStmt(stmt,asstmt) ->
      let (stmt,_) = get_after_e stmt [] in
      let (asstmt,_) = get_after_e asstmt [] in
      (Ast.rewrap s (Ast.AsStmt(stmt,asstmt)),[Ast.Other s])
  | Ast.IfThen(ifheader,branch,aft) ->
      let (br,_) = get_after_e branch a in
      (Ast.rewrap s (Ast.IfThen(ifheader,br,aft)),[Ast.Other s])
  | Ast.IfThenElse(ifheader,branch1,els,branch2,aft) ->
      let (br1,_) = get_after_e branch1 a in
      let (br2,_) = get_after_e branch2 a in
      (Ast.rewrap s (Ast.IfThenElse(ifheader,br1,els,br2,aft)),[Ast.Other s])
  | Ast.While(header,body,aft) ->
      let (bd,_) = get_after_e body ((Ast.Other s) :: a) in
      (Ast.rewrap s (Ast.While(header,bd,aft)),[Ast.Other s])
  | Ast.For(header,body,aft) ->
      let (bd,_) = get_after_e body ((Ast.Other s) :: a) in
      (Ast.rewrap s (Ast.For(header,bd,aft)),[Ast.Other s])
  | Ast.Do(header,body,tail) ->
      let (bd,_) = get_after_e body ((Ast.Other s) :: a) in
      (Ast.rewrap s (Ast.Do(header,bd,tail)),[Ast.Other s])
  | Ast.Iterator(header,body,aft) ->
      let (bd,_) = get_after_e body ((Ast.Other s) :: a) in
      (Ast.rewrap s (Ast.Iterator(header,bd,aft)),[Ast.Other s])
  | Ast.Switch(header,lb,decls,cases,rb) ->
      let index = count_nested_braces s in
      let cases =
	List.map
	  (function case_line ->
	    match Ast.unwrap case_line with
	      Ast.CaseLine(header,body) ->
		let (body,_) = get_after body [Ast.WParen(rb,index)] in
		Ast.rewrap case_line (Ast.CaseLine(header,body))
	    | Ast.OptCase(case_line) -> failwith "not supported")
	  cases in
      let (de,_) = get_after decls [] in
      (Ast.rewrap s (Ast.Switch(header,lb,de,cases,rb)),[Ast.WParen(lb,index)])
  | Ast.FunDecl(header,lbrace,body,rbrace,aft) ->
      let (bd,_) = get_after body [] in
      (Ast.rewrap s (Ast.FunDecl(header,lbrace,bd,rbrace,aft)),[])
  | _ -> failwith "get_after_e: not supported"

let preprocess_dots sl =
  let (sl,_) = get_before sl [] in
  let (sl,_) = get_after sl [] in
  sl

let preprocess_dots_e sl =
  let (sl,_) = get_before_e sl [] in
  let (sl,_) = get_after_e sl [] in
  sl

(* --------------------------------------------------------------------- *)
(* various return_related things *)

let rec ends_in_return_bis preok stmt_list =
  let l = Ast.unwrap stmt_list in
  let contains_dots l =
    List.exists
      (function s ->
	(* doesn't do anything for dots in disj; not sure that makes sense *)
	match Ast.unwrap s with
	  Ast.Nest _ | Ast.Dots _ -> true
	| _ -> false)
      l in
  let preok = preok || contains_dots l in
  match List.rev l with
    x::_ ->
      (match Ast.unwrap x with
	Ast.Atomic(x) ->
	  let rec loop x =
	    match Ast.unwrap x with
	      Ast.Return(_,_) | Ast.ReturnExpr(_,_,_) -> true
	    | Ast.DisjRuleElem((_::_) as l) -> List.for_all loop l
	    | _ -> false in
	  preok && loop x
      | Ast.Disj(disjs) -> List.for_all (ends_in_return_bis preok) disjs
      | Ast.Conj(disjs) -> List.exists (ends_in_return_bis preok) disjs
      | _ -> false)
  | _ -> false

let ends_in_return stmt_list = ends_in_return_bis false stmt_list

(* --------------------------------------------------------------------- *)
(* expressions *)

let exptymatch l make_match make_guard_match =
  let pos = fresh_pos() in
  let matches_guard_matches =
    List.map
      (function x ->
	let pos = Ast.make_mcode pos in
	(make_match (Ast.set_pos x (Some pos)),
	 make_guard_match (Ast.set_pos x (Some pos))))
      l in
  let (matches,guard_matches) = List.split matches_guard_matches in
  let rec suffixes = function
      [] -> []
    | x::xs -> xs::(suffixes xs) in
  let prefixes =
    (* normally, we check that an expression does not match something
       earlier in the disjunction (calculated as prefixes).  But for large
       disjunctions, this can result in a very big CTL formula.  So we
       give the user the option to say he doesn't want this feature, if that is
       the case. *)
    if !Flag_matcher.no_safe_expressions
    then List.map (function _ -> []) matches
    else List.rev (suffixes (List.rev guard_matches)) in
  let info = (* not null *)
    List.map2
      (function matcher ->
	function negates ->
	  CTL.Exists
	    (false,pos,
	     ctl_and CTL.NONSTRICT matcher
	       (ctl_not
		  (ctl_uncheck (List.fold_left ctl_or_fl CTL.False negates)))))
      matches prefixes in
  CTL.InnerAnd(List.fold_left ctl_or_fl CTL.False (List.rev info))

(* code might be a DisjRuleElem, in which case we break it apart
   code might contain an Exp or Ty
   this one pushes the quantifier inwards *)
let do_re_matches label guard res quantified minus_quantified =
  let make_guard_match x =
    let stmt_fvs = Ast.get_mfvs x in
    let fvs = get_unquantified minus_quantified stmt_fvs in
    non_saved_quantify fvs (make_match None true x) in
  let make_match x =
    let stmt_fvs = Ast.get_fvs x in
    let fvs = get_unquantified quantified stmt_fvs in
    quantify guard fvs (make_match None guard x) in
(* label used to be used here, but it is not use; label is only needed after
and within dots
    ctl_and CTL.NONSTRICT (label_pred_maker label) *)
    (match List.map Ast.unwrap res with
      [] -> failwith "unexpected empty disj"
    | Ast.Exp(e)::rest -> exptymatch res make_match make_guard_match
    | Ast.Ty(t)::rest  -> exptymatch res make_match make_guard_match
    | Ast.TopId(i)::rest  -> exptymatch res make_match make_guard_match
    | all ->
	if List.exists
	    (function
		Ast.Exp(_) | Ast.Ty(_)  | Ast.TopId(_) -> true | _ -> false)
	    all
	then failwith "unexpected exp or ty";
	List.fold_left ctl_seqor CTL.False (List.map make_match res))

(* code might be a DisjRuleElem, in which case we break it apart
   code doesn't contain an Exp or Ty
   this one is for use when it is not practical to push the quantifier inwards
 *)
let header_match label guard code : ('a, Ast.meta_name, 'b) CTL.generic_ctl =
  match Ast.unwrap code with
    Ast.DisjRuleElem(res) ->
      let make_match = make_match None guard in
      let orop = if guard then ctl_or else ctl_seqor in
(* label used to be used here, but it is not use; label is only needed after
and within dots
      ctl_and CTL.NONSTRICT (label_pred_maker label) *)
      (List.fold_left orop CTL.False (List.map make_match res))
  | _ -> make_match label guard code

(* --------------------------------------------------------------------- *)
(* control structures *)

let end_control_structure fvs header body after_pred
    after_checks no_after_checks (afvs,afresh,ainh,aft) after label guard =
  (* aft indicates what is added after the whole if, which has to be added
     to the endif node *)
  let (aft_needed,after_branch) =
    match aft with
      Ast.CONTEXT(_,Ast.NOTHING) ->
	(false,make_seq_after2 guard after_pred after)
    | _ ->
	let match_endif =
	  make_match label guard
	    (make_meta_rule_elem "1" aft Ast.CstrTrue (afvs,afresh,ainh)) in
	(true,
	 make_seq_after_ex guard after_pred
	   (After(make_seq_after guard match_endif after))) in
  let body = body after_branch in
  let s = guard_to_strict guard in
  (* the code *)
  quantify guard fvs
    (ctl_and s header
       (opt_and guard
	  (match (after,aft_needed) with
	    (After _,_) (* pattern doesn't end here *)
	  | (_,true) (* + code added after *) -> after_checks
	  | _ -> no_after_checks)
	  (match body with
	    Common.Left body ->
	      (* fixes set of choices, as for if *)
	      (List.fold_left (ctl_and s) CTL.True (List.map ctl_ex body))
	  | Common.Right body ->
	      (* unknown set of choices, as for switch *)
	      ctl_ax_absolute s body)))

let ifthen ifheader branch ((afvs,_,_,_) as aft) after
    quantified minus_quantified label llabel slabel recurse make_match guard =
(* "if (test) thn" becomes:
    if(test) & AX((TrueBranch & AX thn) v FallThrough v After)

    "if (test) thn; after" becomes:
    if(test) & AX((TrueBranch & AX thn) v FallThrough v (After & AXAX after))
             & EX After
*)
  (* free variables *)
  let (efvs,bfvs) =
    match seq_fvs quantified
	[Ast.get_fvs ifheader;Ast.get_fvs branch;afvs] with
      [(efvs,b1fvs);(_,b2fvs);_] -> (efvs,Common.union_set b1fvs b2fvs)
    | _ -> failwith "asttoctl2: not possible 3" in
  let new_quantified = Common.union_set bfvs quantified in
  let (mefvs,mbfvs) =
    match seq_fvs minus_quantified
	[Ast.get_mfvs ifheader;Ast.get_mfvs branch;[]] with
      [(efvs,b1fvs);(_,b2fvs);_] -> (efvs,Common.union_set b1fvs b2fvs)
    | _ -> failwith "asttoctl2: not possible 4" in
  let new_mquantified = Common.union_set mbfvs minus_quantified in
  (* if header *)
  let if_header = quantify guard efvs (make_match ifheader) in
  (* then branch and after *)
  let lv = get_label_ctr() in
  let used = ref false in
  let true_branch =
    (* no point to put a label on truepred etc; it is local to this construct
       so it must have the same label *)
    make_seq guard
      [truepred None; recurse branch NotTop Tail new_quantified new_mquantified
	  (Some (lv,used)) llabel slabel guard] in
  let after_pred = aftpred None in
  let or_cases after_branch =
    Common.Left [true_branch; fallpred None; after_branch] in
  let (if_header,wrapper) =
    if !used
    then
      let label_pred = CTL.Pred (Lib_engine.Label(lv),CTL.Control) in
      (ctl_and CTL.NONSTRICT(*???*) if_header label_pred,
       (function body -> quantify true [lv] body))
    else (if_header,function x -> x) in
  wrapper
    (end_control_structure bfvs if_header or_cases after_pred
	(Some(ctl_ex after_pred)) None aft after label guard)

let ifthenelse ifheader branch1 els branch2 ((afvs,_,_,_) as aft) after
    quantified minus_quantified label llabel slabel recurse make_match guard =
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
*)
  (* free variables *)
  let (e1fvs,b1fvs,s1fvs) =
    match seq_fvs quantified
	[Ast.get_fvs ifheader;Ast.get_fvs branch1;afvs] with
      [(e1fvs,b1fvs);(s1fvs,b1afvs);_] ->
	(e1fvs,Common.union_set b1fvs b1afvs,s1fvs)
    | _ -> failwith "asttoctl2: not possible 5" in
  let (e2fvs,b2fvs,s2fvs) =
    (* fvs on else? *)
    (* just combine with the else branch.  no point to have separate
       quantifier, since there is only one possible control-flow path *)
    let else_fvs = Common.union_set (Ast.get_fvs els) (Ast.get_fvs branch2) in
    match seq_fvs quantified [Ast.get_fvs ifheader;else_fvs;afvs] with
      [(e2fvs,b2fvs);(s2fvs,b2afvs);_] ->
        (e2fvs,Common.union_set b2fvs b2afvs,s2fvs)
    | _ -> failwith "asttoctl2: not possible 6" in
  let bothfvs        = union (union b1fvs b2fvs) (intersect s1fvs s2fvs) in
  let exponlyfvs     = intersect e1fvs e2fvs in
  let new_quantified = union bothfvs quantified in
  (* minus free variables *)
  let (me1fvs,mb1fvs,ms1fvs) =
    match seq_fvs minus_quantified
	[Ast.get_mfvs ifheader;Ast.get_mfvs branch1;[]] with
      [(e1fvs,b1fvs);(s1fvs,b1afvs);_] ->
	(e1fvs,Common.union_set b1fvs b1afvs,s1fvs)
    | _ -> failwith "asttoctl2: not possible 7" in
  let (me2fvs,mb2fvs,ms2fvs) =
    (* fvs on else? *)
    (* just combine with the else branch.  no point to have separate
       quantifier, since there is only one possible control-flow path *)
    let else_mfvs =
      Common.union_set (Ast.get_mfvs els) (Ast.get_mfvs branch2) in
    match seq_fvs minus_quantified [Ast.get_mfvs ifheader;else_mfvs;[]] with
      [(e2fvs,b2fvs);(s2fvs,b2afvs);_] ->
	(e2fvs,Common.union_set b2fvs b2afvs,s2fvs)
    | _ -> failwith "asttoctl2: not possible 8" in
  let mbothfvs       = union (union mb1fvs mb2fvs) (intersect ms1fvs ms2fvs) in
  let new_mquantified = union mbothfvs minus_quantified in
  (* if header *)
  let if_header = quantify guard exponlyfvs (make_match ifheader) in
  (* then and else branches *)
  let lv = get_label_ctr() in
  let used = ref false in
  let true_branch =
    make_seq guard
      [truepred None; recurse branch1 NotTop Tail new_quantified new_mquantified
	  (Some (lv,used)) llabel slabel guard] in
  let false_branch =
    make_seq guard
      [falsepred None;
	quantify guard
	  (Common.minus_set (Ast.get_fvs els) new_quantified)
	  (header_match None guard els);
	recurse branch2 NotTop Tail new_quantified new_mquantified
	  (Some (lv,used)) llabel slabel guard] in
  let after_pred = aftpred None in
  let or_cases after_branch =
    Common.Left [true_branch; false_branch; after_branch] in
  let s = guard_to_strict guard in
  let (if_header,wrapper) =
    if !used
    then
      let label_pred = CTL.Pred (Lib_engine.Label(lv),CTL.Control) in
      (ctl_and CTL.NONSTRICT(*???*) if_header label_pred,
       (function body -> quantify true [lv] body))
    else (if_header,function x -> x) in
  wrapper
    (end_control_structure bothfvs if_header or_cases after_pred
      (Some(ctl_and s (ctl_ex (falsepred None)) (ctl_ex after_pred)))
      (Some(ctl_ex (falsepred None)))
      aft after label guard)

let forwhile header body ((afvs,_,_,_) as aft) after
    quantified minus_quantified label recurse make_match guard =
  let process _ =
    (* the translation in this case is similar to that of an if with no else *)
    (* free variables *)
    let (efvs,bfvs) =
      match seq_fvs quantified [Ast.get_fvs header;Ast.get_fvs body;afvs] with
	[(efvs,b1fvs);(_,b2fvs);_] -> (efvs,Common.union_set b1fvs b2fvs)
      | _ -> failwith "asttoctl2: not possible 9" in
    let new_quantified = Common.union_set bfvs quantified in
    (* minus free variables *)
    let (mefvs,mbfvs) =
      match seq_fvs minus_quantified
	  [Ast.get_mfvs header;Ast.get_mfvs body;[]] with
	[(efvs,b1fvs);(_,b2fvs);_] -> (efvs,Common.union_set b1fvs b2fvs)
      | _ -> failwith "asttoctl2: not possible 10" in
    let new_mquantified = Common.union_set mbfvs minus_quantified in
    (* loop header *)
    let header = quantify guard efvs (make_match header) in
    let lv = get_label_ctr() in
    let used = ref false in
    let body =
      make_seq guard
	[inlooppred None;
	  recurse body NotTop Tail new_quantified new_mquantified
	    (Some (lv,used)) (Some (lv,used)) None guard] in
    let after_pred = loopfallpred None in
    let or_cases after_branch =
      Common.Left [body; after_branch] in
    let (header,wrapper) =
      if !used
      then
	let label_pred = CTL.Pred (Lib_engine.Label(lv),CTL.Control) in
	(ctl_and CTL.NONSTRICT(*???*) header label_pred,
	 (function body -> quantify true [lv] body))
      else (header,function x -> x) in
    wrapper
      (end_control_structure bfvs header or_cases after_pred
	 (Some(ctl_ex after_pred)) None aft after label guard) in
  match (Ast.unwrap body,aft) with
    (Ast.Atomic(re),(_,_,_,Ast.CONTEXT(_,Ast.NOTHING))) ->
      (match Ast.unwrap re with
	Ast.MetaStmt((_,_,Ast.CONTEXT(_,Ast.NOTHING),_),
		     Ast.CstrTrue, Ast.Unitary,_,false)
	when after = Tail || after = End || after = VeryEnd ->
	  let (efvs) =
	    match seq_fvs quantified [Ast.get_fvs header] with
	      [(efvs,_)] -> efvs
	    | _ -> failwith "asttoctl2: not possible 11" in
	  quantify guard efvs (make_match header)
      | _ -> process())
  | _ -> process()

(* --------------------------------------------------------------------- *)
(* statement metavariables *)

(* issue: an S metavariable that is not an if branch/loop body
   should not match an if branch/loop body, so check that the labels
   of the nodes before the first node matched by the S are different
   from the label of the first node matched by the S *)
let sequencibility body label_pred process_bef_aft = function
    Ast.Sequencible | Ast.SequencibleAfterDots [] ->
      body
	(function x ->
	  (ctl_and CTL.NONSTRICT (ctl_not (ctl_back_ax label_pred)) x))
  | Ast.SequencibleAfterDots l ->
      (* S appears after some dots.  l is the code that comes after the S.
	 want to search for that first, because S can match anything, while
	 the stuff after is probably more restricted *)
      let afts = List.map process_bef_aft l in
      let ors = foldl1 ctl_or afts in
      ctl_and CTL.NONSTRICT
	(ctl_ef (ctl_and CTL.NONSTRICT ors (ctl_back_ax label_pred)))
	(body
	   (function x ->
	     ctl_and CTL.NONSTRICT (ctl_not (ctl_back_ax label_pred)) x))
  | Ast.NotSequencible -> body (function x -> x)

let svar_context_with_add_after stmt s label quantified d ast
    seqible after process_bef_aft guard fvinfo =
  let label_var = (*fresh_label_var*) string2var "_lab" in
  let label_pred =
    CTL.Pred (Lib_engine.Label(label_var),CTL.Control) in
  (*let prelabel_pred =
    CTL.Pred (Lib_engine.PrefixLabel(label_var),CTL.Control) in*)
  let matcher d =
    make_match None guard (make_meta_rule_elem "2" d Ast.CstrTrue fvinfo) in
  let full_metamatch = matcher d in
  let first_metamatch =
    matcher
      (match d with
	Ast.CONTEXT(pos,Ast.BEFOREAFTER(bef,_,c)) ->
	  Ast.CONTEXT(pos,Ast.BEFORE(bef,c))
      |	Ast.CONTEXT(pos,_) -> Ast.CONTEXT(pos,Ast.NOTHING)
      | Ast.MINUS(_,_,_,_)
      | Ast.PLUS _ -> failwith "asttoctl2: not possible 12") in
(*
  let middle_metamatch =
    matcher
      (match d with
	Ast.CONTEXT(pos,_) -> Ast.CONTEXT(pos,Ast.NOTHING)
      | Ast.MINUS(_,_,_,_)
      | Ast.PLUS _ -> failwith "asttoctl2: not possible 13") in
*)
  let last_metamatch =
    matcher
      (match d with
	Ast.CONTEXT(pos,Ast.BEFOREAFTER(_,aft,c)) ->
	  Ast.CONTEXT(pos,Ast.AFTER(aft,c))
      |	Ast.CONTEXT(_,_) -> d
      | Ast.MINUS(_,_,_,_)
      | Ast.PLUS _ -> failwith "asttoctl2: not possible 14") in

(*
  let rest_nodes =
    ctl_and CTL.NONSTRICT middle_metamatch prelabel_pred in
*)

  let to_end = ctl_or (aftpred None) (loopfallpred None) in
  let left_or = (* the whole statement is one node *)
    make_seq_after guard
      (ctl_and CTL.NONSTRICT (ctl_not (ctl_ex to_end)) full_metamatch) after in
  let right_or = (* the statement covers multiple nodes *)
    ctl_and CTL.NONSTRICT
      (ctl_ex
	 (make_seq_ex guard
	    [to_end; make_seq_after guard last_metamatch after]))
      first_metamatch in

(*
  let left_or =
    make_seq guard
      [full_metamatch; and_after guard (ctl_not prelabel_pred) after] in
  let right_or =
    make_seq guard
      [first_metamatch;
        ctl_au CTL.NONSTRICT
	  rest_nodes
	  (make_seq guard
	     [ctl_and CTL.NONSTRICT last_metamatch label_pred;
	       and_after guard
		 (ctl_not prelabel_pred) after])] in
*)

  let body f =
    ctl_and CTL.NONSTRICT label_pred
       (f (ctl_and CTL.NONSTRICT
	    (make_raw_match label false ast) (ctl_or left_or right_or))) in
  let stmt_fvs = Ast.get_fvs stmt in
  let fvs = get_unquantified quantified stmt_fvs in
  quantify guard (label_var::fvs)
    (sequencibility body label_pred process_bef_aft seqible)

let svar_minus_or_no_add_after stmt s label quantified d ast
    seqible after process_bef_aft guard fvinfo =
  let label_var = (*fresh_label_var*) string2var "_lab" in
  let label_pred =
    CTL.Pred (Lib_engine.Label(label_var),CTL.Control) in
  let prelabel_pred =
    CTL.Pred (Lib_engine.PrefixLabel(label_var),CTL.Control) in
  let matcher d =
    make_match None guard (make_meta_rule_elem "3" d Ast.CstrTrue fvinfo) in
  let ender =
    match (d,after) with
      (Ast.PLUS _, _) -> failwith "asttoctl2: not possible 15"
    | (Ast.CONTEXT(pos,Ast.NOTHING),(Tail|End|VeryEnd)) ->
	(* just match the root. don't care about label; always ok *)
	make_raw_match None false ast
    | (Ast.CONTEXT(pos,Ast.BEFORE(_,_)),(Tail|End|VeryEnd)) ->
	ctl_and CTL.NONSTRICT
	  (make_raw_match None false ast) (* statement *)
	  (matcher d)                     (* transformation *)
    | (Ast.CONTEXT(pos,(Ast.NOTHING|Ast.BEFORE(_,_))),
       ((After a | Guard a) as after)) ->
	(* This case and the MINUS one could probably be merged, if
	   HackForStmt were to notice when its arguments are trivial *)
	 (* not really sure what this is doing, esp is_compound... *)
	let first_metamatch = matcher d in
	(* try to follow after link *)
	let to_end = ctl_or (aftpred None) (loopfallpred None) in
	let is_compound =
	  ctl_ex
	    (make_seq_ex guard
	       [to_end; make_seq_after guard CTL.True after]) in
	let not_compound =
	  make_seq_after guard (ctl_not (ctl_ex to_end)) after in
	ctl_and CTL.NONSTRICT (make_raw_match label false ast)
	  (ctl_and CTL.NONSTRICT
	     first_metamatch (ctl_or is_compound not_compound))
    | (Ast.CONTEXT(pos,(Ast.AFTER _|Ast.BEFOREAFTER _)),_) ->
	failwith "asttoctl2: not possible 16"
    | (Ast.MINUS(pos,inst,adj,l),after) ->
	let (first_metamatch,last_metamatch,rest_metamatch) =
	  match l with
	    Ast.NOREPLACEMENT ->
	      (matcher(Ast.CONTEXT(pos,Ast.NOTHING)),CTL.True,matcher d)
	  | _ -> (matcher d,
		  matcher(Ast.MINUS(pos,inst,adj,Ast.NOREPLACEMENT)),
		  ctl_and CTL.NONSTRICT
		    (ctl_not (make_raw_match label false ast))
		    (matcher(Ast.MINUS(pos,inst,adj,Ast.NOREPLACEMENT)))) in
	(* try to follow after link *)
	let to_end = ctl_or (aftpred None) (loopfallpred None) in
	let is_compound =
	  ctl_ex
	    (make_seq_ex guard
	       [to_end; make_seq_after guard last_metamatch after]) in
	let not_compound =
	  make_seq_after guard (ctl_not (ctl_ex to_end)) after in
	ctl_and CTL.NONSTRICT
	  (ctl_and CTL.NONSTRICT (make_raw_match label false ast)
	     (ctl_and CTL.NONSTRICT
		first_metamatch (ctl_or is_compound not_compound)))
          (* don't have to put anything before the beginning, so don't have to
          distinguish the first node.  so don't have to bother about paths,
          just use the label. label ensures that found nodes match up with
          what they should because it is in the lhs of the andany. *)
	  (CTL.HackForStmt(CTL.FORWARD,CTL.NONSTRICT,
			   ctl_and CTL.NONSTRICT label_pred
			     (make_raw_match label false ast),
			   ctl_and CTL.NONSTRICT prelabel_pred rest_metamatch))
  in
  let body f = ctl_and CTL.NONSTRICT label_pred (f ender) in
  let stmt_fvs = Ast.get_fvs stmt in
  let fvs = get_unquantified quantified stmt_fvs in
  quantify guard (label_var::fvs)
    (sequencibility body label_pred process_bef_aft seqible)

(* --------------------------------------------------------------------- *)
(* dots and nests *)

let dots_au is_strict toend label s wrapcode n x seq_after y quantifier =
  let matchgoto = gotopred None in
  let matchbreak =
    make_match None false
      (wrapcode
	 (Ast.Break(Ast.make_mcode "break",Ast.make_mcode ";"))) in
  let matchcontinue =
     make_match None false
      (wrapcode
	 (Ast.Continue(Ast.make_mcode "continue",Ast.make_mcode ";"))) in
  let op = if quantifier = !exists then ctl_au else ctl_anti_au in
  let stop_early =
    if quantifier = Exists
    then Common.Left(CTL.False)
    else if toend
    then Common.Left(CTL.Or(aftpred label,exitpred label))
    else if is_strict
    then Common.Left(aftpred label)
    else
      Common.Right
	(function vx -> function v ->
	  (* vx is the contents of the nest, if any.  we can only stop early
	     if we find neither the ending code nor the nest contents in
	     the if branch.  not sure if this is a good idea. *)
	  let lv = get_label_ctr() in
	  let labelpred = CTL.Pred(Lib_engine.Label lv,CTL.Control) in
	  let preflabelpred = label_pred_maker (Some (lv,ref true)) in
	  (*let is_paren =
	    (* Rather a special case.  But if the code afterwards is just
	       a } then there is no point checking after a goto that it does
	       not appear. *)
	    (* this optimization doesn't work.  probably depends on whether
	       the destination of the break/goto is local or more global than
	       the dots *)
	    match seq_after with
	      CTL.And(_,e1,e2) ->
		let is_paren = function
		    CTL.Pred(Lib_engine.Paren _,_) -> true
		  | _ -> false in
		is_paren e1 or is_paren e2
	    | _ -> false in *)
	  ctl_or (aftpred label)
	    (quantify false [lv]
	       (ctl_and CTL.NONSTRICT
		  (ctl_and CTL.NONSTRICT (esctruepred label) labelpred)
		  (op CTL.NONSTRICT
		     (ctl_and CTL.NONSTRICT (ctl_not v)
			(ctl_and CTL.NONSTRICT vx preflabelpred))
		     (ctl_and CTL.NONSTRICT preflabelpred
			(if !Flag_matcher.only_return_is_error_exit
			then
			  (ctl_and CTL.NONSTRICT
			     (retpred None) (ctl_not seq_after))
			else
			  (ctl_or
			     (ctl_and CTL.NONSTRICT
				(ctl_or (aftpred None)
				   (ctl_or (retpred None) matchcontinue))
				(ctl_not seq_after))
			     (ctl_and CTL.NONSTRICT
				(ctl_or matchgoto matchbreak)
				((*if is_paren
                                (* an optim that failed see defn is_paren
                                   and tests/makes_a_loop *)
				then CTL.True
				else*)
				  (ctl_ag s
				     (ctl_not seq_after))))))))))) in
  let v = get_let_ctr() in
  op s x
    (match stop_early with
      Common.Left x1 -> ctl_or y x1
    | Common.Right stop_early ->
	CTL.Let(v,y,
		ctl_or (CTL.Ref v)
		  (ctl_and CTL.NONSTRICT (label_pred_maker label)
		     (stop_early n (CTL.Ref v)))))

let get_quantifier whencodes =
  let check_quantifier whencodes quant other =
    if List.exists (function Ast.WhenModifier(x) -> x = quant | _ -> false)
	whencodes
    then
      if List.exists (function Ast.WhenModifier(x) -> x = other | _ -> false)
	  whencodes
      then failwith "inconsistent annotation on dots"
      else true
    else false in
  if check_quantifier whencodes Ast.WhenExists Ast.WhenForall
  then Exists
  else
    if check_quantifier whencodes Ast.WhenForall Ast.WhenExists
    then Forall
    else !exists

let rec dots_and_nests plus nest whencodes bef aft dotcode after label
    process_bef_aft statement_list statement guard quantified wrapcode =
  let ctl_and_ns = ctl_and CTL.NONSTRICT in
  (* process bef_aft *)
  let shortest l =
    List.fold_left ctl_or_fl CTL.False (List.map process_bef_aft l) in
  let bef_aft = (* to be negated *)
    try
      let _ =
	List.find
	  (function Ast.WhenModifier(Ast.WhenAny) -> true | _ -> false)
	  whencodes in
      CTL.False
    with Not_found -> shortest (Common.union_set bef aft) in
  let is_strict =
    List.exists
      (function Ast.WhenModifier(Ast.WhenStrict) -> true | _ -> false)
      whencodes in
  let quantifier = get_quantifier whencodes in
  (* the following is used when we find a goto, etc and consider accepting
     without finding the rest of the pattern *)
  let aft = shortest aft in
  (* process whencode *)
  let labelled = label_pred_maker label in
  let whencodes arg =
    let (poswhen,negwhen) =
      List.fold_left
	(function (poswhen,negwhen) ->
	  function
	      Ast.WhenNot whencodes ->
		(poswhen,ctl_or (statement_list whencodes) negwhen)
	    | Ast.WhenAlways stm ->
		(ctl_and CTL.NONSTRICT (statement stm) poswhen,negwhen)
	    | Ast.WhenModifier(_) -> (poswhen,negwhen)
	    | Ast.WhenNotTrue(e) ->
		(poswhen,
		  ctl_or (whencond_true e label guard quantified) negwhen)
	    | Ast.WhenNotFalse(e) ->
		(poswhen,
		  ctl_or (whencond_false e label guard quantified) negwhen))
	(CTL.True,CTL.False(*bef_aft*)) (List.rev whencodes) in
    (*bef_aft modifies arg so that inside of a nest can't cause the next
       to overshoot its boundaries, eg a() <...f()...> b() where f is
       a metavariable and the whole thing matches code in a loop;
       don't want f to match eg b(), allowing to go around the loop again*)
    let poswhen = ctl_and_ns arg poswhen in
    let negwhen =
(*    if !exists
      then*)
        (* add in After, because it's not part of the program *)
	ctl_or (aftpred label) negwhen
      (*else negwhen*) in
    ctl_and_ns poswhen (ctl_not negwhen) in
  (* process dot code, if any *)
  let dotcode =
    match (dotcode,guard) with
      (None,_) | (_,true) -> CTL.True
    | (Some dotcode,_) -> dotcode in
  (* process nest code, if any *)
  (* whencode goes in the negated part of the nest; if no nest, just goes
      on the "true" in between code *)
  let plus_var = if plus then get_label_ctr() else string2var "" in
  let plus_var2 = if plus then get_label_ctr() else string2var "" in
  let (ornest,just_nest) =
    (* just_nest is used when considering whether to stop early, to continue
       to collect nest information in the if branch *)
    match (nest,guard && not plus) with
      (None,_) | (_,true) -> (whencodes CTL.True,CTL.True)
    | (Some nest,false) ->
	let v = get_let_ctr() in
	let is_plus x =
	  if plus
	  then
	    (* the idea is that BindGood is sort of a witness; a witness to
	       having found the subterm in at least one place.  If there is
	       not a witness, then there is a risk that it will get thrown
	       away, if it is merged with a node that has an empty
	       environment.  See tests/nestplus.  But this all seems
	       rather suspicious *)
	    CTL.And(CTL.NONSTRICT,x,
		    CTL.Exists(true,plus_var2,
			       CTL.Pred(Lib_engine.BindGood(plus_var),
					CTL.Modif plus_var2)))
	  else x in
	let body =
	  CTL.Let(v,nest,
		  CTL.Or(is_plus (CTL.Ref v),
			 whencodes (CTL.Not(ctl_uncheck (CTL.Ref v))))) in
        (body,body) in
  let plus_modifier x =
    if plus
    then
      CTL.Exists
	(false,plus_var,
	 (CTL.And
	    (CTL.NONSTRICT,x,
	     CTL.Not(CTL.Pred(Lib_engine.BindBad(plus_var),CTL.Control)))))
    else x in

  let ender =
    match after with
      (* label within dots is taken care of elsewhere.  the next two lines
         put the label on the code following dots *)
      After f -> ctl_and (guard_to_strict guard) f labelled
    | Guard f ->
	(* actually, label should be None based on the only use of Guard... *)
	assert (label = None);
	ctl_and CTL.NONSTRICT (ctl_uncheck f) labelled
    | VeryEnd ->
	let exit = endpred label in
	let errorexit = exitpred label in
	ctl_or exit errorexit
    (* not at all sure what the next two mean... *)
    | End -> CTL.True
    | Tail ->
	(match label with
	  Some (lv,used) -> used := true;
	    ctl_or (CTL.Pred(Lib_engine.Label lv,CTL.Control))
	      (ctl_back_ex (ctl_or (retpred label) (gotopred label)))
	| None -> endpred label)
	  (* was the following, but not clear why sgrep should allow
	     incomplete patterns
	let exit = endpred label in
	let errorexit = exitpred label in
	if !exists
	then ctl_or exit errorexit (* end anywhere *)
	else exit (* end at the real end of the function *) *) in
  let exists_without_after =
    if quantifier = Exists
    then (ctl_not (aftpred None))
    else CTL.True in
  plus_modifier
    (dots_au is_strict ((after = Tail) || (after = VeryEnd))
       label (guard_to_strict guard) wrapcode just_nest
       (ctl_and_ns exists_without_after
	  (ctl_and_ns dotcode
	     (ctl_and_ns (ctl_and_ns (ctl_not bef_aft) ornest) labelled)))
       aft ender quantifier)

and get_whencond_exps e =
  match Ast.unwrap e with
    Ast.Exp e -> [e]
  | Ast.DisjRuleElem(res) ->
      List.fold_left Common.union_set [] (List.map get_whencond_exps res)
  | _ -> failwith "asttoctl2: not possible 17"

and make_whencond_headers e e1 label guard quantified =
  let fvs = Ast.get_fvs e in
  let header_pred h =
    quantify guard (get_unquantified quantified fvs)
      (make_match label guard h) in
  let if_header e1 =
    header_pred
      (Ast.rewrap e
	 (Ast.IfHeader
	    (Ast.make_mcode "if",
	     Ast.make_mcode "(",e1,Ast.make_mcode ")"))) in
  let while_header e1 =
    header_pred
      (Ast.rewrap e
	 (Ast.WhileHeader
	    (Ast.make_mcode "while",
	     Ast.make_mcode "(",e1,Ast.make_mcode ")"))) in
  let for_header e1 =
    header_pred
      (Ast.rewrap e
	 (Ast.ForHeader
	    (Ast.make_mcode "for",Ast.make_mcode "(",
	     Ast.ForExp(None,Ast.make_mcode ";"),
	     Some e1,Ast.make_mcode ";",None,Ast.make_mcode ")"))) in
  let if_headers =
    List.fold_left ctl_or CTL.False (List.map if_header e1) in
  let while_headers =
    List.fold_left ctl_or CTL.False (List.map while_header e1) in
  let for_headers =
    List.fold_left ctl_or CTL.False (List.map for_header e1) in
  (if_headers, while_headers, for_headers)

and whencond_true e label guard quantified =
  let e1 = get_whencond_exps e in
  let (if_headers, while_headers, for_headers) =
    make_whencond_headers e e1 label guard quantified in
  ctl_or
    (ctl_and CTL.NONSTRICT (truepred label) (ctl_back_ex if_headers))
    (ctl_and CTL.NONSTRICT
       (inlooppred label) (ctl_back_ex (ctl_or while_headers for_headers)))

and whencond_false e label guard quantified =
  let e1 = get_whencond_exps e in
  let (if_headers, while_headers, for_headers) =
    make_whencond_headers e e1 label guard quantified in
  (* if with else *)
  ctl_or (ctl_and CTL.NONSTRICT (falsepred label) (ctl_back_ex if_headers))
    (* if without else *)
    (ctl_or (ctl_and CTL.NONSTRICT (fallpred label) (ctl_back_ex if_headers))
       (* failure of loop test *)
       (ctl_and CTL.NONSTRICT (loopfallpred label)
	  (ctl_or (ctl_back_ex while_headers) (ctl_back_ex for_headers))))

(* --------------------------------------------------------------------- *)
(* the main translation loop *)

let rec statement_list stmt_list top after quantified minus_quantified
    label llabel slabel dots_before guard =
  let isdots x =
    (* include Disj to be on the safe side *)
    match Ast.unwrap x with
      Ast.Dots _ | Ast.Nest _ | Ast.Disj _ | Ast.Conj _ -> true | _ -> false in
  let compute_label l e db = if db || isdots e then l else None in
  let x = Ast.unwrap stmt_list in
  let rec loop top quantified minus_quantified dots_before label llabel slabel
    = function
	([],_,_) -> (match after with After f -> f | _ -> CTL.True)
      | ([e],_,_) ->
	  statement e top after quantified minus_quantified
	    (compute_label label e dots_before)
	    llabel slabel guard
      | (e::sl,fv::fvs,mfv::mfvs) ->
	  let shared = intersectll fv fvs in
	  let unqshared = get_unquantified quantified shared in
	  let new_quantified = Common.union_set unqshared quantified in
	  let minus_shared = intersectll mfv mfvs in
	  let munqshared =
	    get_unquantified minus_quantified minus_shared in
	  let new_mquantified =
	    Common.union_set munqshared minus_quantified in
	  quantify guard unqshared
	    (statement e top
	       (After
		  (let (label1,llabel1,slabel1) =
		    match Ast.unwrap e with
		      Ast.Atomic(re) ->
			(match Ast.unwrap re with
			  Ast.Goto _ -> (None,None,None)
			| _ -> (label,llabel,slabel))
		    |	_ -> (label,llabel,slabel) in
		  loop NotTop new_quantified new_mquantified (isdots e)
		    label1 llabel1 slabel1
		    (sl,fvs,mfvs)))
	       new_quantified new_mquantified
	       (compute_label label e dots_before) llabel slabel guard)
      | _ -> failwith "asttoctl2: not possible 18" in
  loop top quantified minus_quantified dots_before
    label llabel slabel
    (x,List.map Ast.get_fvs x,List.map Ast.get_mfvs x)

(* llabel is the label of the enclosing loop and slabel is the label of the
   enclosing switch *)
and statement stmt top after quantified minus_quantified
    label llabel slabel guard =
  let ctl_au     = ctl_au CTL.NONSTRICT in
  let ctl_ax     = ctl_ax CTL.NONSTRICT in
  let ctl_and    = ctl_and CTL.NONSTRICT in
  let make_seq   = make_seq guard in
  let make_seq_after = make_seq_after guard in
  let real_make_match = make_match in
  let make_match = header_match label guard in

  let dots_done = ref false in (* hack for dots cases we can easily handle *)

  let term =
  match Ast.unwrap stmt with
    Ast.Atomic(ast) ->
      (match Ast.unwrap ast with
	(* the following optimization is not a good idea, because when S
	   is alone, we would like it not to match a declaration.
	   this makes more matching for things like when (...) S, but perhaps
	   that matching is not so costly anyway *)
        (*Ast.MetaStmt(_,Ast.Unitary,_,false) when guard -> CTL.True*)
      |	Ast.MetaStmt((s,_,(Ast.CONTEXT(_,Ast.BEFOREAFTER(_,_,_)) as d),_),cstr,
		     keep,seqible,_)
      | Ast.MetaStmt((s,_,(Ast.CONTEXT(_,Ast.AFTER(_,_)) as d),_),cstr,
		     keep,seqible,_)->
	  svar_context_with_add_after stmt s label quantified d ast seqible
	    after
	    (process_bef_aft quantified minus_quantified
	       label llabel slabel true)
	    guard
	    (Ast.get_fvs stmt, Ast.get_fresh stmt, Ast.get_inherited stmt)

      |	Ast.MetaStmt((s,_,d,_),cstr,keep,seqible,_) ->
	  svar_minus_or_no_add_after stmt s label quantified d ast seqible
	    after
	    (process_bef_aft quantified minus_quantified
	       label llabel slabel true)
	    guard
	    (Ast.get_fvs stmt, Ast.get_fresh stmt, Ast.get_inherited stmt)

      |	_ ->
	  let term ast =
	    match Ast.unwrap ast with
	      Ast.DisjRuleElem(res) ->
		do_re_matches label guard res quantified minus_quantified
	    | Ast.Exp(_) | Ast.Ty(_) | Ast.TopId(_) ->
		let stmt_fvs = Ast.get_fvs stmt in
		let fvs = get_unquantified quantified stmt_fvs in
		CTL.InnerAnd(quantify guard fvs (make_match ast))
	    | _ ->
		let stmt_fvs = Ast.get_fvs stmt in
		let fvs = get_unquantified quantified stmt_fvs in
		quantify guard fvs (make_match ast) in
	  match Ast.unwrap ast with
	    Ast.Break(brk,semi) ->
	      let term = term ast in
	      (match (llabel,slabel) with
		(_,Some(lv,used)) -> (* use switch label if there is one *)
		  ctl_and term (bclabel_pred_maker slabel)
	      | _ -> ctl_and term (bclabel_pred_maker llabel))
	  | Ast.Continue(brk,semi) ->
	      ctl_and (term ast) (bclabel_pred_maker llabel)
	  | Ast.Return((_,info,retmc,pos),(_,_,semmc,spos)) ->
	      (* discard pattern that comes after return *)
	      let normal_res = make_seq_after (term ast) after in
	      (* the following code tries to propagate the modifications on
		 return; to a close brace, in the case where the final return
		 is absent *)
	      let new_mc =
		match (retmc,semmc) with
		  (Ast.MINUS(_,inst1,adj1,l1),Ast.MINUS(_,_,_,l2))
		  when !Flag.sgrep_mode2 ->
		    (* in sgrep mode, we can propagate the - *)
		    let new_info =
		      match (l1,l2) with
			(Ast.NOREPLACEMENT,Ast.NOREPLACEMENT) ->
			  Ast.NOREPLACEMENT
		      |	_ ->
			  failwith "no replacements allowed in sgrep mode" in
		    Some (Ast.MINUS(Ast.NoPos,inst1,adj1,new_info))
		| (Ast.MINUS(_,_,_,l1),Ast.MINUS(_,_,_,l2)) ->
		    let change =
		      match (l1,l2) with
			(Ast.NOREPLACEMENT,Ast.NOREPLACEMENT) ->
			  Ast.NOTHING
		      |	(Ast.NOREPLACEMENT,Ast.REPLACEMENT(l,ct))
		      |	(Ast.REPLACEMENT(l,ct),Ast.NOREPLACEMENT) ->
			  Ast.BEFORE(l,ct)
		      |	(Ast.REPLACEMENT(l1,ct1),Ast.REPLACEMENT(l2,ct2)) ->
			  Ast.BEFORE(l1@l2,Ast.lub_count ct1 ct2) in
		    Some (Ast.CONTEXT(Ast.NoPos,change))
		| (Ast.CONTEXT(_,Ast.BEFORE(l1,c1)),
		   Ast.CONTEXT(_,Ast.AFTER(l2,c2))) ->
		     Some
		       (Ast.CONTEXT(Ast.NoPos,
				    Ast.BEFORE(l1@l2,Ast.lub_count c1 c2)))
		| (Ast.CONTEXT(_,Ast.BEFORE(_)),Ast.CONTEXT(_,Ast.NOTHING))
		| (Ast.CONTEXT(_,Ast.NOTHING),Ast.CONTEXT(_,Ast.NOTHING)) ->
		    Some retmc
		| (Ast.CONTEXT(_,Ast.NOTHING),
		   Ast.CONTEXT(_,Ast.AFTER(l,c))) ->
		    Some (Ast.CONTEXT(Ast.NoPos,Ast.BEFORE(l,c)))
		| _ -> None in
	      let ret = Ast.make_mcode "return" in
	      let edots =
		Ast.rewrap ast (Ast.Edots(Ast.make_mcode "...",None)) in
	      let semi = Ast.make_mcode ";" in
	      let simple_return =
		make_match(Ast.rewrap ast (Ast.Return(ret,semi))) in
	      let return_expr =
		make_match(Ast.rewrap ast (Ast.ReturnExpr(ret,edots,semi))) in
	      (match new_mc with
		Some new_mc ->
		  let exit = endpred None in
		  let mod_rbrace =
		    Ast.rewrap ast (Ast.SeqEnd (("}",info,new_mc,pos@spos))) in
		  let stripped_rbrace =
		    Ast.rewrap ast (Ast.SeqEnd(Ast.make_mcode "}")) in
		  ctl_or normal_res
		    (ctl_and (term mod_rbrace)
		       (ctl_and
			  (ctl_au
			     (make_match stripped_rbrace)
			     (* error exit not possible; it is in the middle
				of code, so a return is needed *)
			     exit)
			  (* worry about performance, but seems correct, not ax *)
			  (ctl_back_ag
			     (ctl_not
				(ctl_uncheck
				   (ctl_or simple_return return_expr))))))
	      |	_ ->
		  (* some change in the middle of the return, so have to
		     find an actual return *)
		  normal_res)
          | _ ->
	      (* should try to deal with the dots_bef_aft problem elsewhere,
		 but don't have the courage... *)
	      let term = term ast in
	      let term =
		if guard
		then term
		else
		  do_between_dots stmt term End
		    quantified minus_quantified label llabel slabel guard in
	      dots_done := true;
	      make_seq_after term after)
  | Ast.Seq(lbrace,body,rbrace) ->
      let (lbfvs,b1fvs,b2fvs,rbfvs) =
	match
	  seq_fvs quantified
	    [Ast.get_fvs lbrace;Ast.get_fvs body;Ast.get_fvs rbrace]
	with
	  [(lbfvs,b1fvs);(_,b2fvs);(rbfvs,_)] -> (lbfvs,b1fvs,b2fvs,rbfvs)
	| _ -> failwith "asttoctl2: not possible 19" in
      let (mlbfvs,mb1fvs,mb2fvs,mrbfvs) =
	match
	  seq_fvs minus_quantified
	    [Ast.get_mfvs lbrace;Ast.get_mfvs body;Ast.get_mfvs rbrace]
	with
	  [(lbfvs,b1fvs);(_,b2fvs);(rbfvs,_)] ->
	    (lbfvs,b1fvs,b2fvs,rbfvs)
	| _ -> failwith "asttoctl2: not possible 20" in
      let pv = count_nested_braces stmt in
      let lv = get_label_ctr() in
      let paren_pred = CTL.Pred(Lib_engine.Paren pv,CTL.Control) in
      let label_pred = CTL.Pred(Lib_engine.Label lv,CTL.Control) in
      let start_brace =
	ctl_and
	  (quantify guard lbfvs (make_match lbrace))
	  (ctl_and paren_pred label_pred) in
      let empty_rbrace =
	match Ast.unwrap rbrace with
	  Ast.SeqEnd((data,info,_,pos)) ->
	    Ast.rewrap rbrace(Ast.SeqEnd(Ast.make_mcode data))
	| _ -> failwith "unexpected close brace" in
      let end_brace =
	(* label is not needed; paren_pred is enough *)
	quantify guard rbfvs
	  (ctl_au (make_match empty_rbrace)
	     (ctl_and (real_make_match None guard rbrace) paren_pred)) in
      let new_quantified2 =
	Common.union_set b1fvs (Common.union_set b2fvs quantified) in
      let new_mquantified2 =
	Common.union_set mb1fvs (Common.union_set mb2fvs minus_quantified) in
      let pattern_as_given =
	let new_quantified2 = Common.union_set [pv] new_quantified2 in
	quantify true [pv;lv]
	  (quantify guard b1fvs
	     (make_seq
		[start_brace;
		  (ctl_or
		     (if !exists = Exists then CTL.False else (aftpred label))
		     (quantify guard b2fvs
			(statement_list body NotTop
			   (After (make_seq_after end_brace after))
			   new_quantified2 new_mquantified2
			   (Some (lv,ref true))
			   llabel slabel false guard)))])) in
      let empty_body =
	match Ast.unwrap body with
	  [body] ->
	    (match Ast.unwrap body with
	      Ast.Dots
		((_,i,Ast.CONTEXT(_,Ast.NOTHING),_),[],_,_) ->
		  (match Ast.unwrap rbrace with
		    Ast.SeqEnd((_,_,Ast.CONTEXT(_,Ast.NOTHING),_))
		    when not (contains_pos rbrace) -> true
		  | _ -> false)
	    | _ -> false)
	| _ -> false in
      if empty_body && List.mem after [Tail;End;VeryEnd]
	  (* for just a match of an if branch of the form { ... }, just
	     match the first brace *)
      then quantify guard lbfvs (make_match lbrace)
      else
      let empty_body2 =
	(* match { and } but nothing else *)
	match Ast.unwrap body with
	  [body] ->
	    (match Ast.unwrap body with
	      Ast.Dots
		((_,i,Ast.CONTEXT(_,Ast.NOTHING),_),[],_,_) -> true
	    | _ -> false)
	| _ -> false in
      if empty_body2
      then
	quantify true [pv;lv]
	  (quantify guard b1fvs
	     (make_seq_ex guard
		[start_brace;aftpred label;
		  make_seq_after end_brace after]))
      else if ends_in_return body
      then
	(* matching error handling code *)
	(* Cases:
	   1. The pattern as given
	   2. A goto, and then some close braces, and then the pattern as
	   given, but without the braces (only possible if there are no
	   decls, and open and close braces are unmodified)
	   3. Part of the pattern as given, then a goto, and then the rest
	   of the pattern.  For this case, we just check that all paths have
	   a goto within the current braces.  checking for a goto at every
	   point in the pattern seems expensive and not worthwhile. *)
	let pattern2 =
	  let body = preprocess_dots body in (* redo, to drop braces *)
	  make_seq
	    [gotopred label;
	      ctl_au
		(make_match empty_rbrace)
		(ctl_ax (* skip the destination label *)
		   (quantify guard b2fvs
		      (statement_list body NotTop End
			 new_quantified2 new_mquantified2 None llabel slabel
			 true guard)))] in
	let pattern3 =
	  let new_quantified2 = Common.union_set [pv] new_quantified2 in
	  let real_code = (* takes into account whencode (A/E) *)
	    (quantify guard b2fvs
	       (statement_list body NotTop Tail
		  new_quantified2 new_mquantified2
		  None(*no label because past the goto*)
		  llabel slabel false guard)) in
	  if contains_modif rbrace
	  then (* have to match it *)
            let switch_needed =
            (* if real code starts with ... with eg when forall, will need
		       to switch the quantifier chosen by make_seq *)
              match find_xx real_code with
		Some _ -> fun phi -> CTL.XX phi
              | None -> fun phi -> phi in
	    quantify true [pv;lv]
              (quantify guard b1fvs
		 (make_seq
                    [start_brace;
                      switch_needed
			(ctl_and
                           (CTL.AU (* want AF even for sgrep *)
                              (CTL.FORWARD,CTL.STRICT,
                               CTL.Pred
				 (Lib_engine.PrefixLabel(lv),CTL.Control),
                               ctl_or (aftpred None) (* jll new! *)
				 (ctl_and(*brace must be after goto*)
                                    (gotopred (Some (lv,ref true)))
                                  (* want AF even for sgrep *)
                                    (CTL.AF
				       (CTL.FORWARD,CTL.STRICT,end_brace)))))
                           real_code)]))
	  else (* no change to }, so don't bother matching it *)
	    quantify true [pv]
	      (quantify guard b1fvs
		 (make_seq
		    [ctl_and start_brace
		      (* check that we always escape with a goto *)
		      (* must use EX because we want to follow control edges *)
			(ctl_and (ctl_ex (gotoaftpred None))
			   (ctl_not
			      (ctl_ex
				 (ctl_and (aftpred None)
				    (ctl_not (gotoaftpred None))))));
		      real_code])) in
	ctl_or pattern_as_given (ctl_or pattern2 pattern3)
      else pattern_as_given
  | Ast.IfThen(ifheader,branch,aft) ->
      ifthen ifheader branch aft after quantified minus_quantified
	  label llabel slabel statement make_match guard

  | Ast.IfThenElse(ifheader,branch1,els,branch2,aft) ->
      ifthenelse ifheader branch1 els branch2 aft after quantified
	  minus_quantified label llabel slabel statement make_match guard

  | Ast.While(header,body,aft) | Ast.For(header,body,aft)
  | Ast.Iterator(header,body,aft) ->
      forwhile header body aft after quantified minus_quantified
	label statement make_match guard

  | Ast.Disj(stmt_dots_list) -> (* list shouldn't be empty *)
      (*ctl_and        seems pointless, disjuncts see label too
	(label_pred_maker label)*)
      let subformulas =
	List.map
	  (function sl ->
	    statement_list sl top after quantified minus_quantified label
	      llabel slabel true guard)
	  stmt_dots_list in
      let safe_subformulas =
	match top with
	  Top -> List.map2 protect_top_level stmt_dots_list subformulas
	| NotTop -> subformulas in
      List.fold_left ctl_seqor CTL.False safe_subformulas

  | Ast.Conj(stmt_dots_list) -> (* list shouldn't be empty *)
      (*ctl_and        seems pointless, disjuncts see label too
	(label_pred_maker label)*)
      let allfvs = List.map Ast.get_fvs stmt_dots_list in
      let sharedfvs =
	List.concat (List.map snd (seq_fvs quantified allfvs)) in
      let quantified = Common.union_set sharedfvs quantified in
      let subformulas =
	List.map
	  (function sl ->
	    statement_list sl top after quantified minus_quantified label
	      llabel slabel true guard)
	  stmt_dots_list in
      let safe_subformulas =
	match top with
	  Top -> List.map2 protect_top_level stmt_dots_list subformulas
	| NotTop -> subformulas in
      quantify guard sharedfvs
	(List.fold_left ctl_and CTL.True safe_subformulas)

  | Ast.Nest(starter,stmt_dots,ender,whencode,multi,bef,aft) ->
      (* label in recursive call is None because label check is already
	 wrapped around the corresponding code. not good enough, want to stay
	 in a specific region, dots and nests will keep going *)

      let bfvs =
	match seq_fvs quantified [Ast.get_wcfvs whencode;Ast.get_fvs stmt_dots]
	with
	  [(wcfvs,bothfvs);(bdfvs,_)] -> bothfvs
	| _ -> failwith "asttoctl2: not possible 21" in

      (* no minus version because when code doesn't contain any minus code *)
      let new_quantified = Common.union_set bfvs quantified in

      let (dot_code,stmt_dots) =
	match Ast.get_mcodekind starter with (*ender must have the same mcode*)
	  Ast.MINUS(_,_,_,_) as d ->
            (* no need for the fresh metavar, but ... is a bit weird as a
	       variable name *)
	    (* drops minuses on pattern, because d will have the minus effect*)
	    (Some
	       (make_match (make_meta_rule_elem "4" d Ast.CstrTrue ([],[],[]))),
	     drop_minuses stmt_dots)
	| _ -> (None,stmt_dots) in

      quantify guard bfvs
	(let dots_pattern =
	  statement_list stmt_dots top (a2n after)
	    new_quantified minus_quantified
	    label(*None*) llabel slabel true guard in
	dots_and_nests multi
	  (Some dots_pattern) whencode bef aft dot_code after label
	  (process_bef_aft new_quantified minus_quantified
	     label(*None*) llabel slabel true)
	  (function x -> (* for when code *)
	    statement_list x NotTop Tail
	      new_quantified minus_quantified label(*None*)
	      llabel slabel true true)
	  (function x -> (* for when code *)
	    statement x NotTop Tail
	      new_quantified minus_quantified label(*None*)
	      llabel slabel true)
	  guard quantified
	  (function x -> Ast.set_fvs [] (Ast.rewrap stmt x)))

  | Ast.Dots((_,i,d,_),whencodes,bef,aft) ->
      let dot_code =
	match d with
	  Ast.MINUS(_,_,_,_) ->
            (* no need for the fresh metavar, but ... is a bit weird as a
	       variable name *)
	    Some(make_match (make_meta_rule_elem "5" d Ast.CstrTrue ([],[],[])))
	| _ -> None in
      dots_and_nests false None whencodes bef aft dot_code after label
	(process_bef_aft quantified minus_quantified None llabel slabel true)
	(function x ->
	  statement_list x NotTop Tail quantified minus_quantified
	    None llabel slabel true true)
	(function x ->
	  statement x NotTop Tail quantified minus_quantified
	    None llabel slabel true)
	guard quantified
	(function x -> Ast.set_fvs [] (Ast.rewrap stmt x))

  | Ast.Switch(header,lb,decls,cases,rb) ->
      let rec intersect_all = function
	  [] -> []
	| [x] -> x
	| x::xs -> intersect x (intersect_all xs) in
      let rec intersect_all2 = function (* pairwise *)
	  [] -> []
	| x::xs ->
	    let front =
	      List.filter
		(function elem -> List.exists (List.mem elem) xs)
		x in
	    Common.union_set front (intersect_all2 xs) in
      let union_all l = List.fold_left union [] l in
      (* start normal variables *)
      let header_fvs = Ast.get_fvs header in
      let lb_fvs = Ast.get_fvs lb in
      let decl_fvs = union_all (List.map Ast.get_fvs (Ast.unwrap decls)) in
      let case_fvs = List.map Ast.get_fvs cases in
      let rb_fvs = Ast.get_fvs rb in
      let (all_efvs,all_b1fvs,all_lbfvs,all_b2fvs,
	   all_casefvs,all_b3fvs,all_rbfvs) =
	List.fold_left
	  (function (all_efvs,all_b1fvs,all_lbfvs,all_b2fvs,
		     all_casefvs,all_b3fvs,all_rbfvs) ->
	    function case_fvs ->
	      match seq_fvs quantified [header_fvs;lb_fvs;case_fvs;rb_fvs] with
		[(efvs,b1fvs);(lbfvs,b2fvs);(casefvs,b3fvs);(rbfvs,_)] ->
		  (efvs::all_efvs,b1fvs::all_b1fvs,lbfvs::all_lbfvs,
		   b2fvs::all_b2fvs,casefvs::all_casefvs,b3fvs::all_b3fvs,
		   rbfvs::all_rbfvs)
	      |	_ -> failwith "asttoctl2: not possible 22")
	  ([],[],[],[],[],[],[]) (decl_fvs :: case_fvs) in
      let (all_efvs,all_b1fvs,all_lbfvs,all_b2fvs,
	   all_casefvs,all_b3fvs,all_rbfvs) =
	(List.rev all_efvs,List.rev all_b1fvs,List.rev all_lbfvs,
	 List.rev all_b2fvs,List.rev all_casefvs,List.rev all_b3fvs,
	 List.rev all_rbfvs) in
      let exponlyfvs = intersect_all all_efvs in
      let lbonlyfvs = intersect_all all_lbfvs in
(* don't do anything with right brace.  Hope there is no + code on it *)
(*      let rbonlyfvs = intersect_all all_rbfvs in*)
      let b1fvs = union_all all_b1fvs in
      let new1_quantified = union b1fvs quantified in
      let b2fvs =
	union (union_all all_b2fvs) (intersect_all2 all_casefvs) in
      let new2_quantified = union b2fvs new1_quantified in
(*      let b3fvs = union_all all_b3fvs in*)
      (* ------------------- start minus free variables *)
      let header_mfvs = Ast.get_mfvs header in
      let lb_mfvs = Ast.get_mfvs lb in
      let decl_mfvs = union_all (List.map Ast.get_mfvs (Ast.unwrap decls)) in
      let case_mfvs = List.map Ast.get_mfvs cases in
      let rb_mfvs = Ast.get_mfvs rb in
      let (all_mefvs,all_mb1fvs,all_mlbfvs,all_mb2fvs,
	   all_mcasefvs,all_mb3fvs,all_mrbfvs) =
	List.fold_left
	  (function (all_efvs,all_b1fvs,all_lbfvs,all_b2fvs,
		     all_casefvs,all_b3fvs,all_rbfvs) ->
	    function case_mfvs ->
	      match
		seq_fvs quantified
		  [header_mfvs;lb_mfvs;case_mfvs;rb_mfvs] with
		[(efvs,b1fvs);(lbfvs,b2fvs);(casefvs,b3fvs);(rbfvs,_)] ->
		  (efvs::all_efvs,b1fvs::all_b1fvs,lbfvs::all_lbfvs,
		   b2fvs::all_b2fvs,casefvs::all_casefvs,b3fvs::all_b3fvs,
		   rbfvs::all_rbfvs)
	      |	_ -> failwith "asttoctl2: not possible 23")
	  ([],[],[],[],[],[],[]) (decl_mfvs::case_mfvs) in
      let (all_mefvs,all_mb1fvs,all_mlbfvs,all_mb2fvs,
	   all_mcasefvs,all_mb3fvs,all_mrbfvs) =
	(List.rev all_mefvs,List.rev all_mb1fvs,List.rev all_mlbfvs,
	 List.rev all_mb2fvs,List.rev all_mcasefvs,List.rev all_mb3fvs,
	 List.rev all_mrbfvs) in
(* don't do anything with right brace.  Hope there is no + code on it *)
(*      let rbonlyfvs = intersect_all all_rbfvs in*)
      let mb1fvs = union_all all_mb1fvs in
      let new1_mquantified = union mb1fvs quantified in
      let mb2fvs =
	union (union_all all_mb2fvs) (intersect_all2 all_mcasefvs) in
      let new2_mquantified = union mb2fvs new1_mquantified in
(*      let b3fvs = union_all all_b3fvs in*)
      (* ------------------- end collection of free variables *)
      let switch_header = quantify guard exponlyfvs (make_match header) in
      let pv = count_nested_braces stmt in
      let paren_pred = CTL.Pred(Lib_engine.Paren pv,CTL.Control) in
      let lb = quantify guard lbonlyfvs
	  (ctl_and (make_match lb) paren_pred) in
(*      let rb = quantify guard rbonlyfvs (make_match rb) in*)
      let case_headers =
	List.map
	  (function case_line ->
	    match Ast.unwrap case_line with
	      Ast.CaseLine(header,body) ->
		let e1fvs =
		  match seq_fvs new2_quantified [Ast.get_fvs header] with
		    [(e1fvs,_)] -> e1fvs
		  | _ -> failwith "asttoctl2: not possible 24" in
		quantify guard e1fvs (real_make_match label true header)
	    | Ast.OptCase(case_line) -> failwith "not supported")
	  cases in
      let lv = get_label_ctr() in
      let used = ref false in
      let (decls_exists_code,decls_all_code) =
	(*don't really understand this*)
	if (Ast.unwrap decls) = []
	then (CTL.True,CTL.False)
	else
	let res =
	  statement_list decls NotTop Tail
	    new2_quantified new2_mquantified (Some (lv,used)) llabel None
	    false(*?*) guard in
	(res,res) in
      let no_header =
	ctl_not
	  (List.fold_left ctl_or_fl CTL.False
	     (List.map ctl_uncheck
		(decls_all_code::case_headers))) in
      let case_code =
	List.map
	  (function case_line ->
	    match Ast.unwrap case_line with
	      Ast.CaseLine(header,body) ->
		  let (e1fvs,b1fvs,s1fvs) =
		    let fvs = [Ast.get_fvs header;Ast.get_fvs body] in
		    match seq_fvs new2_quantified fvs with
		      [(e1fvs,b1fvs);(s1fvs,_)] -> (e1fvs,b1fvs,s1fvs)
		    | _ -> failwith "asttoctl2: not possible 25" in
		  let (me1fvs,mb1fvs,ms1fvs) =
		    let fvs = [Ast.get_mfvs header;Ast.get_mfvs body] in
		    match seq_fvs new2_mquantified fvs with
		      [(e1fvs,b1fvs);(s1fvs,_)] -> (e1fvs,b1fvs,s1fvs)
		    | _ -> failwith "asttoctl2: not possible 26" in
		  let case_header =
		    quantify guard e1fvs (make_match header) in
		  let new3_quantified = union b1fvs new2_quantified in
		  let new3_mquantified = union mb1fvs new2_mquantified in
		  let body =
		    statement_list body NotTop Tail
		      new3_quantified new3_mquantified (Some (lv,used)) llabel
		      (Some (lv,used)) false(*?*) guard in
		  quantify guard b1fvs (make_seq [case_header; body])
	    | Ast.OptCase(case_line) -> failwith "not supported")
	  cases in
      let default_required =
	if List.exists
	    (function case ->
	      match Ast.unwrap case with
		Ast.CaseLine(header,_) ->
		  (match Ast.unwrap header with
		    Ast.Default(_,_) -> true
		  | _ -> false)
	      |	_ -> false)
	    cases
	then function x -> x
	else function x -> ctl_or (fallpred label) x in
      let after_pred = aftpred label in
      let body after_branch =
	Common.Right
	  (ctl_or
	     (default_required
		(quantify guard b2fvs
		   (make_seq
		      [ctl_and lb
			  (List.fold_left ctl_and CTL.True
			     (List.map ctl_ex
				(decls_exists_code :: case_headers)));
			List.fold_left ctl_or_fl no_header
			  (decls_all_code :: case_code)])))
	     after_branch) in
      let aft =
	(rb_fvs,Ast.get_fresh rb,Ast.get_inherited rb,
	match Ast.unwrap rb with
	  Ast.SeqEnd(rb) -> Ast.get_mcodekind rb
	| _ -> failwith "asttoctl2: not possible 27") in
      let (switch_header,wrapper) =
	if !used
	then
	  let label_pred = CTL.Pred (Lib_engine.Label(lv),CTL.Control) in
	  (ctl_and switch_header label_pred,
	   (function body -> quantify true [lv] body))
	else (switch_header,function x -> x) in
      wrapper
	(end_control_structure b1fvs switch_header body
	   after_pred (Some(ctl_ex after_pred)) None aft after label guard)
  | Ast.FunDecl(header,lbrace,body,rbrace,(afvs,afresh,ainh,aft)) ->
      (* what to do with afvs??? *)
      let (aafvs,ahfvs,hfvs,b1fvs,lbfvs,b2fvs,b3fvs,rbfvs) =
	match
	  seq_fvs quantified
	    [afvs;Ast.get_fvs header;Ast.get_fvs lbrace;
	      Ast.get_fvs body;Ast.get_fvs rbrace]
	with
	  [(afvs,ahfvs);(hfvs,b1fvs);(lbfvs,b2fvs);(_,b3fvs);(rbfvs,_)] ->
	    (afvs,ahfvs,hfvs,b1fvs,lbfvs,b2fvs,b3fvs,rbfvs)
	| _ -> failwith "asttoctl2: not possible 28" in
      let (mhfvs,mb1fvs,mlbfvs,mb2fvs,mb3fvs,mrbfvs) =
	match
	  seq_fvs quantified
	    [Ast.get_mfvs header;Ast.get_mfvs lbrace;
	      Ast.get_mfvs body;Ast.get_mfvs rbrace]
	with
	  [(hfvs,b1fvs);(lbfvs,b2fvs);(_,b3fvs);(rbfvs,_)] ->
	    (hfvs,b1fvs,lbfvs,b2fvs,b3fvs,rbfvs)
	| _ -> failwith "asttoctl2: not possible 29" in
      let function_header = quantify guard hfvs (make_match header) in
      let start_brace = quantify guard lbfvs (make_match lbrace) in
      let stripped_rbrace =
	match Ast.unwrap rbrace with
	  Ast.SeqEnd((data,info,_,_)) ->
	    Ast.rewrap rbrace(Ast.SeqEnd (Ast.make_mcode data))
	| _ -> failwith "unexpected close brace" in
      let end_brace =
	let exit = CTL.Pred (Lib_engine.Exit,CTL.Control) in
	let errorexit = CTL.Pred (Lib_engine.ErrorExit,CTL.Control) in
	let fake_brace = CTL.Pred (Lib_engine.FakeBrace,CTL.Control) in
	ctl_and
	  (quantify guard rbfvs (make_match rbrace))
	  (ctl_and
	     (* the following finds the beginning of the fake braces,
		if there are any, not completely sure how this works.
		see the examples sw and return *)
	     (ctl_back_ex (ctl_not fake_brace))
	     (ctl_au (make_match stripped_rbrace)
		(ctl_or exit errorexit))) in
      let new_quantified3 =
	Common.union_set b1fvs
	  (Common.union_set b2fvs (Common.union_set b3fvs quantified)) in
      let new_mquantified3 =
	Common.union_set mb1fvs
	  (Common.union_set mb2fvs
	     (Common.union_set mb3fvs minus_quantified)) in
      let not_minus = function Ast.MINUS(_,_,_,_) -> false | _ -> true in
      let optim1 =
	match (Ast.unwrap body,
	       contains_modif rbrace || contains_pos rbrace) with
	  ([body],false) ->
	    (match Ast.unwrap body with
	      Ast.Nest(starter,stmt_dots,ender,[],false,_,_)
		(* perhaps could optimize for minus case too... TODO *)
		when not_minus (Ast.get_mcodekind starter)
	      ->
            (* special case for function header + body - header is unambiguous
	       and unique, so we can just look for the nested body anywhere
	       else in the CFG *)
		let pattern =
		  statement_list stmt_dots NotTop
		    (* discards match on right brace, but don't need it *)
		    (Guard (make_seq_after end_brace after))
		    new_quantified3 new_mquantified3
		    None llabel slabel true guard in
		(* The following is based on what is generated for nest code in
		   dots_and_nests.  We skip is_plus, because that is not the case
		   here.  Also no whencode. *)
		let v = get_let_ctr() in
		let complete_pattern =
		  CTL.Let(v,pattern,
			  CTL.Or(CTL.Ref v,CTL.Not(ctl_uncheck (CTL.Ref v)))) in
		Some
		  (CTL.AndAny
		     (CTL.FORWARD,guard_to_strict guard,start_brace,
		      complete_pattern))
	    | Ast.Dots((_,i,d,_),whencode,_,_) when
		(List.for_all
		   (* flow sensitive, so not optimizable *)
		   (function Ast.WhenNotTrue(_) | Ast.WhenNotFalse(_) ->
		      false
		 | _ -> true) whencode) ->
     	    (* try to be more efficient for the case where the body is just
	       ...  Perhaps this is too much of a special case, but useful
	       for dropping a parameter and checking that it is never used. *)
		     (match d with
		       Ast.MINUS(_,_,_,_) -> None
		     | _ ->
			 let quantifier = get_quantifier whencode in
			 let op =
			   if quantifier = !exists
			   then ctl_au
			   else ctl_anti_au CTL.NONSTRICT in
			 let pv =
			   (* no nested braces, because only dots *)
			   string2var ("p1") in
			 let paren_pred =
			   CTL.Pred(Lib_engine.Paren pv,CTL.Control) in
			 Some (
			 make_seq
			   [ctl_and start_brace paren_pred;
			     match whencode with
			       [] -> CTL.True
			     | _ ->
				 let leftarg =
				   ctl_and
				     (ctl_not
					(List.fold_left
					   (function prev ->
					     function
						 Ast.WhenAlways(s) -> prev
					       | Ast.WhenNot(sl) ->
						   let x =
						     statement_list sl
						       NotTop Tail
						       new_quantified3
						       new_mquantified3
						       label llabel slabel
						       true true in
						   ctl_or prev x
					       | Ast.WhenNotTrue(_)
					       | Ast.WhenNotFalse(_) ->
						   failwith "unexpected"
					       | Ast.WhenModifier
						   (Ast.WhenAny) -> CTL.False
					       | Ast.WhenModifier(_) -> prev)
					   CTL.False whencode))
				     (List.fold_left
					(function prev ->
					  function
					      Ast.WhenAlways(s) ->
						let x =
						  statement s NotTop Tail
						    new_quantified3
						    new_mquantified3
						    label llabel slabel true in
						ctl_and prev x
					    | Ast.WhenNot(sl) -> prev
					    | Ast.WhenNotTrue(_)
					    | Ast.WhenNotFalse(_) ->
						failwith "unexpected"
					    | Ast.WhenModifier(Ast.WhenAny) ->
						CTL.True
					    | Ast.WhenModifier(_) -> prev)
					CTL.True whencode) in
				 let leftarg =
				   if quantifier = Exists
				   then
				     ctl_and (ctl_not (aftpred None)) leftarg
				   else leftarg in
				 op leftarg
				   (ctl_and
				      (make_match stripped_rbrace)
				      paren_pred)]))
	    | _ -> None)
	| _ -> None in
      let optim2 =
	(* function body is all minus, no whencode *)
	match Ast.unwrap body with
	  [body] ->
	    (match Ast.unwrap body with
	      Ast.Dots
		((_,i,(Ast.MINUS(_,_,_,Ast.NOREPLACEMENT) as d),_),[],_,_) ->
		  (match (Ast.unwrap lbrace,Ast.unwrap rbrace) with
		    (Ast.SeqStart((_,_,Ast.MINUS(_,_,_,Ast.NOREPLACEMENT),_)),
		     Ast.SeqEnd((_,_,Ast.MINUS(_,_,_,Ast.NOREPLACEMENT),_)))
		    when not (contains_pos rbrace) ->
			(* andany drops everything to the end, including close
			   braces - not just function body, could check
			   label to keep braces *)
		      let pattern = (* see optim1 *)
			make_match
			  (make_meta_rule_elem "6" d Ast.CstrTrue
			     ([],[],[])) in
		      let v = get_let_ctr() in
		      let complete_pattern =
			CTL.Let(v,pattern,
				CTL.Or(CTL.Ref v,CTL.Not(ctl_uncheck (CTL.Ref v)))) in
		      Some
			(ctl_and start_brace
			   (ctl_ax
			      (CTL.AndAny
				 (CTL.FORWARD,guard_to_strict guard,CTL.True,
				  complete_pattern))))
		  | _ -> None)
	    | _ -> None)
	| _ -> None in
      let body_code =
	match (optim1,optim2) with
	  (Some o1,_) -> o1
	| (_,Some o2) -> o2
	| _ ->
	    make_seq
	      [start_brace;
		quantify guard b3fvs
		  (statement_list body NotTop
		     (After (make_seq_after end_brace after))
		     new_quantified3 new_mquantified3 None llabel slabel
		     false guard)] in
      let function_header =
	match aft with
	  Ast.CONTEXT(_,Ast.NOTHING) -> function_header
	| _ ->
	    let match_ender =
	      quantify guard aafvs (* vars needed only for ender *)
		(ctl_and (endpred label)
		   (ctl_back_ex
		      (make_match
			 (make_meta_rule_elem "7" aft Ast.CstrTrue
			    (afvs,afresh,ainh))))) in
	    CTL.AndAny(CTL.FORWARD,CTL.NONSTRICT,function_header,
		       ctl_or (ctl_not (endpred label)) match_ender) in
      quantify guard ahfvs
	(quantify guard b1fvs
	   (make_seq [function_header; quantify guard b2fvs body_code]))
  | Ast.Define(header,body) ->
      let (hfvs,bfvs,bodyfvs) =
	match seq_fvs quantified [Ast.get_fvs header;Ast.get_fvs body]
	with
	  [(hfvs,b1fvs);(bodyfvs,_)] -> (hfvs,b1fvs,bodyfvs)
	| _ -> failwith "asttoctl2: not possible 30" in
      let (mhfvs,mbfvs,mbodyfvs) =
	match seq_fvs minus_quantified [Ast.get_mfvs header;Ast.get_mfvs body]
	with
	  [(hfvs,b1fvs);(bodyfvs,_)] -> (hfvs,b1fvs,bodyfvs)
	| _ -> failwith "asttoctl2: not possible 31" in
      let define_header = quantify guard hfvs (make_match header) in
      let body_code =
	statement_list body NotTop after
	  (Common.union_set bfvs quantified)
	  (Common.union_set mbfvs minus_quantified)
	  None llabel slabel true guard in
      quantify guard bfvs (make_seq [define_header; body_code])
  | Ast.AsStmt(stmt,asstmt) ->
      ctl_and
	(statement stmt top after quantified minus_quantified
	   label llabel slabel guard)
	(statement asstmt top after quantified minus_quantified
	   label llabel slabel guard)
  | Ast.OptStm(stm) ->
      failwith "OptStm should have been compiled away\n"
  | _ -> failwith "not supported" in
  if guard || !dots_done
  then term
  else
    do_between_dots stmt term after quantified minus_quantified
      label llabel slabel guard

(* term is the translation of stmt *)
and do_between_dots stmt term after quantified minus_quantified
    label llabel slabel guard =
    match Ast.get_dots_bef_aft stmt with
      Ast.AddingBetweenDots (brace_term,n)
    | Ast.DroppingBetweenDots (brace_term,n) ->
	let match_brace =
	  statement brace_term NotTop after quantified minus_quantified
	    label llabel slabel guard in
	let v = Printf.sprintf "_r_%d" n in
	let case1 = ctl_and CTL.NONSTRICT (CTL.Ref v) match_brace in
	let case2 = ctl_and CTL.NONSTRICT (ctl_not (CTL.Ref v)) term in
	CTL.Let
	  (v,ctl_or
	     (ctl_back_ex (ctl_or (truepred label) (inlooppred label)))
	     (ctl_back_ex (ctl_back_ex (falsepred label))),
	   ctl_or case1 case2)
    | Ast.NoDots -> term

(* un_process_bef_aft is because we don't want to do transformation in this
  code, and thus don't case about braces before or after it *)
and process_bef_aft quantified minus_quantified label llabel slabel guard =
  function
    Ast.WParen (re,n) ->
      let paren_pred = CTL.Pred (Lib_engine.Paren n,CTL.Control) in
      let s = guard_to_strict guard in
      quantify true (get_unquantified quantified [n])
	(ctl_and s (make_raw_match None guard re) paren_pred)
  | Ast.Other s ->
      statement s NotTop Tail quantified minus_quantified
	label llabel slabel guard
  | Ast.Other_dots d ->
      statement_list d NotTop Tail quantified minus_quantified
	label llabel slabel true guard

and protect_top_level stmt_dots formula =
  let starts_with_dots =
    match Ast.unwrap stmt_dots with
      d::ds ->
	(match Ast.unwrap d with
	  Ast.Dots(_,_,_,_) -> true
	| _ -> false)
    | _ -> false in
  let starts_with_non_context_brace =
    (* None = No danger
       Some false = OK except on function braces
       Some true = Never OK *)
    match Ast.unwrap stmt_dots with
      d::ds ->
	(match Ast.unwrap d with
	  Ast.Seq(before,body,after) ->
	    let rec beforemc before =
	      match Ast.unwrap before with
		Ast.SeqStart(obr) -> Ast.get_mcodekind obr
	      | Ast.AsRe(before,_) -> beforemc before
	      | _ -> failwith "bad seq" in
	    let rec aftermc after =
	      match Ast.unwrap after with
		Ast.SeqEnd(cbr) -> Ast.get_mcodekind cbr
	      | Ast.AsRe(after,_) -> aftermc after
	      | _ -> failwith "bad seq"in
	    (match (beforemc before,aftermc after) with
              (* safe cases *)
	      (Ast.CONTEXT(_,(Ast.NOTHING|Ast.AFTER _)),
	       Ast.CONTEXT(_,(Ast.NOTHING|Ast.BEFORE _))) -> None
	    | (Ast.MINUS(_,_,_,Ast.NOREPLACEMENT),
	       Ast.MINUS(_,_,_,Ast.NOREPLACEMENT))
	      when List.length (Ast.unwrap body) = 1 -> Some false (*ok on if*)
	      (* unsafe, can't be allowed to match fn top *)
	    | _ -> Some true)
	| _ -> None)
    | _ -> None in
  if starts_with_dots
  then (* EX because there is a loop on enter/top *)
    ctl_and CTL.NONSTRICT (toppred None) (ctl_ex formula)
  else
    match starts_with_non_context_brace with
      None -> formula
    | Some false ->
	ctl_and CTL.NONSTRICT
	  (ctl_not(CTL.EX(CTL.BACKWARD,funpred None)))
	  formula
    | Some true ->
	ctl_and CTL.NONSTRICT
	  (ctl_not(CTL.EX(CTL.BACKWARD,unsbrpred None)))
	  formula

and drop_minuses stmt_dots =
  let mcode (x,info,mc,pos) =
    let newmc =
      match mc with
	Ast.MINUS(pos,inst,adj,Ast.NOREPLACEMENT) ->
	  Ast.CONTEXT(pos,Ast.NOTHING) (* drops adjacency, maybe not useful *)
      | _ -> failwith "only pure minus expected in removed nest" in
    (x,info,newmc,pos) in
  let donothing r k e = k e in
  let v =
    V.rebuilder
      mcode mcode mcode mcode mcode mcode mcode mcode mcode
      mcode mcode mcode mcode mcode
      donothing donothing donothing donothing donothing donothing donothing
      donothing donothing donothing donothing donothing donothing donothing
      donothing donothing donothing donothing donothing donothing donothing
      donothing donothing donothing donothing donothing donothing donothing
      donothing donothing in
  v.V.rebuilder_statement_dots stmt_dots

and find_xx = function
    CTL.Exists(keep,v,phi) ->
      (match find_xx phi with
	Some(k,phi1) -> Some((fun phi -> CTL.Exists(keep,v,k phi)),phi1)
      | None -> None)
  | CTL.XX(phi) -> Some((fun phi -> phi),phi)
  | _ -> None

(* --------------------------------------------------------------------- *)
(* cleanup: convert AX to EX for pdots.
Concretely: AX(A[...] & E[...]) becomes AX(A[...]) & EX(E[...])
This is what we wanted in the first place, but it wasn't possible to make
because the AX and its argument are not created in the same place.
Rather clunky... *)
(* also cleanup XX, which is a marker for the case where the programmer
specifies to change the quantifier on .... Assumed to only occur after one AX
or EX, or at top level. *)

let rec cleanup c =
  let c = match c with CTL.XX(c) -> c | _ -> c in
  match c with
    CTL.False    -> CTL.False
  | CTL.True     -> CTL.True
  | CTL.Pred(p)  -> CTL.Pred(p)
  | CTL.Not(phi) -> CTL.Not(cleanup phi)
  | CTL.Exists(keep,v,phi) -> CTL.Exists(keep,v,cleanup phi)
  | CTL.AndAny(dir,s,phi1,phi2) ->
      CTL.AndAny(dir,s,cleanup phi1,cleanup phi2)
  | CTL.HackForStmt(dir,s,phi1,phi2) ->
      CTL.HackForStmt(dir,s,cleanup phi1,cleanup phi2)
  | CTL.And(s,phi1,phi2)   -> CTL.And(s,cleanup phi1,cleanup phi2)
  | CTL.Or(phi1,phi2)      -> CTL.Or(cleanup phi1,cleanup phi2)
  | CTL.SeqOr(phi1,phi2)   -> CTL.SeqOr(cleanup phi1,cleanup phi2)
  | CTL.Implies(phi1,phi2) -> CTL.Implies(cleanup phi1,cleanup phi2)
  | CTL.AF(dir,s,phi1) -> CTL.AF(dir,s,cleanup phi1)
  | CTL.AX(CTL.FORWARD,s,
	   CTL.Let(v1,e1,
		   CTL.And(CTL.NONSTRICT,CTL.AU(CTL.FORWARD,s2,e2,e3),
			   CTL.EU(CTL.FORWARD,e4,e5)))) ->
    CTL.Let(v1,e1,
	    CTL.And(CTL.NONSTRICT,
		    CTL.AX(CTL.FORWARD,s,CTL.AU(CTL.FORWARD,s2,e2,e3)),
		    CTL.EX(CTL.FORWARD,CTL.EU(CTL.FORWARD,e4,e5))))

  | CTL.AX(dir,s,phi1) ->
      (match find_xx phi1 with
	Some _ -> CTL.EX(dir,cleanup phi1)
      | None -> CTL.AX(dir,s,cleanup phi1))
  | CTL.EX(dir,phi) ->
      (match find_xx phi with
	Some (k,phi) ->
	  let phi = cleanup phi in
	  (match phi with
	    CTL.AU(_,s,_,_) -> CTL.AX(dir,s,k phi)
	  | CTL.AF(_,s,_) -> CTL.AX(dir,s,k phi)
	  | CTL.And(s,_,_) -> CTL.AX(dir,s,k phi) (* branches must be AU/AF *)
	  | _ -> failwith "asttoctl2: not possible 32")
      | None -> CTL.EX(dir,cleanup phi))
  | CTL.XX(phi)               -> failwith "bad XX"
  | CTL.AG(dir,s,phi1) -> CTL.AG(dir,s,cleanup phi1)
  | CTL.EF(dir,phi1)   -> CTL.EF(dir,cleanup phi1)
  | CTL.EG(dir,phi1)   -> CTL.EG(dir,cleanup phi1)
  | CTL.AW(dir,s,phi1,phi2) -> CTL.AW(dir,s,cleanup phi1,cleanup phi2)
  | CTL.AU(dir,s,phi1,phi2) -> CTL.AU(dir,s,cleanup phi1,cleanup phi2)
  | CTL.EU(dir,phi1,phi2)   -> CTL.EU(dir,cleanup phi1,cleanup phi2)
  | CTL.Let (x,phi1,phi2)   -> CTL.Let (x,cleanup phi1,cleanup phi2)
  | CTL.LetR (dir,x,phi1,phi2) -> CTL.LetR (dir,x,cleanup phi1,cleanup phi2)
  | CTL.Ref(s) -> CTL.Ref(s)
  | CTL.Uncheck(phi1)  -> CTL.Uncheck(cleanup phi1)
  | CTL.InnerAnd(phi1) -> CTL.InnerAnd(cleanup phi1)

(* --------------------------------------------------------------------- *)
(* Function declaration *)

(* ua = used_after, fua = fresh_used_after, fuas = fresh_used_after_seeds *)

let top_level name ((ua,pos),fua) (fuas,t) =
  this_rule := name;
  let ua = List.filter (function (nm,_) -> nm = name) ua in
  used_after := ua;
  saved := Ast.get_saved t;
  let quantified = Common.minus_set (Common.union_set ua fuas) pos in
  let (wrap,formula) =
    match Ast.unwrap t with
      Ast.FILEINFO(old_file,new_file) -> failwith "not supported fileinfo"
    | Ast.NONDECL(stmt) ->
	let unopt = elim_opt.V.rebuilder_statement stmt in
	let unopt = preprocess_dots_e unopt in
	let formula =
	  cleanup
	    (statement unopt Top VeryEnd quantified [] None None None false) in
	((function x -> NONDECL x), formula)
    | Ast.CODE(stmt_dots) ->
	let unopt = elim_opt.V.rebuilder_statement_dots stmt_dots in
	let unopt = preprocess_dots unopt in
	let formula =
	  statement_list unopt Top VeryEnd quantified [] None None None
	    false false in
	let clean_formula = protect_top_level stmt_dots (cleanup formula) in
	((function x -> CODE x), clean_formula)
    | Ast.ERRORWORDS(exps) -> failwith "not supported errorwords" in
  wrap (quantify false quantified formula)

(* --------------------------------------------------------------------- *)
(* Entry points *)

let asttoctlz (name,(_,_,exists_flag),l)
    (used_after,fresh_used_after,fresh_used_after_seeds) positions =
  letctr := 0;
  labelctr := 0;
  (match exists_flag with
    Ast.Exists -> exists := Exists
  | Ast.Forall -> exists := Forall
  | Ast.Undetermined ->
      exists := if !Flag.sgrep_mode2 then Exists else Forall);

  let (l,used_after) =
    List.split
      (List.filter
	 (function (t,_) ->
	   match Ast.unwrap t with Ast.ERRORWORDS(exps) -> false | _ -> true)
	 (List.combine l (List.combine used_after positions))) in
  let res =
    List.map2 (top_level name)
      (List.combine used_after fresh_used_after)
      (List.combine fresh_used_after_seeds l) in
  exists := Forall;
  res

let asttoctl r used_after positions =
  match r with
    Ast.ScriptRule _ | Ast.InitialScriptRule _ | Ast.FinalScriptRule _ -> []
  | Ast.CocciRule (rule_name,b,c,_,Ast_cocci.Normal) ->
      asttoctlz (rule_name,b,c) used_after positions
  | Ast.CocciRule (a,b,c,_,Ast_cocci.Generated) -> [CODE CTL.True]

let pp_cocci_predicate (pred,modif) =
  Pretty_print_engine.pp_predicate pred

let cocci_predicate_to_string (pred,modif) =
  Pretty_print_engine.predicate_to_string pred
