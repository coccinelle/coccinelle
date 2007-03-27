(* for MINUS and CONTEXT, pos is always None in this file *)
(*search for require*)
(* true = don't see all matched nodes, only modified ones *)
let onlyModif = ref true(*false*)

module Ast = Ast_cocci
module V = Visitor_ast
module CTL = Ast_ctl

let warning s = Printf.fprintf stderr "warning: %s\n" s

type cocci_predicate = Lib_engine.predicate * Ast.meta_name Ast_ctl.modif
type formula =
    (cocci_predicate,Ast.meta_name, Wrapper_ctl.info) Ast_ctl.generic_ctl

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

let wrap n ctl = (ctl,n)

let label_pred_maker line = function
    None -> None
  | Some label_var ->
      let label_pred = (Lib_engine.PrefixLabel(label_var),CTL.Control) in
      Some(wrap line (CTL.Pred label_pred))

let predmaker guard pred line = function
    None -> wrap line (CTL.Pred pred)
  | Some label_var ->
      let label_pred = (Lib_engine.PrefixLabel(label_var),CTL.Control) in
      wrap line
	(CTL.And(guard_to_strict guard,
		 wrap line (CTL.Pred pred),wrap line (CTL.Pred label_pred)))

let aftpred     = predmaker false (Lib_engine.After,       CTL.Control)
let retpred     = predmaker false (Lib_engine.Return,      CTL.Control)
let exitpred    = predmaker false (Lib_engine.ErrorExit,   CTL.Control)
let endpred     = predmaker false (Lib_engine.Exit,        CTL.Control)
let truepred    = predmaker false (Lib_engine.TrueBranch,  CTL.Control)
let falsepred   = predmaker false (Lib_engine.FalseBranch, CTL.Control)
let fallpred    = predmaker false (Lib_engine.FallThrough, CTL.Control)

let aftret line label_var f =
  wrap line
    (CTL.Or
       (aftpred line label_var, exitpred line label_var(*f(retpred line)*)))

let letctr = ref 0
let get_let_ctr _ =
  let cur = !letctr in
  letctr := cur + 1;
  Printf.sprintf "r%d" cur

(* --------------------------------------------------------------------- *)

let wrapImplies n (x,y) = wrap n (CTL.Implies(x,y))
let wrapExists  n keep (x,y) = wrap n (CTL.Exists(x,y,keep))
let wrapAnd     n s (x,y) = wrap n (CTL.And(s,x,y))
let wrapOr      n (x,y) = wrap n (CTL.Or(x,y))
let wrapSeqOr   n (x,y) = wrap n (CTL.SeqOr(x,y))
let wrapAU      n s (x,y) =
  if !Flag_parsing_cocci.sgrep_mode
  then wrap n (CTL.EU(CTL.FORWARD,x,y))
  else wrap n (CTL.AU(CTL.FORWARD,s,x,y))
(* only used for goto, where we want AU even for sgrep *)
let wrapAF      n s (x,y) = wrap n (CTL.AF(CTL.FORWARD,s,x))
let wrapAX      n s x   =
  if !Flag_parsing_cocci.sgrep_mode
  then wrap n (CTL.EX(CTL.FORWARD,x))
  else wrap n (CTL.AX(CTL.FORWARD,s,x))
let wrapAX_absolute      n s (x)   = wrap n (CTL.AX(CTL.FORWARD,s,x))
(* This stays being AX even for sgrep_mode, because it is used to identify
the structure of the term, not matching the pattern. *)
let wrapBackAX  n (x)   = wrap n (CTL.AX(CTL.BACKWARD,CTL.NONSTRICT,x))
let wrapEX      n (x)   = wrap n (CTL.EX(CTL.FORWARD,x))
let wrapBackEX  n (x)   = wrap n (CTL.EX(CTL.BACKWARD,x))
let wrapAG      n (s,x) = wrap n (CTL.AG(CTL.FORWARD,s,x))
let wrapEG      n (x)   = wrap n (CTL.EG(CTL.FORWARD,x))
let wrapEF      n (x)   = wrap n (CTL.EF(CTL.FORWARD,x))
let wrapNot     n (x)   = wrap n (CTL.Not(x))
let wrapPred    n (x)   = wrap n (CTL.Pred(x))
let wrapDots    n s (x,y,z,a,b,c,d,e,f,g) =
  wrap n (CTL.Dots(CTL.FORWARD,s,x,y,z,a,b,c,d,e,f,g))
let wrapPDots   n s (x,y,z,a,b,c,d,e,f,g) =
  wrap n (CTL.PDots(CTL.FORWARD,s,x,y,z,a,b,c,d,e,f,g))
let wrapLet     n (x,y,z) = wrap n (CTL.Let(x,y,z))
let wrapRef     n (x)   = wrap n (CTL.Ref(x))

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

  let freshlist l =
    List.fold_left Common.union_set [] (List.map Ast.get_fresh l) in

  let inheritedlist l =
    List.fold_left Common.union_set [] (List.map Ast.get_inherited l) in

  let savedlist l =
    List.fold_left Common.union_set [] (List.map Ast.get_saved l) in

  let varlists l =
    (fvlist l, freshlist l, inheritedlist l, savedlist l) in

  let rec dots_list unwrapped wrapped =
    match (unwrapped,wrapped) with
      ([],_) -> []

    | (Ast.Dots(_,_,_)::Ast.OptStm(stm)::(Ast.Dots(_,_,_) as u)::urest,
       d0::_::d1::rest)
    | (Ast.Nest(_,_,_)::Ast.OptStm(stm)::(Ast.Dots(_,_,_) as u)::urest,
       d0::_::d1::rest) ->
	 let l = Ast.get_line stm in
	 let new_rest1 = stm :: (dots_list (u::urest) (d1::rest)) in
	 let new_rest2 = dots_list urest rest in
	 let (fv_rest1,fresh_rest1,inherited_rest1,s1) = varlists new_rest1 in
	 let (fv_rest2,fresh_rest2,inherited_rest2,s2) = varlists new_rest2 in
	 [d0;
	   (Ast.Disj
	      [(Ast.DOTS(new_rest1),l,fv_rest1,fresh_rest1,inherited_rest1,s1,
		Ast.NoDots);
		(Ast.DOTS(new_rest2),l,fv_rest2,fresh_rest2,inherited_rest2,s2,
		 Ast.NoDots)],
	      l,fv_rest1,fresh_rest1,inherited_rest1,s1,Ast.NoDots)]

    | (Ast.OptStm(stm)::urest,_::rest) ->
	 let l = Ast.get_line stm in
	 let new_rest1 = dots_list urest rest in
	 let new_rest2 = stm::new_rest1 in
	 let (fv_rest1,fresh_rest1,inherited_rest1,s1) = varlists new_rest1 in
	 let (fv_rest2,fresh_rest2,inherited_rest2,s2) = varlists new_rest2 in
	 [(Ast.Disj
	     [(Ast.DOTS(new_rest2),l,fv_rest2,fresh_rest2,inherited_rest2,s2,
	       Ast.NoDots);
	       (Ast.DOTS(new_rest1),l,fv_rest1,fresh_rest1,inherited_rest1,s1,
		Ast.NoDots)],
	   l,fv_rest2,fresh_rest2,inherited_rest2,s2,Ast.NoDots)]

    | ([Ast.Dots(_,_,_);Ast.OptStm(stm)],[d1;_]) ->
	let l = Ast.get_line stm in
	let fv_stm = Ast.get_fvs stm in
	let fresh_stm = Ast.get_fresh stm in
	let inh_stm = Ast.get_inherited stm in
	let saved_stm = Ast.get_saved stm in
	let fv_d1 = Ast.get_fvs d1 in
	let fresh_d1 = Ast.get_fresh d1 in
	let inh_d1 = Ast.get_inherited d1 in
	let saved_d1 = Ast.get_saved d1 in
	let fv_both = Common.union_set fv_stm fv_d1 in
	let fresh_both = Common.union_set fresh_stm fresh_d1 in
	let inh_both = Common.union_set inh_stm inh_d1 in
	let saved_both = Common.union_set saved_stm saved_d1 in
	[d1;(Ast.Disj[(Ast.DOTS([stm]),l,fv_stm,fresh_stm,inh_stm,saved_stm,
		       Ast.NoDots);
		       (Ast.DOTS([d1]),l,fv_d1,fresh_d1,inh_d1,saved_d1,
			Ast.NoDots)],
	     l,fv_both,fresh_both,inh_both,saved_both,Ast.NoDots)]

    | ([Ast.Nest(_,_,_);Ast.OptStm(stm)],[d1;_]) ->
	let l = Ast.get_line stm in
	let rw = Ast.rewrap stm in
	let rwd = Ast.rewrap stm in
	let dots =
	  Ast.Dots(("...",{ Ast.line = 0; Ast.column = 0 },
		    Ast.CONTEXT(Ast.NoPos,Ast.NOTHING)),
		   Ast.NoWhen,[]) in
	[d1;rw(Ast.Disj[rwd(Ast.DOTS([stm]));
			 (Ast.DOTS([rw dots]),l,[],[],[],[],Ast.NoDots)])]

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
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    donothing donothing stmtdotsfn donothing
    donothing donothing donothing donothing donothing donothing donothing
    donothing donothing donothing donothing donothing

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

type after = After of formula | Guard of formula | Tail | End

let a2n = function After x -> Guard x | a -> a

(* --------------------------------------------------------------------- *)
(* Top-level code *)

let fresh_var _ = string2var "_v"

let fresh_metavar _ = "_S"

(* fvinfo is going to end up being from the whole associated statement.
   it would be better if it were just the free variables in d, but free_vars.ml
   doesn't keep track of free variables on + code *)
let make_meta_rule_elem d fvinfo =
  let nm = fresh_metavar() in
  Ast.make_meta_rule_elem nm d fvinfo

let get_unquantified quantified vars =
  List.filter (function x -> not (List.mem x quantified)) vars

let make_seq n guard l =
  let s = guard_to_strict guard in
  foldr1 (function rest -> function cur -> wrapAnd n s (cur,wrapAX n s rest))
    l

let make_seq_after2 n guard first rest =
  let s = guard_to_strict guard in
  match rest with
    After rest -> wrapAnd n s (first,wrapAX n s (wrapAX n s rest))
  | _ -> first

let make_seq_after n guard first rest =
  match rest with
    After rest -> make_seq n guard [first;rest]
  | _ -> first

let opt_and n guard first rest =
  let s = guard_to_strict guard in
  match first with
    None -> rest
  | Some first -> wrapAnd n s (first,rest)

let and_after n guard first rest =
  let s = guard_to_strict guard in
  match rest with After rest -> wrapAnd n s (first,rest) | _ -> first

let contains_modif =
  let bind x y = x or y in
  let option_default = false in
  let mcode r (_,_,kind) =
    match kind with
      Ast.MINUS(_,_) -> true
    | Ast.PLUS -> failwith "not possible"
    | Ast.CONTEXT(_,info) -> not (info = Ast.NOTHING) in
  let do_nothing r k e = k e in
  let rule_elem r k re =
    let res = k re in
    match Ast.unwrap re with
      Ast.FunHeader(bef,_,stg,ty,name,lp,params,rp) ->
	bind (mcode r ((),(),bef)) res
    | Ast.Decl(bef,_,decl) -> bind (mcode r ((),(),bef)) res
    | _ -> res in
  let recursor =
    V.combiner bind option_default
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      do_nothing do_nothing do_nothing do_nothing
      do_nothing do_nothing do_nothing do_nothing do_nothing do_nothing
      do_nothing rule_elem do_nothing do_nothing do_nothing do_nothing in
  recursor.V.combiner_rule_elem

let make_match n label guard code =
  let v = fresh_var() in
  if contains_modif code && not guard
  then
    wrapExists n true
      (v,predmaker guard (Lib_engine.Match(code),CTL.Modif v) n label)
  else
    match (!onlyModif,guard,intersect !used_after (Ast.get_fvs code)) with
      (true,_,[]) | (_,true,_) ->
	predmaker guard (Lib_engine.Match(code),CTL.Control) n label
    | _ ->
	wrapExists n true
	  (v,predmaker guard (Lib_engine.Match(code),CTL.UnModif v) n label)

let make_raw_match n label guard code =
  predmaker guard (Lib_engine.Match(code),CTL.Control) n label

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

let quantify n =
  List.fold_right
    (function cur ->
      function code -> wrapExists n (List.mem cur !saved) (cur,code))

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
      Ast.Seq(_,_,_,_,_) | Ast.FunDecl(_,_,_,_,_,_) -> (k s) + 1
    | _ -> k s in
  let donothing r k e = k e in
  let mcode r x = 0 in
  let recursor = V.combiner bind option_default
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      donothing donothing donothing donothing
      donothing donothing donothing donothing donothing donothing
      donothing donothing stmt_count donothing donothing donothing in
  let res = string_of_int (recursor.V.combiner_statement s) in
  string2var ("p"^res)

let labelctr = ref 0
let get_label_ctr _ =
  let cur = !labelctr in
  labelctr := cur + 1;
  string2var (Printf.sprintf "l%d" cur)

(* --------------------------------------------------------------------- *)
(* annotate dots with before and after neighbors *)

let rec get_before sl a =
  match Ast.unwrap sl with
    Ast.DOTS(x) ->
      let rec loop sl a =
	match sl with
	  [] -> ([],a)
	| e::sl ->
	    let (e,ea) = get_before_e e a in
	    let (sl,sla) = loop sl ea in
	    (e::sl,sla) in
      let (l,a) = loop x a in
      (Ast.rewrap sl (Ast.DOTS(l)),a)
  | Ast.CIRCLES(x) -> failwith "not supported"
  | Ast.STARS(x) -> failwith "not supported"

and get_before_whencode = function
    Ast.NoWhen -> Ast.NoWhen
  | Ast.WhenNot w -> let (w,_) = get_before w [] in Ast.WhenNot w
  | Ast.WhenAlways w -> let (w,_) = get_before_e w [] in Ast.WhenAlways w

and get_before_e s a =
  match Ast.unwrap s with
    Ast.Dots(d,w,t) -> (Ast.rewrap s (Ast.Dots(d,get_before_whencode w,a@t)),a)
  | Ast.Nest(stmt_dots,w,t) ->
      let w = get_before_whencode w in
      let (sd,_) = get_before stmt_dots a in
      let a =
	List.filter
	  (function
	      Ast.Other a ->
		let unifies =
		  Unify_ast.unify_statement_dots
		    (Ast.rewrap s (Ast.DOTS([a]))) stmt_dots in
		(match unifies with
		  Unify_ast.MAYBE -> false
		| _ -> true)
	    | Ast.Other_dots a ->
		let unifies = Unify_ast.unify_statement_dots a stmt_dots in
		(match unifies with
		  Unify_ast.MAYBE -> false
		| _ -> true)
	    | _ -> true)
	  a in
      (Ast.rewrap s (Ast.Nest(sd,w,a@t)),[Ast.Other_dots stmt_dots])
  | Ast.Disj(stmt_dots_list) ->
      (* put the result of processing each branch in the whencode of the
	 subsequent branches.  acc_dsl collects these extra whencodes *)
      let (dsl,_,dsla) =
	List.fold_left
	  (function (dsl,acc_dsl,dsla) ->
	    function cur ->
	      let (cur_dsl,cur_dsla) = get_before cur (acc_dsl@a) in
	      (cur_dsl::dsl,(Ast.Other_dots cur_dsl)::acc_dsl,cur_dsla::dsla))
	  ([],[],[]) stmt_dots_list in
      let dsl = List.rev dsl in
      (Ast.rewrap s (Ast.Disj(dsl)),List.fold_left Common.union_set [] dsla)
  | Ast.Atomic(ast) ->
      (match Ast.unwrap ast with
	Ast.MetaStmt(_,_,_,_) -> (s,[])
      |	_ -> (s,[Ast.Other s]))
  | Ast.Seq(lbrace,decls,dots,body,rbrace) ->
      let index = count_nested_braces s in
      let (de,dea) = get_before decls [Ast.WParen(lbrace,index)] in
      let (bd,_) = get_before body dea in
      (Ast.rewrap s (Ast.Seq(lbrace,de,dots,bd,rbrace)),
       [Ast.WParen(rbrace,index)])
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
  | Ast.Switch(header,lb,cases,rb) ->
      let cases =
	List.map
	  (function case_line ->
	    match Ast.unwrap case_line with
	      Ast.CaseLine(header,body) ->
		let (body,_) = get_before body [] in
		Ast.rewrap case_line (Ast.CaseLine(header,body))
	    | Ast.OptCase(case_line) -> failwith "not supported")
	  cases in
      (Ast.rewrap s (Ast.Switch(header,lb,cases,rb)),[Ast.Other s])
  | Ast.FunDecl(header,lbrace,decls,dots,body,rbrace) ->
      let index = count_nested_braces s in
      let (de,dea) = get_before decls [Ast.WParen(lbrace,index)] in
      let (bd,_) = get_before body dea in
      (Ast.rewrap s (Ast.FunDecl(header,lbrace,de,dots,bd,rbrace)),[])
  | Ast.MultiStm(stm) -> (* I have no idea ... *)
      let (stm,res) = get_before_e stm a in
      (Ast.rewrap s (Ast.MultiStm(stm)),res)
  | _ -> failwith "get_before_e: not supported"

let rec get_after sl a =
  match Ast.unwrap sl with
    Ast.DOTS(x) ->
      let rec loop sl =
	match sl with
	  [] -> ([],a)
	| e::sl ->
	    let (sl,sla) = loop sl in
	    let (e,ea) = get_after_e e sla in
	    (e::sl,ea) in
      let (l,a) = loop x in
      (Ast.rewrap sl (Ast.DOTS(l)),a)
  | Ast.CIRCLES(x) -> failwith "not supported"
  | Ast.STARS(x) -> failwith "not supported"

and get_after_whencode a = function
    Ast.NoWhen -> Ast.NoWhen
  | Ast.WhenNot w -> let (w,_) = get_after w a (*?*) in Ast.WhenNot w
  | Ast.WhenAlways w -> let (w,_) = get_after_e w a in Ast.WhenAlways w

and get_after_e s a =
  match Ast.unwrap s with
    Ast.Dots(d,w,t) ->
      (Ast.rewrap s (Ast.Dots(d,get_after_whencode a w,a@t)),a)
  | Ast.Nest(stmt_dots,w,t) ->
      let w = get_after_whencode a w in
      let (sd,_) = get_after stmt_dots a in
      let a =
	List.filter
	  (function
	      Ast.Other a ->
		let unifies =
		  Unify_ast.unify_statement_dots
		    (Ast.rewrap s (Ast.DOTS([a]))) stmt_dots in
		(match unifies with
		  Unify_ast.MAYBE -> false
		| _ -> true)
	    | Ast.Other_dots a ->
		let unifies = Unify_ast.unify_statement_dots a stmt_dots in
		(match unifies with
		  Unify_ast.MAYBE -> false
		| _ -> true)
	    | _ -> true)
	  a in
      (Ast.rewrap s (Ast.Nest(sd,w,a@t)),[Ast.Other_dots stmt_dots])
  | Ast.Disj(stmt_dots_list) ->
      let (dsl,dsla) =
	List.split (List.map (function e -> get_after e a) stmt_dots_list) in
      (Ast.rewrap s (Ast.Disj(dsl)),List.fold_left Common.union_set [] dsla)
  | Ast.Atomic(ast) ->
      (match Ast.unwrap ast with
	Ast.MetaStmt(nm,keep,Ast.SequencibleAfterDots _,i) ->
	  (* check "after" information for metavar optimization *)
	  (* if the error is not desired, could just return [], then
	     the optimization (check for EF) won't take place *)
	  List.iter
	    (function
		Ast.Other x ->
		  (match Ast.unwrap x with
		    Ast.Dots(_,_,_) | Ast.Nest(_,_,_) ->
		      failwith
			"dots/nest not allowed before and after stmt metavar"
		  | _ -> ())
	      |	Ast.Other_dots x ->
		  (match Ast.undots x with
		    x::_ ->
		      (match Ast.unwrap x with
			Ast.Dots(_,_,_) | Ast.Nest(_,_,_) ->
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
		   (Ast.MetaStmt(nm,keep,Ast.SequencibleAfterDots a,i)))),[])
      |	Ast.MetaStmt(_,_,_,_) -> (s,[])
      |	_ -> (s,[Ast.Other s]))
  | Ast.Seq(lbrace,decls,dots,body,rbrace) ->
      let index = count_nested_braces s in
      let (bd,bda) = get_after body [Ast.WParen(rbrace,index)] in
      let (de,_) = get_after decls bda in
      (Ast.rewrap s (Ast.Seq(lbrace,de,dots,bd,rbrace)),
       [Ast.WParen(lbrace,index)])
  | Ast.IfThen(ifheader,branch,aft) ->
      let (br,_) = get_after_e branch a in
      (Ast.rewrap s (Ast.IfThen(ifheader,br,aft)),[Ast.Other s])
  | Ast.IfThenElse(ifheader,branch1,els,branch2,aft) ->
      let (br1,_) = get_after_e branch1 a in
      let (br2,_) = get_after_e branch2 a in
      (Ast.rewrap s (Ast.IfThenElse(ifheader,br1,els,br2,aft)),[Ast.Other s])
  | Ast.While(header,body,aft) ->
      let (bd,_) = get_after_e body a in
      (Ast.rewrap s (Ast.While(header,bd,aft)),[Ast.Other s])
  | Ast.For(header,body,aft) ->
      let (bd,_) = get_after_e body a in
      (Ast.rewrap s (Ast.For(header,bd,aft)),[Ast.Other s])
  | Ast.Switch(header,lb,cases,rb) ->
      let cases =
	List.map
	  (function case_line ->
	    match Ast.unwrap case_line with
	      Ast.CaseLine(header,body) ->
		let (body,_) = get_after body [] in
		Ast.rewrap case_line (Ast.CaseLine(header,body))
	    | Ast.OptCase(case_line) -> failwith "not supported")
	  cases in
      (Ast.rewrap s (Ast.Switch(header,lb,cases,rb)),[Ast.Other s])
  | Ast.FunDecl(header,lbrace,decls,dots,body,rbrace) ->
      let (bd,bda) = get_after body [] in
      let (de,_) = get_after decls bda in
      (Ast.rewrap s (Ast.FunDecl(header,lbrace,de,dots,bd,rbrace)),[])
  | Ast.MultiStm(stm) -> (* I have no idea ... *)
      let (stm,res) = get_after_e stm a in
      (Ast.rewrap s (Ast.MultiStm(stm)),res)
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

let rec ends_in_return stmt_list =
  match Ast.unwrap stmt_list with
    Ast.DOTS(x) ->
      (match List.rev x with
	x::_ ->
	  (match Ast.unwrap x with
	    Ast.Atomic(x) ->
	      (match Ast.unwrap x with
		Ast.Return(_,_) | Ast.ReturnExpr(_,_,_) -> true
	      | _ -> false)
	  | Ast.Disj(disjs) -> List.for_all ends_in_return disjs
	  | _ -> false)
      |	_ -> false)
  | Ast.CIRCLES(x) -> failwith "not supported"
  | Ast.STARS(x) -> failwith "not supported"

(* --------------------------------------------------------------------- *)
(* control structures *)

let end_control_structure fvs header body after_pred
    after_checks no_after_checks aft after n label guard aftfvinfo =
  (* aft indicates what is added after the whole if, which has to be added
     to the endif node *)
  let (aft_needed,after_branch) =
    match aft with
      Ast.CONTEXT(_,Ast.NOTHING) ->
	(false,make_seq_after2 n guard after_pred after)
    | _ ->
	let match_endif =
	  make_match n label guard (make_meta_rule_elem aft aftfvinfo) in
	(true,
	 make_seq_after n guard after_pred
	   (After(make_seq_after n guard match_endif after))) in
  let body = body after_branch in
  let s = guard_to_strict guard in
  (* the code *)
  quantify n fvs
    (wrapAnd n s
       (header, opt_and n guard
	  (match (after,aft_needed) with
	    (After _,_) (* pattern doesn't end here *)
	  | (_,true) (* + code added after *) -> after_checks
	  | _ -> no_after_checks)
	  (wrapAX_absolute n s body)))

let ifthen ifheader branch aft after quantified n label recurse make_match
    guard aftfvinfo =
(* "if (test) thn" becomes:
    if(test) & AX((TrueBranch & AX thn) v FallThrough v After)

    "if (test) thn; after" becomes:
    if(test) & AX((TrueBranch & AX thn) v FallThrough v (After & AXAX after))
             & EX After
*)
  (* free variables *) 
  let (efvs,bfvs) =
    List.hd(seq_fvs quantified [Ast.get_fvs ifheader;Ast.get_fvs branch]) in
  let new_quantified = Common.union_set bfvs quantified in
  (* if header *)
  let if_header = quantify n efvs (make_match ifheader) in
  (* then branch and after *)
  let true_branch =
    make_seq n guard
      [truepred n label; recurse branch Tail new_quantified label guard] in
  let after_pred = aftpred n label in
  let or_cases after_branch =
    wrapOr n (true_branch,wrapOr n (fallpred n label,after_branch)) in
  end_control_structure bfvs if_header or_cases after_pred
      (Some(wrapEX n after_pred)) None aft after n label guard aftfvinfo

let ifthenelse ifheader branch1 els branch2 aft after quantified n label
    recurse make_match guard aftfvinfo =
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
    match seq_fvs quantified [Ast.get_fvs ifheader;Ast.get_fvs branch1] with
      [(e1fvs,b1fvs);(s1fvs,_)] -> (e1fvs,b1fvs,s1fvs)
    | _ -> failwith "not possible" in
  let (e2fvs,b2fvs,s2fvs) =
    (* fvs on else? *)
    match seq_fvs quantified [Ast.get_fvs ifheader;Ast.get_fvs branch2] with
      [(e2fvs,b2fvs);(s2fvs,_)] -> (e2fvs,b2fvs,s2fvs)
    | _ -> failwith "not possible" in
  let bothfvs        = union (union b1fvs b2fvs) (intersect s1fvs s2fvs) in
  let exponlyfvs     = intersect e1fvs e2fvs in
  let new_quantified = union bothfvs quantified in
  (* if header *)
  let if_header = quantify n exponlyfvs (make_match ifheader) in
  (* then and else branches *)
  let true_branch =
    make_seq n guard
      [truepred n label; recurse branch1 Tail new_quantified label guard] in
  let false_branch =
    make_seq n guard
      [falsepred n label; make_match els;
	recurse branch2 Tail new_quantified label guard] in
  let after_pred = aftpred n label in
  let or_cases after_branch =
    wrapOr n (true_branch,wrapOr n (false_branch,after_branch)) in
  let s = guard_to_strict guard in
  end_control_structure bothfvs if_header or_cases after_pred
      (Some(wrapAnd n s (wrapEX n (falsepred n label),wrapEX n after_pred)))
      (Some(wrapEX n (falsepred n label)))
      aft after n label guard aftfvinfo

let forwhile header body aft after quantified n label recurse make_match
    guard aftfvinfo =
  (* the translation in this case is similar to that of an if with no else *)
  (* free variables *) 
  let (efvs,bfvs) =
    List.hd(seq_fvs quantified [Ast.get_fvs header;Ast.get_fvs body]) in
  let new_quantified = Common.union_set bfvs quantified in
  (* loop header *)
  let header = quantify n efvs (make_match header) in
  let body =
    make_seq n guard
      [truepred n label; recurse body Tail new_quantified label guard] in
  let after_pred = fallpred n label in
  let or_cases after_branch = wrapOr n (body,after_branch) in
  end_control_structure bfvs header or_cases after_pred
    (Some(wrapEX n after_pred)) None aft after n label guard aftfvinfo
  
(* --------------------------------------------------------------------- *)
(* statement metavariables *)

(* issue: an S metavariable that is not an if branch/loop body
   should not match an if branch/loop body, so check that the labels
   of the nodes before the first node matched by the S are different
   from the label of the first node matched by the S *)
let sequencibility body n label_pred process_bef_aft = function
    Ast.Sequencible | Ast.SequencibleAfterDots [] ->
      body
	(function x ->
	  (wrapAnd n CTL.NONSTRICT (wrapNot n (wrapBackAX n label_pred),x)))
  | Ast.SequencibleAfterDots l ->
      (* S appears after some dots.  l is the code that comes after the S.
	 want to search for that first, because S can match anything, while
	 the stuff after is probably more restricted *)
      let afts = List.map process_bef_aft l in
      let ors = foldl1 (function x -> function y -> wrapOr n (x,y)) afts in
      wrapAnd n CTL.NONSTRICT
	(wrapEF n (wrapAnd n CTL.NONSTRICT (ors,wrapBackAX n label_pred)),
	 body
	   (function x ->
	     wrapAnd n CTL.NONSTRICT (wrapNot n (wrapBackAX n label_pred),x)))
  | Ast.NotSequencible -> body (function x -> x)

let svar_context_with_add_after s n label quantified d ast
    seqible after process_bef_aft guard fvinfo =
  let label_var = (*fresh_label_var*) string2var "_lab" in
  let label_pred =
    wrapPred n (Lib_engine.Label(label_var),CTL.Control) in
  let prelabel_pred =
    wrapPred n (Lib_engine.PrefixLabel(label_var),CTL.Control) in
  let matcher d = make_match n None guard (make_meta_rule_elem d fvinfo) in
  let full_metamatch = matcher d in
  let first_metamatch =
    matcher
      (match d with
	Ast.CONTEXT(pos,Ast.BEFOREAFTER(bef,_)) ->
	  Ast.CONTEXT(pos,Ast.BEFORE(bef))
      |	Ast.CONTEXT(pos,_) -> Ast.CONTEXT(pos,Ast.NOTHING)
      | Ast.MINUS(_,_) | Ast.PLUS -> failwith "not possible") in
  let middle_metamatch =
    matcher
      (match d with
	Ast.CONTEXT(pos,_) -> Ast.CONTEXT(pos,Ast.NOTHING)
      | Ast.MINUS(_,_) | Ast.PLUS -> failwith "not possible") in
  let last_metamatch =
    matcher
      (match d with
	Ast.CONTEXT(pos,Ast.BEFOREAFTER(_,aft)) ->
	  Ast.CONTEXT(pos,Ast.AFTER(aft))
      |	Ast.CONTEXT(_,_) -> d
      | Ast.MINUS(_,_) | Ast.PLUS -> failwith "not possible") in

  let rest_nodes =
    wrapAnd n CTL.NONSTRICT (middle_metamatch,prelabel_pred) in  
  let left_or = (* the whole statement is one node *)
    make_seq n guard
      [full_metamatch; and_after n guard (wrapNot n prelabel_pred) after] in
  let right_or = (* the statement covers multiple nodes *)
    make_seq n guard
      [first_metamatch;
	wrapAU n CTL.NONSTRICT
	  (rest_nodes,
	   make_seq n guard
	     [wrapAnd n CTL.NONSTRICT (last_metamatch,label_pred);
	       and_after n guard
		 (wrapNot n prelabel_pred) after])] in
  let body f =
    wrapAnd n CTL.NONSTRICT
      (label_pred,
       f (wrapAnd n CTL.NONSTRICT
	    (make_raw_match n label false ast,wrapOr n (left_or,right_or)))) in
  quantify n (label_var::get_unquantified quantified [s])
    (sequencibility body n label_pred process_bef_aft seqible)

let svar_minus_or_no_add_after s n label quantified d ast
    seqible after process_bef_aft guard fvinfo =
  let label_var = (*fresh_label_var*) string2var "_lab" in
  let label_pred =
    wrapPred n (Lib_engine.Label(label_var),CTL.Control) in
  let prelabel_pred =
    wrapPred n (Lib_engine.PrefixLabel(label_var),CTL.Control) in
  let matcher d = make_match n None guard (make_meta_rule_elem d fvinfo) in
  let first_metamatch = matcher d in
  let rest_metamatch =
    matcher
      (match d with
	Ast.MINUS(pos,_) -> Ast.MINUS(pos,[])
      | Ast.CONTEXT(pos,_) -> Ast.CONTEXT(pos,Ast.NOTHING)
      | Ast.PLUS -> failwith "not possible") in
  let rest_nodes = wrapAnd n CTL.NONSTRICT (rest_metamatch,prelabel_pred) in
  let last_node = and_after n guard (wrapNot n prelabel_pred) after in
  let body f =
    wrapAnd n CTL.NONSTRICT
      (label_pred,
       f (wrapAnd n CTL.NONSTRICT
	    (make_raw_match n label false ast,
	     (make_seq n guard
		[first_metamatch;
		  wrapAU n CTL.NONSTRICT (rest_nodes,last_node)])))) in
  quantify n (label_var::get_unquantified quantified [s])
    (sequencibility body n label_pred process_bef_aft seqible)

(* --------------------------------------------------------------------- *)
(* dots and nests *)

let dots_and_nests nest whencodes befaftexps dot_code after n label
    process_bef_aft statement_list statement guard builder wrapcode =
  let befaft = List.map (process_bef_aft guard) befaftexps in
  let befaftg = List.map (process_bef_aft true) befaftexps in
  let (notwhencodes,whencodes) =
    match whencodes with
      Ast.NoWhen -> (None,label_pred_maker n label)
    | Ast.WhenNot whencodes ->
	(Some (statement_list whencodes),label_pred_maker n label)
    | Ast.WhenAlways s -> (None,Some(statement s)) in
  let notwhencodes =
    (* add in after, because it's not part of the program *)
    if !Flag_parsing_cocci.sgrep_mode
    then
      let after = aftpred n label in
      match notwhencodes with
	None -> Some after
      |	Some x -> Some (wrapOr n (after,x))(*can use v because disjoint w/ x*)
    else notwhencodes in
  let ender =
    match after with
      After f -> f
    | Guard f -> CTL.rewrap f (CTL.Uncheck f)
    | End -> wrap n CTL.True
    | Tail ->
	let exit = endpred n label in
	let errorexit = exitpred n label in
	if !Flag_parsing_cocci.sgrep_mode
	then wrapOr n (exit,errorexit)
	else exit (* was wrap n CTL.False *) in
  builder n (guard_to_strict guard)
    (List.combine befaft befaftg,nest,notwhencodes,whencodes,dot_code,ender,
     aftret n label, truepred n label,
     make_match n label false (wrapcode Ast.Goto),
     make_match n None false (wrapcode Ast.Goto))

(* --------------------------------------------------------------------- *)
(* the main translation loop *)
  
let decl_to_not_decl n dots stmt make_match f =
  if dots
  then f
  else
    let de =
      let md =
	Ast.make_meta_decl "_d" (Ast.CONTEXT(Ast.NoPos,Ast.NOTHING)) ([],[],[])
      in
      Ast.rewrap md (Ast.Decl(Ast.CONTEXT(Ast.NoPos,Ast.NOTHING),false,md)) in
    wrapAU n CTL.NONSTRICT
      (make_match de,
       wrapAnd n CTL.NONSTRICT (wrap n (CTL.Not (make_match de)), f))

let rec statement_list stmt_list after quantified label dots_before guard =
  let n = Ast.get_line stmt_list in
  let isdots x =
    (* include Disj to be on the safe side *)
    match Ast.unwrap x with
      Ast.Dots _ | Ast.Nest _ | Ast.Disj _ -> true | _ -> false in
  let compute_label l e db = if db or isdots e then l else None in
  match Ast.unwrap stmt_list with
    Ast.DOTS(x) ->
      let rec loop quantified dots_before = function
	  ([],_) -> (match after with After f -> f | _ -> wrap n CTL.True)
	| ([e],_) ->
	    statement e after quantified (compute_label label e dots_before)
	      guard
	| (e::sl,fv::fvs) ->
	    let shared = intersectll fv fvs in
	    let unqshared = get_unquantified quantified shared in
	    let new_quantified = Common.union_set unqshared quantified in
	    quantify n unqshared
	      (statement e
		 (After(loop new_quantified (isdots e) (sl,fvs)))
		 new_quantified
		 (compute_label label e dots_before) guard)
	| _ -> failwith "not possible" in
      loop quantified dots_before (x,List.map Ast.get_fvs x)
  | Ast.CIRCLES(x) -> failwith "not supported"
  | Ast.STARS(x) -> failwith "not supported"

and statement stmt after quantified label guard =
  let n = Ast.get_line stmt in
  let wrapExists = wrapExists n true in
  let wrapAnd    = wrapAnd n CTL.NONSTRICT in
  let wrapOr     = wrapOr n in
  let wrapSeqOr  = wrapSeqOr n in
  let wrapAU     = wrapAU n CTL.NONSTRICT in
  let wrapAF     = wrapAF n CTL.NONSTRICT in
  let wrapAX     = wrapAX n CTL.NONSTRICT in
  let wrapBackEX = wrapBackEX n in
  let wrapBackAX = wrapBackAX n in
  let wrapNot    = wrapNot n in
  let wrapPred   = wrapPred n in
  let wrapLet    = wrapLet n in
  let wrapRef    = wrapRef n in
  let make_seq   = make_seq n guard in
  let make_seq_after = make_seq_after n guard in
  let quantify   = quantify n in
  let real_make_match = make_match in
  let make_match = make_match n label guard in

  match Ast.unwrap stmt with
    Ast.Atomic(ast) ->
      (match Ast.unwrap ast with
	Ast.MetaStmt((s,_,(Ast.CONTEXT(_,Ast.BEFOREAFTER(_,_)) as d)),
		     keep,seqible,_)
      | Ast.MetaStmt((s,_,(Ast.CONTEXT(_,Ast.AFTER(_)) as d)),keep,seqible,_)->
	  svar_context_with_add_after s n label quantified d ast seqible after
	    (process_bef_aft quantified n label true) guard
	    (Ast.get_fvs stmt, Ast.get_fresh stmt, Ast.get_inherited stmt)

      |	Ast.MetaStmt((s,_,d),keep,seqible,_) ->
	  svar_minus_or_no_add_after s n label quantified d ast seqible after
	    (process_bef_aft quantified n label true) guard
	    (Ast.get_fvs stmt, Ast.get_fresh stmt, Ast.get_inherited stmt)

      |	_ ->
	  let stmt_fvs = Ast.get_fvs stmt in
	  let fvs = get_unquantified quantified stmt_fvs in
	  let between_dots = Ast.get_dots_bef_aft stmt in
	  let term = make_match ast in
	  let term =
	    if guard
	    then term
	    else
	      match between_dots with
		Ast.BetweenDots (brace_term,n) ->
		  (match Ast.unwrap brace_term with
		    Ast.Atomic(brace_ast) ->
		      let v = Printf.sprintf "_r_%d" n in
		      let case1 = wrapAnd(wrapRef v,make_match brace_ast) in
		      let case2 = wrapAnd(wrapNot(wrapRef v),term) in
		      wrapLet
			(v,wrapOr
			   (wrapBackEX (truepred n label),
			    wrapBackEX (wrapBackEX (falsepred n label))),
			 wrapOr(case1,case2))
		  | _ -> failwith "not possible")
	      | Ast.NoDots -> term in
	  match Ast.unwrap ast with
            Ast.Return((_,info,retmc),(_,_,semmc)) ->
	      (* discard pattern that comes after return *)
	      let normal_res = make_seq_after (quantify fvs term) after in
	      (* the following code tries to propagate the modifications on
		 return; to a close brace, in the case where the final return
		 is absent *)
	      let new_mc =
		match (retmc,semmc) with
		  (Ast.MINUS(_,l1),Ast.MINUS(_,l2))
		| (Ast.CONTEXT(_,Ast.BEFORE(l1)),
		   Ast.CONTEXT(_,Ast.AFTER(l2))) ->
		    Some (Ast.CONTEXT(Ast.NoPos,Ast.BEFORE(l1@l2)))
		| (Ast.CONTEXT(_,Ast.BEFORE(_)),Ast.CONTEXT(_,Ast.NOTHING))
		| (Ast.CONTEXT(_,Ast.NOTHING),Ast.CONTEXT(_,Ast.NOTHING)) ->
		    Some retmc
		| (Ast.CONTEXT(_,Ast.NOTHING),Ast.CONTEXT(_,Ast.AFTER(l))) ->
		    Some (Ast.CONTEXT(Ast.NoPos,Ast.BEFORE(l)))
		| _ -> None in
	      let ret = ("return",{Ast.line = (-1);Ast.column = (-1)},
			 Ast.CONTEXT(Ast.NoPos,Ast.NOTHING)) in
	      let edots =
		Ast.rewrap ast
		  (Ast.Edots(("...",{Ast.line = (-1);Ast.column = (-1)},
			     Ast.CONTEXT(Ast.NoPos,Ast.NOTHING)),None)) in
	      let semi = (";",{Ast.line = (-1);Ast.column = (-1)},
			 Ast.CONTEXT(Ast.NoPos,Ast.NOTHING)) in
	      let simple_return =
		make_match(Ast.rewrap ast (Ast.Return(ret,semi))) in
	      let return_expr =
		make_match(Ast.rewrap ast (Ast.ReturnExpr(ret,edots,semi))) in
	      (match new_mc with
		Some new_mc ->
		  let exit = endpred n None in
		  let errorexit = exitpred n None in
		  let mod_rbrace =
		    Ast.rewrap ast (Ast.SeqEnd (("}",info,new_mc))) in
		  let stripped_rbrace =
		    Ast.rewrap ast
		      (Ast.SeqEnd
			 (("}",info,Ast.CONTEXT(Ast.NoPos,Ast.NOTHING)))) in
		  wrapOr(normal_res,
			 wrapAnd
			   (make_match mod_rbrace,
			    wrapAnd
			      (wrapBackAX
				 (wrapNot
				    (wrap n
				       (CTL.Uncheck
					  (wrapOr
					     (simple_return,return_expr))))),
			       wrapAU(make_match stripped_rbrace,
				      wrapOr(exit,errorexit)))))
	      |	_ ->
		  (* some change in the middle of the return, so have to
		     find an actual return *)
		  normal_res)
	  | Ast.ReturnExpr(_,_,_) ->
	      (* have to have the return, if there is a return value *)
	      make_seq_after (quantify fvs term) after
          | _ -> make_seq_after (quantify fvs term) after)
  | Ast.Seq(lbrace,decls,dots,body,rbrace) ->
      let (lbfvs,b1fvs,b2fvs,b3fvs,rbfvs) =
	match
	  seq_fvs quantified
	    [Ast.get_fvs lbrace;Ast.get_fvs decls;
	      Ast.get_fvs body;Ast.get_fvs rbrace]
	with
	  [(lbfvs,b1fvs);(_,b2fvs);(_,b3fvs);(rbfvs,_)] ->
	    (lbfvs,b1fvs,b2fvs,b3fvs,rbfvs)
	| _ -> failwith "not possible" in
      let pv = count_nested_braces stmt in
      let lv = get_label_ctr() in
      let paren_pred = wrapPred(Lib_engine.Paren pv,CTL.Control) in
      let label_pred = wrapPred(Lib_engine.Label lv,CTL.Control) in
      let start_brace =
	wrapAnd (quantify lbfvs (make_match lbrace),
		 wrapAnd (paren_pred,label_pred)) in
      let end_brace =
	wrapAnd
	  (quantify rbfvs (make_match rbrace),paren_pred) in
      let new_quantified2 =
	Common.union_set b1fvs (Common.union_set b2fvs quantified) in
      let new_quantified3 = Common.union_set b3fvs new_quantified2 in
      let pattern_as_given =
	wrapExists
	  (pv,wrapExists
	     (lv,quantify b1fvs
		(make_seq
		   [start_brace;
		     quantify b2fvs
		       (statement_list decls
			  (After
			     (decl_to_not_decl n dots stmt make_match
				(quantify b3fvs
				   (statement_list body
				      (After (make_seq_after end_brace after))
				      new_quantified3 (Some lv) true guard))))
			  new_quantified2 (Some lv) false guard)]))) in
      if ends_in_return body
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
	  let empty_rbrace =
	    match Ast.unwrap rbrace with
	      Ast.SeqEnd((data,info,_)) ->
		Ast.rewrap rbrace
		  (Ast.SeqEnd ((data,info,Ast.CONTEXT(Ast.NoPos,Ast.NOTHING))))
	    | _ -> failwith "unexpected close brace" in
	  make_seq
	    [make_match (Ast.rewrap stmt Ast.Goto);
	      wrapAX (* skip the destination label *)
		(wrapAU
		   (make_match empty_rbrace,
		    quantify b3fvs
		      (statement_list body End new_quantified3 None true
			 guard)))] in
	let pattern3 =
	  wrapExists
	    (lv,quantify b1fvs
	       (make_seq
		  [start_brace;
		    wrapAnd
		      (wrapAU
			 (wrap n
			    (CTL.Pred(Lib_engine.PrefixLabel(lv),CTL.Control)),
			  (wrapAnd (* brace must be eventually after goto *)
			     (real_make_match n (Some lv) false
				(Ast.rewrap stmt Ast.Goto),
			      wrapAF (wrap n end_brace)))),
		       quantify b2fvs
			 (statement_list decls
			    (After
			       (decl_to_not_decl n dots stmt make_match
				  (quantify b3fvs
				     (statement_list body Tail
					(*After
					   (make_seq_after
					      nopv_end_brace after)*)
					new_quantified3 None true guard))))
			    new_quantified2 (Some lv) false guard))])) in
	wrapOr(pattern_as_given,
	       match Ast.unwrap decls with
		 Ast.DOTS([]) -> wrapOr(pattern2,pattern3)
	       | Ast.DOTS(l) -> pattern3
	       | _ -> failwith "circles and stars not supported")
      else pattern_as_given
  | Ast.IfThen(ifheader,branch,aft) ->
      ifthen ifheader branch aft after quantified n label statement
	  make_match guard
	  (Ast.get_fvs stmt, Ast.get_fresh stmt, Ast.get_inherited stmt)
	 
  | Ast.IfThenElse(ifheader,branch1,els,branch2,aft) ->
      ifthenelse ifheader branch1 els branch2 aft after quantified n label
	  statement make_match guard
	  (Ast.get_fvs stmt, Ast.get_fresh stmt, Ast.get_inherited stmt)

  | Ast.While(header,body,aft) | Ast.For(header,body,aft) ->
      forwhile header body aft after quantified n label statement make_match
	guard (Ast.get_fvs stmt, Ast.get_fresh stmt, Ast.get_inherited stmt)

  | Ast.Disj(stmt_dots_list) -> (* list shouldn't be empty *)
      List.fold_left
	(function prev -> function cur ->
	  wrapSeqOr
	    (prev,statement_list cur after quantified label true guard))
	(statement_list (List.hd stmt_dots_list) after quantified label true
	   guard)
	(List.tl stmt_dots_list)

  | Ast.Nest(stmt_dots,whencode,t) ->
      let call builder stmt_dots =
	let dots_pattern =
	  statement_list stmt_dots (a2n after) quantified label true
	    guard in
	dots_and_nests (Some dots_pattern) whencode t None after n label
	  (process_bef_aft quantified n label)
	  (function x ->
	    statement_list x Tail quantified label true true)
	  (function x -> statement x Tail quantified label true)
	  guard builder (Ast.rewrap stmt) in

      (match Ast.unwrap stmt_dots with
	Ast.DOTS([l]) ->
	  (match Ast.unwrap l with
	    Ast.MultiStm(stm) ->
	      call wrapPDots (Ast.rewrap stmt_dots (Ast.DOTS([stm])))
	  | _ -> call wrapDots stmt_dots)
      |	_  -> call wrapDots stmt_dots)

  | Ast.Dots((_,i,d),whencodes,t) ->
      let dot_code =
	match d with
	  Ast.MINUS(_,_) ->
            (* no need for the fresh metavar, but ... is a bit wierd as a
	       variable name *)
	    Some(make_match (make_meta_rule_elem d ([],[],[])))
	| _ -> None in
      dots_and_nests None whencodes t dot_code after n label
	(process_bef_aft quantified n label)
	(function x -> statement_list x Tail quantified label true true)
	(function x -> statement x Tail quantified label true)
	guard wrapDots (Ast.rewrap stmt)

  | Ast.Switch(header,lb,cases,rb) ->
      (match after with
	After(_) -> failwith "pattern code after switch not supported"
      |	_ -> ());
      let header_fvs = Ast.get_fvs header in
      let lb_fvs = Ast.get_fvs lb in
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
	      |	_ -> failwith "not possible")
	  ([],[],[],[],[],[],[]) case_fvs in
      let (all_efvs,all_b1fvs,all_lbfvs,all_b2fvs,
	   all_casefvs,all_b3fvs,all_rbfvs) =
	(List.rev all_efvs,List.rev all_b1fvs,List.rev all_lbfvs,
	 List.rev all_b2fvs,List.rev all_casefvs,List.rev all_b3fvs,
	 List.rev all_rbfvs) in
      let rec intersect_all = function
	  [] -> []
	| [x] -> x
	| x::xs -> intersect x (intersect_all xs) in
      let rec union_all l = List.fold_left union [] l in
      let exponlyfvs = intersect_all all_efvs in
      let lbonlyfvs = intersect_all all_lbfvs in
(* don't do anything with right brace.  Hope there is no + code on it *)
(*      let rbonlyfvs = intersect_all all_rbfvs in*)
      let b1fvs = union_all all_b1fvs in
      let new1_quantified = union b1fvs quantified in
      let b2fvs = union (union_all all_b1fvs) (intersect_all all_casefvs) in
      let new2_quantified = union b2fvs new1_quantified in
(*      let b3fvs = union_all all_b3fvs in*)
      let switch_header = quantify exponlyfvs (make_match header) in
      let lb = quantify lbonlyfvs (make_match lb) in
(*      let rb = quantify rbonlyfvs (make_match rb) in*)
      let case_headers =
	List.map
	  (function case_line ->
	    match Ast.unwrap case_line with
	      Ast.CaseLine(header,body) ->
		let e1fvs =
		  match seq_fvs new2_quantified [Ast.get_fvs header] with
		    [(e1fvs,_)] -> e1fvs
		  | _ -> failwith "not possible" in
		quantify e1fvs (real_make_match n label true header)
	    | Ast.OptCase(case_line) -> failwith "not supported")
	  cases in
      let no_header =
	match case_headers with
	  [] -> wrap n CTL.True
	| [x] -> wrapNot x
	| x::xs ->
	    wrapNot
	      (List.fold_left
		 (function prev -> function cur -> wrapOr(prev,cur))
		 x xs) in
      let case_code =
	List.map
	  (function case_line ->
	    match Ast.unwrap case_line with
	      Ast.CaseLine(header,body) ->
		  let (e1fvs,b1fvs,s1fvs) =
		    let fvs = [Ast.get_fvs header;Ast.get_fvs body] in
		    match seq_fvs new2_quantified fvs with
		      [(e1fvs,b1fvs);(s1fvs,_)] -> (e1fvs,b1fvs,s1fvs)
		    | _ -> failwith "not possible" in
		  let case_header =
		    quantify e1fvs (make_match header) in
		  let new3_quantified = union b1fvs new2_quantified in
		  let body =
		    statement_list body Tail new3_quantified label
		      true(*?*) guard in
		  quantify b1fvs (make_seq [case_header; body])
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
	else (function x -> wrapOr(fallpred n label,x)) in
      quantify b1fvs
	(make_seq
	   [switch_header;
	     default_required
	       (quantify b2fvs
		  (make_seq
		     [wrapAnd(lb,
			      List.fold_left
				(function prev ->
				  function cur ->
				    wrapAnd(prev,wrapEX n cur))
				(wrap n CTL.True)
				case_headers);
		       List.fold_left
			 (function prev -> function cur ->
			   wrapOr(prev,cur))
			 no_header case_code]))])
  | Ast.FunDecl(header,lbrace,decls,dots,body,rbrace) ->
      let (hfvs,b1fvs,lbfvs,b2fvs,b3fvs,b4fvs,rbfvs) =
	match
	  seq_fvs quantified
	    [Ast.get_fvs header;Ast.get_fvs lbrace;Ast.get_fvs decls;
	      Ast.get_fvs body;Ast.get_fvs rbrace]
	with
	  [(hfvs,b1fvs);(lbfvs,b2fvs);(_,b3fvs);(_,b4fvs);(rbfvs,_)] ->
	    (hfvs,b1fvs,lbfvs,b2fvs,b3fvs,b4fvs,rbfvs)
	| _ -> failwith "not possible" in
      let function_header = quantify hfvs (make_match header) in
      let start_brace = quantify lbfvs (make_match lbrace) in
      let stripped_rbrace =
	match Ast.unwrap rbrace with
	  Ast.SeqEnd((data,info,_)) ->
	    Ast.rewrap rbrace
	      (Ast.SeqEnd ((data,info,Ast.CONTEXT(Ast.NoPos,Ast.NOTHING))))
	| _ -> failwith "unexpected close brace" in
      let end_brace =
	let exit = wrap n (CTL.Pred (Lib_engine.Exit,CTL.Control)) in
	let errorexit = wrap n (CTL.Pred (Lib_engine.ErrorExit,CTL.Control)) in
	wrapAnd(quantify rbfvs (make_match rbrace),
		wrapAnd(wrapBackEX(wrapNot(make_match stripped_rbrace)),
			wrapAU(make_match stripped_rbrace,
			       wrapOr(exit,errorexit)))) in
      let new_quantified3 =
	Common.union_set b1fvs
	  (Common.union_set b2fvs (Common.union_set b3fvs quantified)) in
      let new_quantified4 = Common.union_set b4fvs new_quantified3 in
      let fn_nest =
	match (Ast.undots decls,Ast.undots body,contains_modif rbrace) with
	  ([],[body],false) ->
	    (match Ast.unwrap body with
	      Ast.Nest(stmt_dots,Ast.NoWhen,_) -> Some (Common.Left stmt_dots)
	    | Ast.Dots(_,whencode,_) -> Some (Common.Right whencode)
	    | _ -> None)
	| _ -> None in
      let body_code =
	match fn_nest with
	  Some (Common.Left stmt_dots) ->
	    (* special case for function header + body - header is unambiguous
	       and unique, so we can just look for the nested body anywhere
	       else in the CFG *)
	    wrap n
	      (CTL.AndAny
		 (CTL.FORWARD,guard_to_strict guard,start_brace,
		  statement_list stmt_dots
		    (* discards match on right brace, but don't need it *)
		    (Guard (make_seq_after end_brace after))
		    new_quantified4 None true guard))
	| Some (Common.Right whencode) ->
	    (* try to be more efficient for the case where the body is just
	       ...  Perhaps this is too much of a special case, but useful
	       for dropping a parameter and checking that it is never used. *)
	    	    make_seq
	      [start_brace;
		match whencode with
		  Ast.NoWhen -> wrap n CTL.True
		| Ast.WhenNot(x) ->
		    wrapAU
		      (wrapNot(statement_list x Tail new_quantified4 label
				 true true),
		       make_match stripped_rbrace)
		| Ast.WhenAlways(x) ->
		    wrapAU (statement x Tail new_quantified4 label true,
			    make_match stripped_rbrace)]
	| None ->
	    make_seq
	      [start_brace;
		quantify b3fvs
		  (statement_list decls
		     (After
			(decl_to_not_decl n dots stmt make_match
			   (quantify b4fvs
			      (statement_list body
				 (After (make_seq_after end_brace after))
				 new_quantified4 None true guard))))
		     new_quantified3 None false guard)] in
      quantify b1fvs (make_seq [function_header; quantify b2fvs body_code])
  | Ast.OptStm(stm) ->
      failwith "OptStm should have been compiled away\n"
  | Ast.UniqueStm(stm) ->
      failwith "arities not yet supported"
  | Ast.MultiStm(stm) ->
      failwith "MultiStm should have been compiled away\n"
  | _ -> failwith "not supported"

(* un_process_bef_aft is because we don't want to do transformation in this
  code, and thus don't case about braces before or after it *)
and process_bef_aft quantified ln label guard = function
    Ast.WParen (re,n) ->
      let paren_pred = wrapPred ln (Lib_engine.Paren n,CTL.Control) in
      let s = guard_to_strict guard in
      wrapAnd ln s (make_raw_match ln None guard re,paren_pred)
  | Ast.Other s -> statement s Tail quantified label guard
  | Ast.Other_dots d -> statement_list d Tail quantified label true guard

(* --------------------------------------------------------------------- *)
(* letify.  Only before and after Dots. *)

let get_option f = function
    None -> None
  | Some x -> Some (f x)

let rec letify x =
  CTL.rewrap x
    (match CTL.unwrap x with
      CTL.False              -> CTL.False
    | CTL.True               -> CTL.True
    | CTL.Pred(p)            -> CTL.Pred(p)
    | CTL.Not(phi)           -> CTL.Not(letify phi)
    | CTL.Exists(v,phi,keep) -> CTL.Exists(v,letify phi,keep)
    | CTL.And(s,phi1,phi2)     ->
	let fail _ = CTL.And(s,letify phi1,letify phi2) in
	(match CTL.unwrap phi2 with
	  CTL.AX(dir1,s1,ax) ->
	    (match CTL.unwrap ax with
	      CTL.Dots(dir2,s2,before_after,nest,notwhens,whens,dotcode,rest,
		       ar,tr,goto,goto')
	      when dir1 = dir2 ->
		let (same,different) =
		  List.partition (function (x,_) -> x = phi1) before_after in
		(match same with
		  [] -> fail()
		| [(same,_)] ->
		    let v = get_let_ctr() in
		    CTL.LetR
		      (dir2,v,letify phi1,
		       CTL.rewrap x
			 (CTL.And
			    (s,CTL.rewrap phi1 (CTL.Ref v),
			     CTL.rewrap phi2
			       (CTL.AX
				  (dir1,s1,
				   letify
				     (CTL.rewrap ax
					(CTL.Dots
					   (dir2,s2,
					    (same,
					     CTL.rewrap same (CTL.Ref v))::
					    different,nest,
					    notwhens,whens,dotcode,rest,
					    ar,tr,goto,goto'))))))))
		|	_ -> failwith "duplicated befores?")
	    | _ -> fail())
	| _ -> fail())
    | CTL.AndAny(dir,s,phi1,phi2) -> CTL.AndAny(dir,s,letify phi1,letify phi2)
    | CTL.Or(phi1,phi2)      -> CTL.Or(letify phi1,letify phi2)
    | CTL.SeqOr(phi1,phi2)   -> CTL.SeqOr(letify phi1,letify phi2)
    | CTL.Implies(phi1,phi2) -> CTL.Implies(letify phi1,letify phi2)
    | CTL.AF(dir,s,phi1)     -> CTL.AF(dir,s,letify phi1)
    | CTL.AX(dir1,s1,phi1)       ->
	(match CTL.unwrap phi1 with
	  CTL.PDots(dir2,s2,before_after,nest,notwhens,whens,dotcode,rest,
		    ar,tr,goto,goto') when dir1 = dir2 ->
	    drop_pdots phi1
	      (dir2,s2,List.map (function (x,y) -> (x,letify y)) before_after,
	       get_option letify nest,
	       get_option letify notwhens,get_option letify whens,
	       dotcode, letify rest, ar, tr, goto, goto')
	      (function ax -> CTL.rewrap x (CTL.AX(dir1,s1,ax)))
	      (function ex -> CTL.rewrap x (CTL.EX(dir1,ex)))
	| _ -> CTL.AX(dir1,s1,letify phi1))
    | CTL.AG(dir,s,phi1)     -> CTL.AG(dir,s,letify phi1)
    | CTL.EF(dir,phi1)       -> CTL.EF(dir,letify phi1)
    | CTL.EX(dir,phi1)       -> CTL.EX(dir,letify phi1)
    | CTL.EG(dir,phi1)       -> CTL.EG(dir,letify phi1)
    | CTL.AU(dir,s,phi1,phi2) -> CTL.AU(dir,s,letify phi1,letify phi2)
    | CTL.AW(dir,s,phi1,phi2) -> CTL.AW(dir,s,letify phi1,letify phi2)
    | CTL.EU(dir,phi1,phi2)  -> CTL.EU(dir,letify phi1,letify phi2)
    | CTL.Let (x,phi1,phi2)  -> CTL.Let (x,letify phi1,letify phi2)
    | CTL.LetR (d,x,phi1,phi2)  -> CTL.LetR (d,x,letify phi1,letify phi2)
    | CTL.Ref(s)             -> CTL.Ref(s)
    | CTL.Uncheck(phi1)      -> CTL.Uncheck(letify phi1)
    | CTL.Dots(dir,s,before_after,nest,notwhens,whens,dotcode,rest,ar,tr,
	       goto,goto') ->
	drop_dots x
	  (dir,s,List.map (function (x,y) -> (x,letify y)) before_after,
	   get_option letify nest,
	   get_option letify notwhens,get_option letify whens,
	   dotcode, letify rest, ar, tr, goto, goto')
    | CTL.PDots(dir,s,before_after,nest,notwhens,whens,dotcode,rest,ar,tr,
		goto,goto')->
	drop_pdots x
	  (dir,s,List.map (function (x,y) -> (x,letify y)) before_after,
	   get_option letify nest,
	   get_option letify notwhens,get_option letify whens,
	   dotcode, letify rest, ar, tr, goto, goto')
	  (function ax -> ax) (function ex -> ex))

and drop_dots x
    (dir,s,before_after,nest,notwhens,whens,dotcode,rest,
     aftret,truepred,gotopred,gotomatch) =
  let lst = function None -> [] | Some x -> [x] in
  let uncheck nw = CTL.rewrap x (CTL.Uncheck nw) in
  let not_uncheck y = CTL.rewrap x (CTL.Not (CTL.rewrap x (CTL.Uncheck y))) in
  let before_after =
    List.map not_uncheck (List.map (function (_,x) -> x) before_after) in
  let nest =
    get_option
      (function n -> 
	let v = get_let_ctr() in
	CTL.rewrap x
	  (CTL.Let
	     (v,n,
	      CTL.rewrap x
		(CTL.Or(CTL.rewrap n (CTL.Ref v),
			CTL.rewrap n
			  (CTL.Not
			     (CTL.rewrap n
				(CTL.Uncheck (CTL.rewrap n (CTL.Ref v))))))))))
      nest in
  let notwhens = get_option not_uncheck notwhens in
  let whens = get_option uncheck whens in
  let all =
    (lst dotcode) @ (lst nest) @ (lst notwhens) @ (lst whens) @ before_after in
  let wrap f = CTL.rewrap x f in
  let gotopred = (* dotcode may contain - *)
    match dotcode with
      None -> gotopred
    | Some x ->
	wrap
	  (CTL.And
	     (CTL.NONSTRICT,x,
	      wrap
		(CTL.And
		   (CTL.NONSTRICT,gotopred,
			 (* the code below keeps matching dotcode until
			    it doesn't match any more, in hopes of getting
			    rid of braces following a goto, if the dotcode
			    is -.  Probably doesn't completely work - depends
			    on the label associated with the dots *)
		    wrap(CTL.AU(dir,CTL.NONSTRICT,x,wrap(CTL.Not(x)))))))) in
  let build_aftret bef_aft_builder =
    aftret
      (* this gets the label of TrueBranch and associates it with the
	 return.  we should probably do this for the goto case also. *)
      (function x ->
	let lv = get_label_ctr() in
	let label_pred = wrapPred 0 (Lib_engine.Label lv,CTL.Control) in
	wrap
	  (CTL.Exists
	     (lv,
	      wrap
		(CTL.And
		   (CTL.NONSTRICT,
		    wrap(CTL.And (CTL.NONSTRICT,truepred,label_pred)),
		    bef_aft_builder (x (Some lv)))),
	      true))) in
  let build_big_rest bef_aft_builder =
    (* rest v After v (TrueBranch & A[!all U (exit v error_exit)]) *)
    let error_exiter =
      match nest with
	None -> (* sequence should stop at goto *)
	  wrap
	    (CTL.Or
	       (build_aftret bef_aft_builder,
	       (* encoding of a goto-generated error exit *)
		wrap
		  (CTL.And
		     (CTL.NONSTRICT,truepred,
		      bef_aft_builder
			(wrap
			   (CTL.And
			      (CTL.NONSTRICT,gotopred,
			       wrap
				 (CTL.Not
				    (wrap
				       (CTL.EX
					  (dir,
					   wrap
					     (CTL.EF(dir,gotomatch)))))))))))))
    | Some _ ->
	(* nest should keep going to exit or error exit *)
	build_aftret bef_aft_builder in
    wrap(CTL.Or(rest,error_exiter)) in
  
  match (all,!Flag_parsing_cocci.sgrep_mode) with
    ([],true) -> CTL.EF(dir,rest)
  | ([],false) ->
      CTL.AF(dir,s,
	     build_big_rest
	       (function body -> wrap(CTL.AF(dir,CTL.NONSTRICT,body))))
  | (l,true) ->
      let flat_all =
	foldr1
	  (function rest -> function cur -> CTL.rewrap x (CTL.And(s,cur,rest)))
	  l in
      CTL.EU(dir,flat_all,rest)
  | (l,false) ->
      let v = get_let_ctr() in
      let flat_all =
	foldr1
	  (function rest -> function cur -> CTL.rewrap x (CTL.And(s,cur,rest)))
	  l in
      let rest =
	build_big_rest
	  (function body ->
	    wrap(CTL.AU(dir,CTL.NONSTRICT,wrap(CTL.Ref v),body))) in
      CTL.LetR(dir,v,flat_all,wrap(CTL.AU(dir,s,wrap(CTL.Ref v),rest)))
	
(* f(); <... \+ g(); ...> h(); after 
   
   becomes:
   
   f(); & AX(A[!f(); & !h() & (!g(); v g();) U h(); after] &
   E[!f(); & !h() U (g() & AXA[!f(); & !h() U h(); after])])
   
Unfortunately, this is not really what we want.  We really want the outer
AX to become an EX.  That could perhaps be done with some postprocessing.
We have taken care of the AX(PDots ...) case, and hope nothing else can
show up. *)

and drop_pdots x
    (dir,s,before_after,nest,notwhens,whens,dotcode,rest,aftret,truepred,
     goto,goto')
    fA fE =
  let rewrap e = CTL.rewrap x e in
  let befaft_name = get_let_ctr() in
  let nest_name = get_let_ctr() in
  let rest_name = get_let_ctr() in
  let befaft_maker body =
    CTL.LetR(dir,befaft_name,
	     List.fold_left (function x -> function y -> rewrap(CTL.Or(x,y)))
	       (rewrap CTL.False)
	       (List.map (function (_,x) -> x) before_after),
	     body) in
  let nest_maker body =
    match nest with
      None -> body
    | Some n -> rewrap (CTL.LetR(dir,nest_name,n,body)) in
  let rest_maker body = rewrap (CTL.LetR(dir,rest_name,rest,body)) in
  let nest_pattern =
    rewrap
      (drop_dots x
	 (dir,s,
	  [(rewrap(CTL.Ref befaft_name),rewrap(CTL.Ref befaft_name))],
	  get_option (function _ -> rewrap(CTL.Ref nest_name)) nest,
	  notwhens,whens,dotcode,rewrap (CTL.Ref rest_name),
	  aftret,truepred,goto,goto')) in
  let exists_pattern body =
    rewrap(CTL.EU(dir,
		  rewrap
		    (CTL.Not
		       (rewrap(CTL.Uncheck(rewrap(CTL.Ref befaft_name))))),
	   rewrap(CTL.And(CTL.NONSTRICT,
			  rewrap(CTL.Uncheck(rewrap(CTL.Ref nest_name))),
			  body)))) in
  let end_pattern =
    rewrap
      (drop_dots x
	 (dir,CTL.NONSTRICT,
	  [(rewrap(CTL.Ref befaft_name),rewrap(CTL.Ref befaft_name))],
	  None,notwhens,whens,dotcode,
	  rewrap(CTL.Uncheck(rewrap(CTL.Ref rest_name))),
	  aftret,truepred,goto,goto')) in
  befaft_maker
    (nest_maker
       (rest_maker
	  (* fA might add AX or do nothing, fE might add EX or do nothing *)
	  (rewrap(CTL.And(CTL.NONSTRICT,
			  fE(exists_pattern end_pattern),fA nest_pattern)))))

(* --------------------------------------------------------------------- *)
(* Function declaration *)

let top_level ua t =
  used_after := ua;
  saved := Ast.get_saved t;
  match Ast.unwrap t with
    Ast.FILEINFO(old_file,new_file) -> failwith "not supported fileinfo"
  | Ast.DECL(stmt) ->
      let unopt = elim_opt.V.rebuilder_statement stmt in
      let unopt = preprocess_dots_e unopt in
      letify (statement unopt Tail [] None false)
  | Ast.CODE(stmt_dots) ->
      let unopt = elim_opt.V.rebuilder_statement_dots stmt_dots in
      let unopt = preprocess_dots unopt in
      letify (statement_list unopt Tail [] None false false)
  | Ast.ERRORWORDS(exps) -> failwith "not supported errorwords"

(* --------------------------------------------------------------------- *)
(* Entry points *)

let asttoctl l used_after =
  letctr := 0;
  labelctr := 0;
  let (l,used_after) =
    List.split
      (List.filter
	 (function (t,_) ->
	   match Ast.unwrap t with Ast.ERRORWORDS(exps) -> false | _ -> true)
	 (List.combine l used_after)) in
  List.map2 top_level used_after l

let pp_cocci_predicate (pred,modif) =
  Pretty_print_engine.pp_predicate pred

let cocci_predicate_to_string (pred,modif) =
  Pretty_print_engine.predicate_to_string pred

