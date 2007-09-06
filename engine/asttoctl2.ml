(* for MINUS and CONTEXT, pos is always None in this file *)
(*search for require*)
(* true = don't see all matched nodes, only modified ones *)
let onlyModif = ref true(*false*)

let exists = ref false

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
    (CTL.True,_) | (_,CTL.True) -> CTL.True
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
      if !exists
      then CTL.EX(CTL.FORWARD,x)
      else CTL.AX(CTL.FORWARD,s,x)

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
    (CTL.True,true) -> CTL.EF(CTL.FORWARD,y)
  | (CTL.True,false) -> CTL.AF(CTL.FORWARD,s,y)
  | (_,true) -> CTL.EU(CTL.FORWARD,x,y)
  | (_,false) -> CTL.AU(CTL.FORWARD,s,x,y)

let label_pred_maker = function
    None -> CTL.True
  | Some label_var -> CTL.Pred(Lib_engine.PrefixLabel(label_var),CTL.Control)

let predmaker guard pred label =
  ctl_and (guard_to_strict guard) (CTL.Pred pred) (label_pred_maker label)

let aftpred     = predmaker false (Lib_engine.After,       CTL.Control)
let retpred     = predmaker false (Lib_engine.Return,      CTL.Control)
let enterpred   = predmaker false (Lib_engine.Enter,       CTL.Control)
let exitpred    = predmaker false (Lib_engine.ErrorExit,   CTL.Control)
let endpred     = predmaker false (Lib_engine.Exit,        CTL.Control)
let inlooppred  = predmaker false (Lib_engine.InLoop,      CTL.Control)
let truepred    = predmaker false (Lib_engine.TrueBranch,  CTL.Control)
let falsepred   = predmaker false (Lib_engine.FalseBranch, CTL.Control)
let fallpred    = predmaker false (Lib_engine.FallThrough, CTL.Control)

let aftret label_var f = ctl_or (aftpred label_var) (exitpred label_var)

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

    | (Ast.Dots(_,_,_,_)::Ast.OptStm(stm)::(Ast.Dots(_,_,_,_) as u)::urest,
       d0::_::d1::rest)
    | (Ast.Nest(_,_,_,_)::Ast.OptStm(stm)::(Ast.Dots(_,_,_,_) as u)::urest,
       d0::_::d1::rest) ->
	 let l = Ast.get_line stm in
	 let new_rest1 = stm :: (dots_list (u::urest) (d1::rest)) in
	 let new_rest2 = dots_list urest rest in
	 let (fv_rest1,fresh_rest1,inherited_rest1,s1) = varlists new_rest1 in
	 let (fv_rest2,fresh_rest2,inherited_rest2,s2) = varlists new_rest2 in
	 [d0;
	   (Ast.Disj
	      [(Ast.DOTS(new_rest1),l,fv_rest1,fresh_rest1,inherited_rest1,s1,
		Ast.NoDots,None);
		(Ast.DOTS(new_rest2),l,fv_rest2,fresh_rest2,inherited_rest2,s2,
		 Ast.NoDots,None)],
	      l,fv_rest1,fresh_rest1,inherited_rest1,s1,Ast.NoDots,None)]

    | (Ast.OptStm(stm)::urest,_::rest) ->
	 let l = Ast.get_line stm in
	 let new_rest1 = dots_list urest rest in
	 let new_rest2 = stm::new_rest1 in
	 let (fv_rest1,fresh_rest1,inherited_rest1,s1) = varlists new_rest1 in
	 let (fv_rest2,fresh_rest2,inherited_rest2,s2) = varlists new_rest2 in
	 [(Ast.Disj
	     [(Ast.DOTS(new_rest2),l,fv_rest2,fresh_rest2,inherited_rest2,s2,
	       Ast.NoDots,None);
	       (Ast.DOTS(new_rest1),l,fv_rest1,fresh_rest1,inherited_rest1,s1,
		Ast.NoDots,None)],
	   l,fv_rest2,fresh_rest2,inherited_rest2,s2,Ast.NoDots,None)]

    | ([Ast.Dots(_,_,_,_);Ast.OptStm(stm)],[d1;_]) ->
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
		       Ast.NoDots,None);
		       (Ast.DOTS([d1]),l,fv_d1,fresh_d1,inh_d1,saved_d1,
			Ast.NoDots,None)],
	     l,fv_both,fresh_both,inh_both,saved_both,Ast.NoDots,None)]

    | ([Ast.Nest(_,_,_,_);Ast.OptStm(stm)],[d1;_]) ->
	let l = Ast.get_line stm in
	let rw = Ast.rewrap stm in
	let rwd = Ast.rewrap stm in
	let dots =
	  Ast.Dots(("...",
		    {Ast.line = 0;Ast.column = 0;
		      Ast.strbef = [];Ast.straft = []},
		    Ast.CONTEXT(Ast.NoPos,Ast.NOTHING)),
		   [],[],[]) in
	[d1;rw(Ast.Disj[rwd(Ast.DOTS([stm]));
			 (Ast.DOTS([rw dots]),l,[],[],[],[],Ast.NoDots,None)])]

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
    mcode
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

(* what is the difference between tail and end??? *)

type after = After of formula | Guard of formula | Tail | End

let a2n = function After x -> Guard x | a -> a

(* --------------------------------------------------------------------- *)
(* Top-level code *)

let fresh_var _ = string2var "_v"
let fresh_pos _ = string2var "_pos" (* must be a constant *)

let fresh_metavar _ = "_S"

(* fvinfo is going to end up being from the whole associated statement.
   it would be better if it were just the free variables in d, but free_vars.ml
   doesn't keep track of free variables on + code *)
let make_meta_rule_elem d fvinfo =
  let nm = fresh_metavar() in
  Ast.make_meta_rule_elem nm d fvinfo

let get_unquantified quantified vars =
  List.filter (function x -> not (List.mem x quantified)) vars

let make_seq guard l =
  let s = guard_to_strict guard in
  foldr1 (function rest -> function cur -> ctl_and s cur (ctl_ax s rest)) l

let make_seq_after2 guard first rest =
  let s = guard_to_strict guard in
  match rest with
    After rest -> ctl_and s first (ctl_ax s (ctl_ax s rest))
  | _ -> first

let make_seq_after guard first rest =
  match rest with
    After rest -> make_seq guard [first;rest]
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
      Ast.FunHeader(bef,_,fninfo,name,lp,params,rp) ->
	bind (mcode r ((),(),bef)) res
    | Ast.Decl(bef,_,decl) -> bind (mcode r ((),(),bef)) res
    | _ -> res in
  let recursor =
    V.combiner bind option_default
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      mcode
      do_nothing do_nothing do_nothing do_nothing
      do_nothing do_nothing do_nothing do_nothing do_nothing do_nothing
      do_nothing rule_elem do_nothing do_nothing do_nothing do_nothing in
  recursor.V.combiner_rule_elem

(* code is not a DisjRuleElem *)
let make_match label guard code =
  let v = fresh_var() in
  let matcher = Lib_engine.Match(code) in
  if contains_modif code && not guard
  then CTL.Exists(true,v,predmaker guard (matcher,CTL.Modif v) label)
  else
    match (!onlyModif,guard,intersect !used_after (Ast.get_fvs code)) with
      (true,_,[]) | (_,true,_) ->
	predmaker guard (matcher,CTL.Control) label
    | (b1,b2,l) ->
	CTL.Exists(true,v,predmaker guard (matcher,CTL.UnModif v) label)

let make_raw_match label guard code =
  predmaker guard (Lib_engine.Match(code),CTL.Control) label
    
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

let quantify =
  List.fold_right
    (function cur ->
      function code -> CTL.Exists (List.mem cur !saved,cur,code))

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
      Ast.Seq(_,_,_,_) | Ast.FunDecl(_,_,_,_,_) -> (k s) + 1
    | _ -> k s in
  let donothing r k e = k e in
  let mcode r x = 0 in
  let recursor = V.combiner bind option_default
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      mcode
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
  match Ast.unwrap sl with
    Ast.DOTS(x) ->
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
      (Ast.rewrap sl (Ast.DOTS(l)),a)
  | Ast.CIRCLES(x) -> failwith "not supported"
  | Ast.STARS(x) -> failwith "not supported"

and get_before sl a =
  match get_before_elem sl a with
    (term,Common.Left x) -> (term,x)
  | (term,Common.Right x) -> (term,x)

and get_before_whencode wc =
  List.map
    (function
	Ast.WhenNot w -> let (w,_) = get_before w [] in Ast.WhenNot w
      | Ast.WhenAlways w -> let (w,_) = get_before_e w [] in Ast.WhenAlways w
      |	Ast.WhenAny -> Ast.WhenAny)
    wc

and get_before_e s a =
  match Ast.unwrap s with
    Ast.Dots(d,w,_,aft) ->
      (Ast.rewrap s (Ast.Dots(d,get_before_whencode w,a,aft)),a)
  | Ast.Nest(stmt_dots,w,_,aft) ->
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
      (Ast.rewrap s (Ast.Nest(sd,w,a,aft)),[Ast.Other_dots stmt_dots])
  | Ast.Disj(stmt_dots_list) ->
      (* put the result of processing each branch in the whencode of the
	 subsequent branches.  acc_dsl collects these extra whencodes
	 not sure what the point of this is...
      *)
      let (dsl,_,dsla) =
	List.fold_left
	  (function (dsl,acc_dsl,dsla) ->
	    function cur ->
	      let (cur_dsl,cur_dsla) = get_before_elem cur acc_dsl in
	      (cur_dsl::dsl,(Ast.Other_dots cur_dsl)::acc_dsl,cur_dsla::dsla))
	  ([],a,[]) stmt_dots_list in
      let dsl = List.rev dsl in
      let dsla =
	List.fold_left
	  (function prev ->
	    function
		Common.Left x -> Common.union_set x prev
	      |	Common.Right x -> x)
	  [] dsla in
      (Ast.rewrap s (Ast.Disj(dsl)),dsla)
  | Ast.Atomic(ast) ->
      (match Ast.unwrap ast with
	Ast.MetaStmt(_,_,_,_) -> (s,[])
      |	_ -> (s,[Ast.Other s]))
  | Ast.Seq(lbrace,decls,body,rbrace) ->
      let index = count_nested_braces s in
      let (de,dea) = get_before decls [Ast.WParen(lbrace,index)] in
      let (bd,_) = get_before body dea in
      (Ast.rewrap s (Ast.Seq(lbrace,de,bd,rbrace)),
       [Ast.WParen(rbrace,index)])
  | Ast.Define(header,body) ->
      let (body,_) = get_before body [] in
      (Ast.rewrap s (Ast.Define(header,body)), [Ast.Other s])
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
  | Ast.FunDecl(header,lbrace,decls,body,rbrace) ->
      let (de,dea) = get_before decls [] in
      let (bd,_) = get_before body dea in
      (Ast.rewrap s (Ast.FunDecl(header,lbrace,de,bd,rbrace)),[])
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

and get_after_whencode a wc =
  List.map
    (function
	Ast.WhenNot w -> let (w,_) = get_after w a (*?*) in Ast.WhenNot w
      | Ast.WhenAlways w -> let (w,_) = get_after_e w a in Ast.WhenAlways w
      |	Ast.WhenAny -> Ast.WhenAny)
    wc

and get_after_e s a =
  match Ast.unwrap s with
    Ast.Dots(d,w,bef,_) ->
      (Ast.rewrap s (Ast.Dots(d,get_after_whencode a w,bef,a)),a)
  | Ast.Nest(stmt_dots,w,bef,_) ->
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
      (Ast.rewrap s (Ast.Nest(sd,w,bef,a)),[Ast.Other_dots stmt_dots])
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
		    Ast.Dots(_,_,_,_) | Ast.Nest(_,_,_,_) ->
		      failwith
			"dots/nest not allowed before and after stmt metavar"
		  | _ -> ())
	      |	Ast.Other_dots x ->
		  (match Ast.undots x with
		    x::_ ->
		      (match Ast.unwrap x with
			Ast.Dots(_,_,_,_) | Ast.Nest(_,_,_,_) ->
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
  | Ast.Seq(lbrace,decls,body,rbrace) ->
      let index = count_nested_braces s in
      let (bd,bda) = get_after body [Ast.WParen(rbrace,index)] in
      let (de,_) = get_after decls bda in
      (Ast.rewrap s (Ast.Seq(lbrace,de,bd,rbrace)),
       [Ast.WParen(lbrace,index)])
  | Ast.Define(header,body) ->
      let (body,_) = get_after body a in
      (Ast.rewrap s (Ast.Define(header,body)), [Ast.Other s])
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
  | Ast.Do(header,body,tail) ->
      let (bd,_) = get_after_e body a in
      (Ast.rewrap s (Ast.Do(header,bd,tail)),[Ast.Other s])
  | Ast.Iterator(header,body,aft) ->
      let (bd,_) = get_after_e body a in
      (Ast.rewrap s (Ast.Iterator(header,bd,aft)),[Ast.Other s])
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
  | Ast.FunDecl(header,lbrace,decls,body,rbrace) ->
      let (bd,bda) = get_after body [] in
      let (de,_) = get_after decls bda in
      (Ast.rewrap s (Ast.FunDecl(header,lbrace,de,bd,rbrace)),[])
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
(* expressions *)

let exptymatch l make_match make_guard_match =
  let pos = fresh_pos() in
  let matches_guard_matches =
    List.map
      (function x ->
	(make_match (Ast.rewrap_pos x (Some pos)),
	 make_guard_match (Ast.rewrap_pos x (Some pos))))
      l in
  let (matches,guard_matches) = List.split matches_guard_matches in
  let rec suffixes = function
      [] -> []
    | x::xs -> xs::(suffixes xs) in
  let prefixes = List.rev (suffixes (List.rev guard_matches)) in
  let info = (* not null *)
    List.map2
      (function matcher ->
	function negates ->
	  CTL.Exists
	    (false,pos,
	     ctl_and CTL.NONSTRICT matcher
	       (ctl_not
		  (CTL.Uncheck (List.fold_left ctl_or_fl CTL.False negates)))))
      matches prefixes in
  List.fold_left ctl_or_fl CTL.False (List.rev info)

(* code might be a DisjRuleElem, in which case we break it apart
   code might contain an Exp or Ty
   this one pushes the quantifier inwards *)
let do_re_matches label guard res quantified =
  let make_guard_match x =
    let stmt_fvs = Ast.get_fvs x in
    let fvs = get_unquantified quantified stmt_fvs in
    quantify fvs (make_match None true x) in
  let make_match x =
    let stmt_fvs = Ast.get_fvs x in
    let fvs = get_unquantified quantified stmt_fvs in
    quantify fvs (make_match None guard x) in
  ctl_and CTL.NONSTRICT (label_pred_maker label)
    (match List.map Ast.unwrap res with
      [] -> failwith "unexpected empty disj"
    | Ast.Exp(e)::rest -> exptymatch res make_match make_guard_match
    | Ast.Ty(t)::rest  -> exptymatch res make_match make_guard_match
    | all ->
	if List.exists (function Ast.Exp(_) | Ast.Ty(_) -> true | _ -> false)
	    all
	then failwith "unexpected exp or ty";
	List.fold_left ctl_seqor CTL.False
	  (List.rev (List.map make_match res)))

(* code might be a DisjRuleElem, in which case we break it apart
   code doesn't contain an Exp or Ty
   this one is for use when it is not practical to push the quantifier inwards
 *)
let header_match label guard code : ('a, Ast.meta_name, 'b) CTL.generic_ctl =
  match Ast.unwrap code with
    Ast.DisjRuleElem(res) ->
      let make_match = make_match None guard in
      let orop = if guard then ctl_or else ctl_seqor in
      ctl_and CTL.NONSTRICT (label_pred_maker label)
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
	    (make_meta_rule_elem aft (afvs,afresh,ainh)) in
	(true,
	 make_seq_after guard after_pred
	   (After(make_seq_after guard match_endif after))) in
  let body = body after_branch in
  let s = guard_to_strict guard in
  (* the code *)
  quantify fvs
    (ctl_and s header
       (opt_and guard
	  (match (after,aft_needed) with
	    (After _,_) (* pattern doesn't end here *)
	  | (_,true) (* + code added after *) -> after_checks
	  | _ -> no_after_checks)
	  (ctl_ax_absolute s body)))

let ifthen ifheader branch ((afvs,_,_,_) as aft) after quantified label
    recurse make_match guard =
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
    | _ -> failwith "not possible" in
  let new_quantified = Common.union_set bfvs quantified in
  (* if header *)
  let if_header = quantify efvs (make_match ifheader) in
  (* then branch and after *)
  let true_branch =
    make_seq guard
      [truepred label; recurse branch Tail new_quantified label guard] in
  let after_pred = aftpred label in
  let or_cases after_branch =
    ctl_or true_branch (ctl_or (fallpred label) after_branch) in
  end_control_structure bfvs if_header or_cases after_pred
      (Some(ctl_ex after_pred)) None aft after label guard

let ifthenelse ifheader branch1 els branch2 ((afvs,_,_,_) as aft) after
    quantified label recurse make_match guard =
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
    | _ -> failwith "not possible" in
  let (e2fvs,b2fvs,s2fvs) =
    (* fvs on else? *)
    match seq_fvs quantified
	[Ast.get_fvs ifheader;Ast.get_fvs branch2;afvs] with
      [(e2fvs,b2fvs);(s2fvs,b2afvs);_] ->
	(e2fvs,Common.union_set b2fvs b2afvs,s2fvs)
    | _ -> failwith "not possible" in
  let bothfvs        = union (union b1fvs b2fvs) (intersect s1fvs s2fvs) in
  let exponlyfvs     = intersect e1fvs e2fvs in
  let new_quantified = union bothfvs quantified in
  (* if header *)
  let if_header = quantify exponlyfvs (make_match ifheader) in
  (* then and else branches *)
  let true_branch =
    make_seq guard
      [truepred label; recurse branch1 Tail new_quantified label guard] in
  let false_branch =
    make_seq guard
      [falsepred label; make_match els;
	recurse branch2 Tail new_quantified label guard] in
  let after_pred = aftpred label in
  let or_cases after_branch =
    ctl_or true_branch (ctl_or false_branch after_branch) in
  let s = guard_to_strict guard in
  end_control_structure bothfvs if_header or_cases after_pred
      (Some(ctl_and s (ctl_ex (falsepred label)) (ctl_ex after_pred)))
      (Some(ctl_ex (falsepred label)))
      aft after label guard

let forwhile header body ((afvs,_,_,_) as aft) after quantified label
    recurse make_match guard =
  let process _ =
    (* the translation in this case is similar to that of an if with no else *)
    (* free variables *) 
    let (efvs,bfvs) =
      match seq_fvs quantified [Ast.get_fvs header;Ast.get_fvs body;afvs] with
	[(efvs,b1fvs);(_,b2fvs);_] -> (efvs,Common.union_set b1fvs b2fvs)
      | _ -> failwith "not possible" in
    let new_quantified = Common.union_set bfvs quantified in
    (* loop header *)
    let header = quantify efvs (make_match header) in
    let body =
      make_seq guard
	[inlooppred label; recurse body Tail new_quantified label guard] in
    let after_pred = fallpred label in
    let or_cases after_branch = ctl_or body after_branch in
    end_control_structure bfvs header or_cases after_pred
      (Some(ctl_ex after_pred)) None aft after label guard in
  match (Ast.unwrap body,aft) with
    (Ast.Atomic(re),(_,_,_,Ast.CONTEXT(_,Ast.NOTHING))) ->
      (match Ast.unwrap re with
	Ast.MetaStmt((_,_,Ast.CONTEXT(_,Ast.NOTHING)),
		     Type_cocci.Unitary,_,false) ->
	  let (efvs) =
	    match seq_fvs quantified [Ast.get_fvs header] with
	      [(efvs,_)] -> efvs
	    | _ -> failwith "not possible" in
	  quantify efvs (make_match header)
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

let svar_context_with_add_after s label quantified d ast
    seqible after process_bef_aft guard fvinfo =
  let label_var = (*fresh_label_var*) string2var "_lab" in
  let label_pred =
    CTL.Pred (Lib_engine.Label(label_var),CTL.Control) in
  let prelabel_pred =
    CTL.Pred (Lib_engine.PrefixLabel(label_var),CTL.Control) in
  let matcher d = make_match None guard (make_meta_rule_elem d fvinfo) in
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
    ctl_and CTL.NONSTRICT middle_metamatch prelabel_pred in  
  let left_or = (* the whole statement is one node *)
    make_seq guard
      [full_metamatch; and_after guard (ctl_not prelabel_pred) after] in
  let right_or = (* the statement covers multiple nodes *)
    make_seq guard
      [first_metamatch;
        ctl_au CTL.NONSTRICT
	  rest_nodes
	  (make_seq guard
	     [ctl_and CTL.NONSTRICT last_metamatch label_pred;
	       and_after guard
		 (ctl_not prelabel_pred) after])] in
  let body f =
    ctl_and CTL.NONSTRICT label_pred
       (f (ctl_and CTL.NONSTRICT
	    (make_raw_match label false ast) (ctl_or left_or right_or))) in
  quantify (label_var::get_unquantified quantified [s])
    (sequencibility body label_pred process_bef_aft seqible)

let svar_minus_or_no_add_after s label quantified d ast
    seqible after process_bef_aft guard fvinfo =
  let label_var = (*fresh_label_var*) string2var "_lab" in
  let label_pred =
    CTL.Pred (Lib_engine.Label(label_var),CTL.Control) in
  let prelabel_pred =
    CTL.Pred (Lib_engine.PrefixLabel(label_var),CTL.Control) in
  let matcher d = make_match None guard (make_meta_rule_elem d fvinfo) in
  let first_metamatch = matcher d in
  let rest_metamatch =
    matcher
      (match d with
	Ast.MINUS(pos,_) -> Ast.MINUS(pos,[])
      | Ast.CONTEXT(pos,_) -> Ast.CONTEXT(pos,Ast.NOTHING)
      | Ast.PLUS -> failwith "not possible") in
  let rest_nodes = ctl_and CTL.NONSTRICT rest_metamatch prelabel_pred in
  let last_node = and_after guard (ctl_not prelabel_pred) after in
  let body f =
    ctl_and CTL.NONSTRICT label_pred
      (f (ctl_and CTL.NONSTRICT
	    (make_raw_match label false ast)
	    (make_seq guard
	       [first_metamatch;
		 ctl_au CTL.NONSTRICT rest_nodes last_node]))) in
  quantify (label_var::get_unquantified quantified [s])
    (sequencibility body label_pred process_bef_aft seqible)

(* --------------------------------------------------------------------- *)
(* dots and nests *)

let dots_au toend label s wrapcode x seq_after y =
  let lv = get_label_ctr() in
  let labelpred = CTL.Pred(Lib_engine.Label lv,CTL.Control) in
  let preflabelpred = label_pred_maker (Some lv) in
  let matchgoto =
    make_match None false (wrapcode Ast.Goto) in
  let matchbreak =
    make_match None false
      (wrapcode
	 (Ast.Break(Ast.make_mcode "break",Ast.make_mcode ";"))) in
  let matchcontinue =
     make_match None false
      (wrapcode
	 (Ast.Continue(Ast.make_mcode "continue",Ast.make_mcode ";"))) in
  let stop_early =
    if !exists
    then CTL.False
    else if toend
    then CTL.Or(aftpred label,exitpred label)
    else
      ctl_or (aftpred label)
	(quantify [lv]
	   (ctl_and CTL.NONSTRICT
	      (ctl_and CTL.NONSTRICT (truepred label) labelpred)
	      (ctl_au CTL.NONSTRICT preflabelpred
		 (ctl_and CTL.NONSTRICT preflabelpred
		    (ctl_or (retpred None)
		       (ctl_or matchcontinue
			  (ctl_and CTL.NONSTRICT
			     (ctl_or matchgoto matchbreak)
			     (ctl_ag s (ctl_not seq_after))))))))) in
  ctl_au s x (ctl_or y stop_early)

let rec dots_and_nests plus nest whencodes bef aft dotcode after label
    process_bef_aft statement_list statement guard wrapcode =
  let ctl_and_ns = ctl_and CTL.NONSTRICT in
  (* proces bef_aft *)
  let shortest l =
    List.fold_left ctl_or_fl CTL.False (List.map process_bef_aft l) in
  let bef_aft = (* to be negated *)
    try
      let _ =
	List.find (function Ast.WhenAny -> true | _ -> false) whencodes in
      CTL.False
    with Not_found -> shortest (Common.union_set bef aft) in
  (* the following is used when we find a goto, etc and consider accepting
     without finding the rest of the pattern *)
  let aft = shortest aft in
  (* process whencode *)
  let whencodes =
    let (poswhen,negwhen) =
      List.fold_left
	(function (poswhen,negwhen) ->
	  function
	      Ast.WhenNot whencodes ->
		(poswhen,ctl_or (statement_list whencodes) negwhen)
	    | Ast.WhenAlways stm ->
		(ctl_and CTL.NONSTRICT (statement stm) poswhen,negwhen)
	    | Ast.WhenAny -> (poswhen,negwhen))
	(CTL.True,bef_aft) (List.rev whencodes) in
    let poswhen = ctl_and_ns (label_pred_maker label) poswhen in
    let negwhen =
      if !exists
      then
        (* add in After, because it's not part of the program *)
	ctl_or (aftpred label) negwhen
      else negwhen in
    ctl_and_ns poswhen (ctl_not negwhen) in
  (* process dot code, if any *)
  let dotcode =
    match (dotcode,guard) with
      (None,_) | (_,true) -> CTL.True
    | (Some dotcode,_) -> dotcode in
  (* process nest code, if any *)
  let ornest =
    match (nest,guard) with
      (None,_) | (_,true) -> CTL.True
    | (Some nest,false) ->
	let v = get_let_ctr() in
        CTL.Let(v,nest,CTL.Or(CTL.Ref v,CTL.Not(CTL.Uncheck (CTL.Ref v)))) in
  let ender =
    match after with
      After f -> f
    | Guard f -> CTL.Uncheck f
    | End -> CTL.True
    | Tail -> endpred label
	  (* was the following, but not clear why sgrep should allow
	     incomplete patterns
	let exit = endpred label in
	let errorexit = exitpred label in
	if !exists
	then ctl_or exit errorexit (* end anywhere *)
	else exit (* end at the real end of the function *) *) in
  if plus
  then
    do_plus_dots (after = Tail) label guard wrapcode ornest nest whencodes
      aft ender
  else
    dots_au (after = Tail) label (guard_to_strict guard) wrapcode
      (ctl_and_ns dotcode (ctl_and_ns ornest whencodes))
      aft ender

and do_plus_dots toend label guard wrapcode ornest nest whencodes aft ender =
  (* f(); <... \+ g(); ...> h(); after 
     becomes:
        f(); & AX(A[!f(); & !h() & (!g(); v g();) U h(); after] &
        E[!f(); & !h() U (g() & AXA[!f(); & !h() U h(); after])])
  *)
  let nest =
    match nest with
      Some n -> n
    | None -> failwith "nest expected" in
  let v = get_let_ctr() in
  CTL.LetR
    (CTL.FORWARD,v,whencodes,
     ctl_and CTL.NONSTRICT
       (dots_au toend label (guard_to_strict guard) wrapcode
	  (ctl_and CTL.NONSTRICT (CTL.Ref v) ornest) aft ender)
       (CTL.EU(CTL.FORWARD,CTL.Ref v,
	       ctl_and CTL.NONSTRICT (CTL.Uncheck nest)
		 (CTL.AX(CTL.FORWARD,CTL.NONSTRICT,
			 (dots_au toend label (guard_to_strict guard) wrapcode
			    (CTL.Ref v) aft ender))))))

(* --------------------------------------------------------------------- *)
(* the main translation loop *)
  
let rec statement_list stmt_list after quantified label dots_before guard =
  let isdots x =
    (* include Disj to be on the safe side *)
    match Ast.unwrap x with
      Ast.Dots _ | Ast.Nest _ | Ast.Disj _ -> true | _ -> false in
  let compute_label l e db = if db or isdots e then l else None in
  match Ast.unwrap stmt_list with
    Ast.DOTS(x) ->
      let rec loop quantified dots_before = function
	  ([],_) -> (match after with After f -> f | _ -> CTL.True)
	| ([e],_) ->
	    statement e after quantified (compute_label label e dots_before)
	      guard
	| (e::sl,fv::fvs) ->
	    let shared = intersectll fv fvs in
	    let unqshared = get_unquantified quantified shared in
	    let new_quantified = Common.union_set unqshared quantified in
	    quantify unqshared
	      (statement e
		 (After(loop new_quantified (isdots e) (sl,fvs)))
		 new_quantified
		 (compute_label label e dots_before) guard)
	| _ -> failwith "not possible" in
      loop quantified dots_before (x,List.map Ast.get_fvs x)
  | Ast.CIRCLES(x) -> failwith "not supported"
  | Ast.STARS(x) -> failwith "not supported"

and statement stmt after quantified label guard =
  let ctl_au     = ctl_au CTL.NONSTRICT in
  let ctl_ax     = ctl_ax CTL.NONSTRICT in
  let ctl_and    = ctl_and CTL.NONSTRICT in
  let make_seq   = make_seq guard in
  let make_seq_after = make_seq_after guard in
  let real_make_match = make_match in
  let make_match = header_match label guard in
  let new_info =
    {Ast.line = (-1);Ast.column = (-1);Ast.strbef = []; Ast.straft = []} in

  let dots_done = ref false in (* hack for dots cases we can easily handle *)

  let term =
  match Ast.unwrap stmt with
    Ast.Atomic(ast) ->
      (match Ast.unwrap ast with
	Ast.MetaStmt((s,_,(Ast.CONTEXT(_,Ast.BEFOREAFTER(_,_)) as d)),
		     keep,seqible,_)
      | Ast.MetaStmt((s,_,(Ast.CONTEXT(_,Ast.AFTER(_)) as d)),keep,seqible,_)->
	  svar_context_with_add_after s label quantified d ast seqible after
	    (process_bef_aft quantified label true) guard
	    (Ast.get_fvs stmt, Ast.get_fresh stmt, Ast.get_inherited stmt)

      |	Ast.MetaStmt((s,_,d),keep,seqible,_) ->
	  svar_minus_or_no_add_after s label quantified d ast seqible after
	    (process_bef_aft quantified label true) guard
	    (Ast.get_fvs stmt, Ast.get_fresh stmt, Ast.get_inherited stmt)

      |	_ ->
	  let term =
	    match Ast.unwrap ast with
	      Ast.DisjRuleElem(res) -> do_re_matches label guard res quantified
	    | _ ->
		let stmt_fvs = Ast.get_fvs stmt in
		let fvs = get_unquantified quantified stmt_fvs in
		quantify fvs (make_match ast) in
	  match Ast.unwrap ast with
            Ast.Return((_,info,retmc),(_,_,semmc)) ->
	      (* discard pattern that comes after return *)
	      let normal_res = make_seq_after term after in
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
	      let ret = ("return",new_info,
			 Ast.CONTEXT(Ast.NoPos,Ast.NOTHING)) in
	      let edots =
		Ast.rewrap ast
		  (Ast.Edots(("...",new_info,
			     Ast.CONTEXT(Ast.NoPos,Ast.NOTHING)),None)) in
	      let semi = (";",new_info,Ast.CONTEXT(Ast.NoPos,Ast.NOTHING)) in
	      let simple_return =
		make_match(Ast.rewrap ast (Ast.Return(ret,semi))) in
	      let return_expr =
		make_match(Ast.rewrap ast (Ast.ReturnExpr(ret,edots,semi))) in
	      (match new_mc with
		Some new_mc ->
		  let exit = endpred None in
		  let errorexit = exitpred None in
		  let mod_rbrace =
		    Ast.rewrap ast (Ast.SeqEnd (("}",info,new_mc))) in
		  let stripped_rbrace =
		    Ast.rewrap ast
		      (Ast.SeqEnd
			 (("}",info,Ast.CONTEXT(Ast.NoPos,Ast.NOTHING)))) in
		  ctl_or normal_res
		    (ctl_and (make_match mod_rbrace)
		       (ctl_and
			  (ctl_back_ax
			     (ctl_not
				(CTL.Uncheck
				   (ctl_or simple_return return_expr))))
			  (ctl_au
			     (make_match stripped_rbrace)
			     (ctl_or exit errorexit))))
	      |	_ ->
		  (* some change in the middle of the return, so have to
		     find an actual return *)
		  normal_res)
          | _ ->
	      (* should try to deal with the dots_bef_aft problem elsewhere,
		 but don't have the courage... *)
	      let term =
		if guard
		then term
		else do_between_dots stmt term End quantified label guard in
	      dots_done := true;
	      make_seq_after term after)
  | Ast.Seq(lbrace,decls,body,rbrace) ->
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
      let paren_pred = CTL.Pred(Lib_engine.Paren pv,CTL.Control) in
      let label_pred = CTL.Pred(Lib_engine.Label lv,CTL.Control) in
      let start_brace =
	ctl_and
	  (quantify lbfvs (make_match lbrace))
	  (ctl_and paren_pred label_pred) in
      let end_brace =
	ctl_and (quantify rbfvs (make_match rbrace)) paren_pred in
      let new_quantified2 =
	Common.union_set b1fvs (Common.union_set b2fvs quantified) in
      let new_quantified3 = Common.union_set b3fvs new_quantified2 in
      let pattern_as_given =
	CTL.Exists
	  (true,pv,CTL.Exists
	     (true,lv,quantify b1fvs
		(make_seq
		   [start_brace;
		     quantify b2fvs
		       (statement_list decls
			  (After
			     (quantify b3fvs
				(statement_list body
				   (After (make_seq_after end_brace after))
				   new_quantified3 (Some lv) true guard)))
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
	      ctl_ax (* skip the destination label *)
		(ctl_au
		   (make_match empty_rbrace)
		   (quantify b3fvs
		      (statement_list body End new_quantified3 None true
			 guard)))] in
	let pattern3 =
	  CTL.Exists
	    (true,lv,quantify b1fvs
	       (make_seq
		  [start_brace;
		    ctl_and
		      (ctl_au
			 (CTL.Pred(Lib_engine.PrefixLabel(lv),CTL.Control))
			 (ctl_and (* brace must be eventually after goto *)
			    (real_make_match (Some lv) false
			       (Ast.rewrap stmt Ast.Goto))
			    (* want AF even for sgrep *)
			    (CTL.AF(CTL.FORWARD,CTL.STRICT,end_brace))))
		      (quantify b2fvs
			 (statement_list decls
			    (After
			       (quantify b3fvs
				  (statement_list body Tail
					(*After
					   (make_seq_after
					      nopv_end_brace after)*)
				     new_quantified3 None true guard)))
			    new_quantified2 (Some lv) false guard))])) in
	ctl_or pattern_as_given
	  (match Ast.unwrap decls with
	    Ast.DOTS([]) -> ctl_or pattern2 pattern3
	  | Ast.DOTS(l) -> pattern3
	  | _ -> failwith "circles and stars not supported")
      else pattern_as_given
  | Ast.IfThen(ifheader,branch,aft) ->
      ifthen ifheader branch aft after quantified label statement
	  make_match guard
	 
  | Ast.IfThenElse(ifheader,branch1,els,branch2,aft) ->
      ifthenelse ifheader branch1 els branch2 aft after quantified label
	  statement make_match guard

  | Ast.While(header,body,aft) | Ast.For(header,body,aft)
  | Ast.Iterator(header,body,aft) ->
      forwhile header body aft after quantified label statement make_match
	guard

  | Ast.Disj(stmt_dots_list) -> (* list shouldn't be empty *)
      ctl_and
	(label_pred_maker label)
	(List.fold_left ctl_seqor CTL.False
	   (List.map
	      (function sl ->
		statement_list sl after quantified label true guard)
	      stmt_dots_list))

  | Ast.Nest(stmt_dots,whencode,bef,aft) ->
      (* label in recursive call is None because label check is already
	 wrapped around the corresponding code *)
      let call stmt_dots =
	let dots_pattern =
	  statement_list stmt_dots (a2n after) quantified None true guard in
	dots_and_nests false
	  (Some dots_pattern) whencode bef aft None after label
	  (process_bef_aft quantified None true)
	  (function x -> statement_list x Tail quantified None true true)
	  (function x -> statement x Tail quantified None true)
	  guard (function x -> Ast.set_fvs [] (Ast.rewrap stmt x)) in

      (match Ast.unwrap stmt_dots with
	Ast.DOTS([l]) ->
	  (match Ast.unwrap l with
	    Ast.MultiStm(stm) ->
	      (* f(); <... \+ g(); ...> h(); after 
   
		 becomes:
   
		 f(); & AX(A[!f(); & !h() & (!g(); v g();) U h(); after] &
		 E[!f(); & !h() U (g() & AXA[!f(); & !h() U h(); after])])
   
		 Unfortunately, this is not really what we want.  We really
		 want the outer AX to become an EX.  That could perhaps be
		 done with some postprocessing.  We have taken care of the
		 AX(PDots ...) case, and hope nothing else can show up. *)

	      dots_and_nests true
		(Some (statement stm (a2n after) quantified None guard))
		whencode bef aft None after label
		(process_bef_aft quantified None true)
		(function x -> statement_list x Tail quantified None true true)
		(function x -> statement x Tail quantified None true)
		guard (function x -> Ast.set_fvs [] (Ast.rewrap stmt x))
	  | _ -> call stmt_dots)
      |	_  -> call stmt_dots)

  | Ast.Dots((_,i,d),whencodes,bef,aft) ->
      let dot_code =
	match d with
	  Ast.MINUS(_,_) ->
            (* no need for the fresh metavar, but ... is a bit wierd as a
	       variable name *)
	    Some(make_match (make_meta_rule_elem d ([],[],[])))
	| _ -> None in
      dots_and_nests false None whencodes bef aft dot_code after label
	(process_bef_aft quantified None true)
	(function x -> statement_list x Tail quantified None true true)
	(function x -> statement x Tail quantified None true)
	guard (function x -> Ast.set_fvs [] (Ast.rewrap stmt x))

  | Ast.Switch(header,lb,cases,rb) ->
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
		quantify e1fvs (real_make_match label true header)
	    | Ast.OptCase(case_line) -> failwith "not supported")
	  cases in
      let no_header =
	ctl_not (List.fold_left ctl_or_fl CTL.False case_headers) in
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
	else function x -> ctl_or (fallpred label) x in
      let after_pred = aftpred label in
      let body after_branch =
	ctl_or
	  (default_required
	     (quantify b2fvs
		(make_seq
		   [ctl_and lb
		       (List.fold_left ctl_and CTL.True
			  (List.map ctl_ex case_headers));
		     List.fold_left ctl_or_fl no_header case_code])))
	  after_branch in
      end_control_structure b1fvs switch_header body
	after_pred (Some(ctl_ex after_pred)) None
	(* fake aft; attaching to close brace not yet supported *)
	([],[],[],Ast.CONTEXT(Ast.NoPos,Ast.NOTHING))
	after label guard
  | Ast.FunDecl(header,lbrace,decls,body,rbrace) ->
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
	let exit = CTL.Pred (Lib_engine.Exit,CTL.Control) in
	let errorexit = CTL.Pred (Lib_engine.ErrorExit,CTL.Control) in
	ctl_and
	  (quantify rbfvs (make_match rbrace))
	  (ctl_and
	     (ctl_back_ex (ctl_not (make_match stripped_rbrace)))
	     (ctl_au
		(make_match stripped_rbrace)
		(ctl_or exit errorexit))) in
      let new_quantified3 =
	Common.union_set b1fvs
	  (Common.union_set b2fvs (Common.union_set b3fvs quantified)) in
      let new_quantified4 = Common.union_set b4fvs new_quantified3 in
      let fn_nest =
	match (Ast.undots decls,Ast.undots body,contains_modif rbrace) with
	  ([],[body],false) ->
	    (match Ast.unwrap body with
	      Ast.Nest(stmt_dots,[],_,_) ->
		(match Ast.undots stmt_dots with
		  [s] ->
		    (match Ast.unwrap s with
		      Ast.MultiStm(stm) ->
			(* not sure how to optimize this case *)
			None
		    | _ -> Some (Common.Left stmt_dots))
		| _ -> Some (Common.Left stmt_dots))
	    | Ast.Dots(_,whencode,_,_) -> Some (Common.Right whencode)
	    | _ -> None)
	| _ -> None in
      let body_code =
	match fn_nest with
	  Some (Common.Left stmt_dots) ->
	    (* special case for function header + body - header is unambiguous
	       and unique, so we can just look for the nested body anywhere
	       else in the CFG *)
	    CTL.AndAny
	      (CTL.FORWARD,guard_to_strict guard,start_brace,
	       statement_list stmt_dots
		 (* discards match on right brace, but don't need it *)
		 (Guard (make_seq_after end_brace after))
		 new_quantified4 None true guard)
	| Some (Common.Right whencode) ->
	    (* try to be more efficient for the case where the body is just
	       ...  Perhaps this is too much of a special case, but useful
	       for dropping a parameter and checking that it is never used. *)
	    make_seq
	      [start_brace;
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
					statement_list sl Tail new_quantified4
					  label true true in
				      ctl_or prev x
				  | Ast.WhenAny -> CTL.False)
			      CTL.False whencode))
			 (List.fold_left
			   (function prev ->
			     function
				 Ast.WhenAlways(s) ->
				   let x =
				     statement s Tail new_quantified4
				       label true in
				   ctl_and prev x
			       | Ast.WhenNot(sl) -> prev
			       | Ast.WhenAny -> CTL.True)
			   CTL.True whencode) in
		    ctl_au leftarg (make_match stripped_rbrace)]
	| None ->
	    make_seq
	      [start_brace;
		quantify b3fvs
		  (statement_list decls
		     (After
			(quantify b4fvs
			   (statement_list body
			      (After (make_seq_after end_brace after))
			      new_quantified4 None true guard)))
		     new_quantified3 None false guard)] in
      quantify b1fvs (make_seq [function_header; quantify b2fvs body_code])
  | Ast.Define(header,body) ->
      let (hfvs,bfvs,bodyfvs) =
	match seq_fvs quantified [Ast.get_fvs header;Ast.get_fvs body]
	with
	  [(hfvs,b1fvs);(bodyfvs,_)] -> (hfvs,b1fvs,bodyfvs)
	| _ -> failwith "not possible" in
      let define_header = quantify hfvs (make_match header) in
      let body_code =
	statement_list body after (Common.union_set bfvs quantified)
	  None true guard in
      quantify bfvs (make_seq [define_header; body_code])
  | Ast.OptStm(stm) ->
      failwith "OptStm should have been compiled away\n"
  | Ast.UniqueStm(stm) ->
      failwith "arities not yet supported"
  | Ast.MultiStm(stm) ->
      failwith "MultiStm should have been compiled away\n"
  | _ -> failwith "not supported" in
  if guard or !dots_done
  then term
  else do_between_dots stmt term after quantified label guard

(* term is the translation of stmt *)
and do_between_dots stmt term after quantified label guard =
    match Ast.get_dots_bef_aft stmt with
      Ast.AddingBetweenDots (brace_term,n)
    | Ast.DroppingBetweenDots (brace_term,n) ->
	let match_brace = statement brace_term after quantified label guard in
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
and process_bef_aft quantified label guard = function
    Ast.WParen (re,n) ->
      let paren_pred = CTL.Pred (Lib_engine.Paren n,CTL.Control) in
      let s = guard_to_strict guard in
      ctl_and s (make_raw_match None guard re) paren_pred
  | Ast.Other s ->
      statement s Tail quantified label guard
  | Ast.Other_dots d ->
      statement_list d Tail quantified label true guard

(* --------------------------------------------------------------------- *)
(* cleanup: convert AX to EX for pdots.
Concretely: AX(A[...] & E[...]) becomes AX(A[...]) & EX(E[...])
This is what we wanted in the first place, but it wasn't possible to make
because the AX and its argument are not created in the same place.
Rather clunky... *)

let rec cleanup = function
    CTL.False    -> CTL.False
  | CTL.True     -> CTL.True
  | CTL.Pred(p)  -> CTL.Pred(p)
  | CTL.Not(phi) -> CTL.Not(cleanup phi)
  | CTL.Exists(keep,v,phi) -> CTL.Exists(keep,v,cleanup phi)
  | CTL.AndAny(dir,s,phi1,phi2) ->
      CTL.AndAny(dir,s,cleanup phi1,cleanup phi2)
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
  | CTL.AX(dir,s,phi1) -> CTL.AX(dir,s,cleanup phi1)
  | CTL.AG(dir,s,phi1) -> CTL.AG(dir,s,cleanup phi1)
  | CTL.EF(dir,phi1)   -> CTL.EF(dir,cleanup phi1)
  | CTL.EX(dir,phi1)   -> CTL.EX(dir,cleanup phi1)
  | CTL.EG(dir,phi1)   -> CTL.EG(dir,cleanup phi1)
  | CTL.AW(dir,s,phi1,phi2) -> CTL.AW(dir,s,cleanup phi1,cleanup phi2)
  | CTL.AU(dir,s,phi1,phi2) -> CTL.AU(dir,s,cleanup phi1,cleanup phi2)
  | CTL.EU(dir,phi1,phi2)   -> CTL.EU(dir,cleanup phi1,cleanup phi2)
  | CTL.Let (x,phi1,phi2)   -> CTL.Let (x,cleanup phi1,cleanup phi2)
  | CTL.LetR (dir,x,phi1,phi2) -> CTL.LetR (dir,x,cleanup phi1,cleanup phi2)
  | CTL.Ref(s) -> CTL.Ref(s)
  | CTL.Uncheck(phi1) -> CTL.Uncheck(cleanup phi1)

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
      cleanup(statement unopt Tail [] None false)
  | Ast.CODE(stmt_dots) ->
      let unopt = elim_opt.V.rebuilder_statement_dots stmt_dots in
      let unopt = preprocess_dots unopt in
      let starts_with_dots =
	match Ast.undots stmt_dots with
	  d::ds ->
	    (match Ast.unwrap d with
	      Ast.Dots(_,_,_,_) | Ast.Circles(_,_,_,_) | Ast.Stars(_,_,_,_) ->
		true
	    | _ -> false)
	| _ -> false in
      let res = statement_list unopt Tail [] None false false in
      cleanup
	(if starts_with_dots
	then
	  (* EX because there is a loop on enter/top *)
	  ctl_and CTL.NONSTRICT (enterpred None) (ctl_ex res)
	else res)
  | Ast.ERRORWORDS(exps) -> failwith "not supported errorwords"

(* --------------------------------------------------------------------- *)
(* Entry points *)

let asttoctl (name,(_,_,exists_flag),l) used_after =
  letctr := 0;
  labelctr := 0;
  (if exists_flag = Ast.Exists or
    !Flag_parsing_cocci.sgrep_mode or !Flag.sgrep_mode2
  then exists := true
  else exists := false);

  let (l,used_after) =
    List.split
      (List.filter
	 (function (t,_) ->
	   match Ast.unwrap t with Ast.ERRORWORDS(exps) -> false | _ -> true)
	 (List.combine l used_after)) in
  let res = List.map2 top_level used_after l in
  exists := false;
  res

let pp_cocci_predicate (pred,modif) =
  Pretty_print_engine.pp_predicate pred

let cocci_predicate_to_string (pred,modif) =
  Pretty_print_engine.predicate_to_string pred
