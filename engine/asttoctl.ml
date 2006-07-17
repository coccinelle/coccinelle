(* false = simpler formulas, only for debugging *)
let useEU = ref true
(* true = don't see all matched nodes, only modified ones *)
let onlyModif = ref true

(* Question: where do we put the existential quantifier for or.  At the
moment, let it float inwards. *)

module Ast = Ast_cocci
module V = Visitor_ast
module CTL = Ast_ctl

let warning s = Printf.fprintf stderr "warning: %s\n" s

type cocci_predicate = Lib_engine.predicate * string Ast_ctl.modif

let aftpred = (Lib_engine.After,CTL.Control)
let retpred = (Lib_engine.Return,CTL.Control)

(* --------------------------------------------------------------------- *)

let wrap n ctl = (ctl,n)

let aftret =
  wrap 0 (CTL.Or(wrap 0 (CTL.Pred aftpred),wrap 0 (CTL.Pred retpred)))

let wrapImplies n (x,y) = wrap n (CTL.Implies(x,y))
let wrapExists n (x,y) = wrap n (CTL.Exists(x,y))
let wrapAnd n (x,y) = wrap n (CTL.And(x,y))
let wrapOr n (x,y) = wrap n (CTL.Or(x,y))
let wrapAU n (x,y) = wrap n (CTL.AU(x,y))
let wrapEU n (x,y) = wrap n (CTL.EU(x,y))
let wrapAX n (x) = wrap n (CTL.AX(x))
let wrapEX n (x) = wrap n (CTL.EX(x))
let wrapAG n (x) = wrap n (CTL.AG(x))
let wrapEG n (x) = wrap n (CTL.EG(x))
let wrapAF n (x) = wrap n (CTL.AF(x))
let wrapEF n (x) = wrap n (CTL.EF(x))
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
(* Whenify *)
(* For A ... B, neither A nor B should occur in the code matched by the ...
We add these to any when code associated with the dots *)

let rec when_dots before after d =
  Ast.rewrap d
    (match Ast.unwrap d with
      Ast.DOTS(l) -> Ast.DOTS(dots_list before after l)
    | Ast.CIRCLES(l) -> Ast.CIRCLES(dots_list before after l)
    | Ast.STARS(l) -> Ast.STARS(dots_list before after l))

and get_end f s =
  match Ast.unwrap s with
    Ast.Disj(stmt_dots_list) ->
      List.concat
	(List.map
	   (function x ->
	     match Ast.unwrap x with
	       Ast.DOTS(l) ->
		 (match l with [] -> [] | xs -> get_first (f xs))
	     | _ -> failwith "circles and stars not supported")
	   stmt_dots_list)
  | Ast.Dots(_,_,_) -> []
  | Ast.OptStm(stm) -> [stm]
  | Ast.UniqueStm(stm) -> [stm]
  | Ast.MultiStm(stm) -> [stm]
  | _ -> [s]

and get_first s =
  get_end (function xs -> List.hd xs) s

and get_last s =
  get_end (function xs -> List.hd (List.rev xs)) s

and dots_list before after = function
    [] -> []
  | [x] -> [when_statement before after x]
  | (cur::((aft::_) as rest)) ->
      (when_statement before (Some (get_first aft)) cur)::
      (dots_list (Some (get_last cur)) after rest)

and when_statement before after s =
  Ast.rewrap s
    (match Ast.unwrap s with
      Ast.Atomic(_) as x -> x
    | Ast.Seq(lbrace,body,rbrace) ->
	Ast.Seq(lbrace,when_dots None None body,rbrace)
    | Ast.IfThen(header,branch) ->
	Ast.IfThen(header,when_statement None None branch)
    | Ast.IfThenElse(header,branch1,els,branch2) ->
	Ast.IfThenElse(header,
		       when_statement None None branch1,els,
		       when_statement None None branch2)
    | Ast.While(header,body) -> Ast.While(header,when_statement None None body)
    | Ast.Disj(stmt_dots_list) ->
	Ast.Disj(List.map (when_dots before after) stmt_dots_list)
    | Ast.Nest(stmt_dots) -> Ast.Nest(when_dots None None stmt_dots)
    | Ast.Dots(d,whencode,t) as x ->
	(match (before,after) with
	  (None,None) -> x
	| (None,Some aft) -> Ast.Dots(d,whencode,t@aft)
	| (Some bef,None) -> Ast.Dots(d,whencode,t@bef)
	| (Some bef,Some aft) -> Ast.Dots(d,whencode,t@bef@aft))
    | Ast.FunDecl(header,lbrace,body,rbrace) ->
	Ast.FunDecl(header,lbrace,when_dots None None body,rbrace)
    | Ast.OptStm(stm) -> Ast.OptStm(when_statement before after stm)
    | Ast.UniqueStm(stm) -> Ast.UniqueStm(when_statement before after stm)
    | Ast.MultiStm(stm) -> Ast.MultiStm(when_statement before after stm)
    | _ -> failwith "not supported")

(* --------------------------------------------------------------------- *)
(* Computing free variables *)

type anything =
    Rule_elem        of Ast.rule_elem
  | Statement        of Ast.statement
  | StatementDots    of Ast.statement Ast.dots

let free_table =
  (Hashtbl.create(50) : (anything,string list) Hashtbl.t)

(* Note that we would really rather attach + code to - or context code that
shares the same variables, if there is such.  If we attach it to something
else, the we increase the scope of the variable, and may allow less
variation.  Perhaps this is never a problem, because multiple control-flow
paths are only possible when there are dots, and + code can't attach to
dots.  If there are two options for attaching the + code, then both options
necessarily occur the same number of times in the matched code, so it
doesn't matter where the quantifier goes. *)

let metaid (x,_,_) = x

let bind = Common.union_set
let option_default = []

let mcode astfvs (_,_,mcodekind) =
  let process_anything_list_list anythings =
    List.fold_left Common.union_set []
      (List.map
	 (function l ->
	   List.fold_left Common.union_set [] (List.map astfvs l))
	 anythings) in
  match mcodekind with
    Ast.MINUS(anythings) -> process_anything_list_list anythings
  | Ast.CONTEXT(befaft) ->
      (match befaft with
	Ast.BEFORE(ll) -> process_anything_list_list ll
      | Ast.AFTER(ll) -> process_anything_list_list ll
      | Ast.BEFOREAFTER(llb,lla) ->
	  Common.union_set
	    (process_anything_list_list lla)
	    (process_anything_list_list llb)
      | Ast.NOTHING -> [])
  | Ast.PLUS -> []

let donothing recursor k e = k e (* just combine in the normal way *)

let rec astfvident recursor k i =
  match Ast.unwrap i with
    Ast.MetaId(name) | Ast.MetaFunc(name) | Ast.MetaLocalFunc(name) ->
      Common.union_set [metaid name] (mcode astfvs name)
  | _ -> k i

and astfvexpr recursor k e =
  match Ast.unwrap e with
    Ast.MetaConst(name,_) | Ast.MetaErr(name) | Ast.MetaExpr(name,_)
  | Ast.MetaExprList(name) ->
      Common.union_set [metaid name] (mcode astfvs name)
  | _ -> k e

and astfvtypeC recursor k ty =
  match Ast.unwrap ty with
    Ast.MetaType(name) -> Common.union_set [metaid name] (mcode astfvs name)
  | _ -> k ty

and astfvparam recursor k p =
  match Ast.unwrap p with
    Ast.MetaParam(name) | Ast.MetaParamList(name) ->
      Common.union_set [metaid name] (mcode astfvs name)
  | _ -> k p

and astfvrule_elem recursor k re =
  let res =
    match Ast.unwrap re with
      Ast.MetaRuleElem(name) | Ast.MetaStmt(name) | Ast.MetaStmtList(name) ->
	Common.union_set [metaid name] (mcode astfvs name)
    | _ -> k re in
  Hashtbl.add free_table (Rule_elem re) res;
  res

and astfvstatement recursor k s =
  let res =
    match Ast.unwrap s with
      Ast.Dots(_,whencode,tmpcode)
    | Ast.Circles(_,whencode,tmpcode)
    | Ast.Stars(_,whencode,tmpcode) ->
	let _ = List.map recursor.V.combiner_statement tmpcode in k s
    | _ -> k s in
  Hashtbl.add free_table (Statement s) res;
  res

and astfvstatement_dots recursor k s = 
  let res = k s in Hashtbl.add free_table (StatementDots s) res; res

(* both of the following create the same recursor, but the restrictions on
   letrec don't seem to make it easy to export simple names for the various
   fields, so it is duplicated *)
and astfvs a =
  let recursor = V.combiner bind option_default
    (mcode astfvs) (mcode astfvs) (mcode astfvs) (mcode astfvs)
    (mcode astfvs) (mcode astfvs) (mcode astfvs) (mcode astfvs)
    (mcode astfvs) (mcode astfvs) (mcode astfvs)
    donothing donothing astfvstatement_dots
    astfvident astfvexpr donothing astfvtypeC astfvparam donothing
    astfvrule_elem astfvstatement donothing donothing in
  recursor.V.combiner_anything a

let aststmfvs s =
  let recursor = V.combiner bind option_default
    (mcode astfvs) (mcode astfvs) (mcode astfvs) (mcode astfvs)
    (mcode astfvs) (mcode astfvs) (mcode astfvs) (mcode astfvs)
    (mcode astfvs) (mcode astfvs) (mcode astfvs)
    donothing donothing astfvstatement_dots
    astfvident astfvexpr donothing astfvtypeC astfvparam donothing
    astfvrule_elem astfvstatement donothing donothing in
  recursor.V.combiner_statement s

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

let and_opt n first = function
    None -> first
  | Some rest -> wrapAnd n (first,rest)

let make_cond n branch re = wrapImplies n (branch,wrapAX n re)

let contains_modif =
  let bind x y = x or y in
  let option_default = false in
  let mcode (_,_,kind) =
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

let make_match n code =
  let v = fresh_var() in
  if contains_modif code
  then wrapExists n (v,wrapPred n (Lib_engine.Match(code),CTL.Modif v))
  else
    if !onlyModif
    then wrapPred n (Lib_engine.Match(code),CTL.Control)
    else wrapExists n (v,wrapPred n (Lib_engine.Match(code),CTL.UnModif v))

let make_raw_match n code = wrapPred n (Lib_engine.Match(code),CTL.Control)

let seq_fvs quantified term1 term2 =
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

let rec dots_stmt quantified l after =
  let n = Ast.get_line l in
  let quantify = quantify n in
  match Ast.unwrap l with
    Ast.DOTS(x) ->
      let fvs =
	List.map (function x -> Hashtbl.find free_table (Statement x)) x in
      let rec loop quantified = function
	  ([],[]) -> (match after with Some x -> x | None -> wrap n (CTL.True))
	| ([x],[fv]) ->
	    quantify (get_unquantified quantified fv)
	      (statement (Common.union_set fv quantified) x after)
	| (x::xs,fv::fvs) ->
	    let shared = intersectll fv fvs in
	    let unqshared = get_unquantified quantified shared in
	    let new_quantified = Common.union_set unqshared quantified in
	    quantify unqshared
	      (statement new_quantified x (Some(loop new_quantified (xs,fvs))))
	| _ -> failwith "not possible" in
      loop quantified (x,fvs)
  | Ast.CIRCLES(x) -> failwith "not supported"
  | Ast.STARS(x) -> failwith "not supported"

and statement quantified stmt after =

  let n = Ast.get_line stmt in
  let wrapExists = wrapExists n in
  let wrapAnd = wrapAnd n in
  let wrapOr = wrapOr n in
  let wrapAU = wrapAU n in
  let wrapEU = wrapEU n in
  let wrapAX = wrapAX n in
  let wrapEX = wrapEX n in
  let wrapAG = wrapAG n in
  let wrapAF = wrapAF n in
  let wrapEF = wrapEF n in
  let wrapNot = wrapNot n in
  let wrapPred = wrapPred n in
  let make_seq = make_seq n in
  let and_opt = and_opt n in
  let make_cond = make_cond n in
  let quantify = quantify n in
  let make_match = make_match n in
  let make_raw_match = make_raw_match n in

  match Ast.unwrap stmt with
    Ast.Atomic(ast) ->
      (match Ast.unwrap ast with
	Ast.MetaStmt((s,i,(Ast.CONTEXT(Ast.BEFOREAFTER(_,_)) as d)))
      |	Ast.MetaStmt((s,i,(Ast.CONTEXT(Ast.AFTER(_)) as d))) ->
	  let label_var = fresh_label_var "lab" in
	  let label_pred = wrapPred(Lib_engine.Label(label_var),CTL.Control) in
	  let prelabel_pred =
	    wrapPred(Lib_engine.PrefixLabel(label_var),CTL.Control) in
	  let matcher d =
	    make_match
	      (Ast.rewrap ast (Ast.MetaRuleElem(fresh_metavar(),i,d))) in
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

	  let first_node =
	    let fvs = get_unquantified quantified [s] in
	    wrapAnd(quantify fvs (make_raw_match ast),label_pred) in
	  let left_or =
	    make_seq full_metamatch
	      (Some(and_opt (wrapNot(prelabel_pred)) after)) in
	  let right_or =
	    make_seq first_metamatch
	      (Some(wrapAU(wrapAnd(middle_metamatch,prelabel_pred),
			   make_seq (wrapAnd(last_metamatch,prelabel_pred))
			     (Some(and_opt (wrapNot(prelabel_pred))
				     after))))) in
	  quantify [label_var]
	    (wrapAnd(first_node,wrapOr(left_or,right_or)))

      |	Ast.MetaStmt((s,i,d)) ->
	  let label_var = fresh_label_var "lab" in
	  let label_pred = wrapPred(Lib_engine.Label(label_var),CTL.Control) in
	  let prelabel_pred =
	    wrapPred(Lib_engine.PrefixLabel(label_var),CTL.Control) in
	  let matcher d =
	    make_match
	      (Ast.rewrap ast (Ast.MetaRuleElem(fresh_metavar(),i,d))) in
	  let first_metamatch = matcher d in
	  let rest_metamatch =
	    matcher
	      (match d with
		Ast.MINUS(_) -> Ast.MINUS([])
	      | Ast.CONTEXT(_) -> Ast.CONTEXT(Ast.NOTHING)
	      | Ast.PLUS -> failwith "not possible") in
	  let first_node =
	    let fvs = get_unquantified quantified [s] in
	    wrapAnd(quantify fvs (make_raw_match ast),
		    wrapAnd(first_metamatch,label_pred)) in
	  let rest_nodes = wrapAnd(rest_metamatch,prelabel_pred) in
	  let last_node = and_opt (wrapNot(prelabel_pred)) after in
	  quantify [label_var]
	    (make_seq first_node (Some (wrapAU(rest_nodes,last_node))))
      |	_ ->
	  let stmt_fvs = Hashtbl.find free_table (Statement stmt) in
	  let fvs = get_unquantified quantified stmt_fvs in
	  make_seq (quantify fvs (make_match ast)) after)
  | Ast.Seq(lbrace,body,rbrace) ->
      let v = fresh_label_var "p" in
      let paren_pred = wrapPred(Lib_engine.Paren v,CTL.Control) in
      let start_brace = wrapAnd(make_match lbrace,paren_pred) in
      let end_brace = wrapAnd(make_match rbrace,paren_pred) in
      wrapExists
	(v,make_seq start_brace
	   (Some(dots_stmt quantified body (Some (make_seq end_brace after)))))
  | Ast.IfThen(ifheader,branch) ->

(* "if (test) thn" becomes:
    if(test) & AX((TrueBranch v After) & TrueBranch => AX thn)

    "if (test) thn; after" becomes:
    if(test) & AX((TrueBranch v After)
                  & TrueBranch => AX thn
                  & After => AX after)
             & EX After *)

       (* free variables *) 
       let (efvs,bfvs,_) =
	 seq_fvs quantified (Rule_elem ifheader) (Statement branch) in
       let new_quantified = Common.union_set bfvs quantified in
       (* if header *)
       let if_header = quantify efvs (make_match ifheader) in
       (* then branch and after *)
       let true_branch =  wrapPred(Lib_engine.TrueBranch,CTL.Control) in
       let after_branch = wrapPred(Lib_engine.After,CTL.Control) in
       let then_line =
	 make_cond true_branch (statement new_quantified branch None) in
       let or_cases = wrapOr(true_branch,after_branch) in
       (* the code *)
       (match after with
	 None ->
	   quantify bfvs
	     (make_seq if_header (Some(wrapAnd(or_cases,then_line))))
       | Some after ->
	   let after_line = make_cond after_branch after in
	   quantify bfvs
	     (wrapAnd
		(if_header,
		  (wrapAnd
		     (wrapAX
			(wrapAnd(or_cases, wrapAnd(then_line,after_line))),
		      wrapEX(after_branch))))))
	 
  | Ast.IfThenElse(ifheader,branch1,_,branch2) ->

(*  "if (test) thn else els" becomes:
    if(test) & AX(TrueBranch => AX thn
                  & FalseBranch => AX els)
             & EX FalseBranch

    "if (test) thn else els; after" becomes:
    if(test) & AX(TrueBranch => AX thn
                  & FalseBranch => AX els
                  & After => AX after)
             & EX FalseBranch
             & EX After


 Note that we rely on the well-formedness of C programs.  For example, we
 do not use EX to check that there is at least one then branch, because
 there is always one.  And we do not check that there is only one then or
 else branch, because these again are always the case in a well-formed C
 program. *)
       (* free variables *)
       let (e1fvs,b1fvs,_) =
	 seq_fvs quantified (Rule_elem ifheader) (Statement branch1) in
       let (e2fvs,b2fvs,_) =
	 seq_fvs quantified (Rule_elem ifheader) (Statement branch2) in
       let bothfvs = Common.union_set b1fvs b2fvs in
       let exponlyfvs = Common.inter_set e1fvs e2fvs in
       let new_quantified = Common.union_set bothfvs quantified in
       (* if header *)
       let if_header = quantify exponlyfvs (make_match ifheader) in
       (* then and else branches *)
       let true_branch =
	 wrapPred(Lib_engine.TrueBranch,CTL.Control) in
       let false_branch =
	 wrapPred(Lib_engine.FalseBranch,CTL.Control) in
       let then_line =
	 make_cond true_branch (statement new_quantified branch1 None) in
       let else_line =
	 make_cond false_branch (statement new_quantified branch2 None) in
       (* the code *)
       (match after with
	None ->
	  quantify bothfvs
	    (wrapAnd
	       (if_header,
		 wrapAnd(wrapAX(wrapAnd(then_line,else_line)),
			 wrapEX(false_branch))))
      |	Some after ->
	  let after_branch = wrapPred(Lib_engine.After,CTL.Control) in
	  let after_line = make_cond after_branch after in
	  quantify bothfvs
	    (wrapAnd
	       (if_header,
		 wrapAnd(wrapAX(wrapAnd
				  (wrapAnd(then_line,else_line),after_line)),
			 wrapAnd(wrapEX(false_branch),wrapEX(after_branch))))))

  | Ast.While(header,body) ->
   (* the translation in this case is similar to that of an if with no else *)
      let (efvs,bfvs,_) =
	seq_fvs quantified (Rule_elem header) (Statement body) in
      let new_quantified = Common.union_set bfvs quantified in
      let while_header = quantify efvs (make_match header) in
      let true_branch = wrapPred(Lib_engine.TrueBranch,CTL.Control) in
      let body_line =
	make_cond true_branch (statement new_quantified body None) in
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
	      wrapAnd(wrapNot (dots_stmt quantified cur None),rest))
	  e l in
      let rec loop after = function
	  [] -> failwith "disj shouldn't be empty" (*wrap n CTL.False*)
	| [(nots,cur)] -> add_nots nots (dots_stmt quantified cur after)
	| (nots,cur)::rest ->
	    wrapOr(add_nots nots (dots_stmt quantified cur after),
		   loop after rest) in
      loop after (preprocess_disj stmt_dots_list)
  | Ast.Nest(stmt_dots) ->
      let dots_pattern = dots_stmt quantified stmt_dots None in
      (match after with
	None -> wrapAG(wrapOr(dots_pattern,wrapNot dots_pattern))
      |	Some after -> wrapAU(wrapOr(dots_pattern,wrapNot dots_pattern),after))
  | Ast.Dots((_,i,d),whencodes,tmp_whencode) ->
      let dot_code =
	match d with
	  Ast.MINUS(_) ->
            (* no need for the fresh metavar, but ... is a bit wierd as a
	       variable name *)
	    let s = fresh_metavar() in
	    Some(make_match (Ast.rewrap stmt (Ast.MetaRuleElem(s,i,d))))
	| _ -> None in
      let tmp_whencode =
	List.map (function s -> statement quantified s None) tmp_whencode in
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
		List.map (function w -> wrapNot(dots_stmt quantified w None))
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
      (match (after,phi3) with (* add in the after code to make the result *)
	  (None,None) -> wrap n (CTL.True)
	| (Some after,None) ->
	    wrapAnd(wrapAF(wrapOr(after,aftret)),wrapEF(after))
	| (None,Some whencode) -> wrapAU(whencode,aftret)
	| (Some after,Some whencode) ->
	    if !useEU
	    then
	      wrapAnd(wrapAU(whencode,wrapOr(after,aftret)),
		      wrapEU(whencode,after))
	    else wrapAU(whencode,wrapOr(after,aftret)))
  | Ast.FunDecl(header,lbrace,body,rbrace) ->
      let (hfvs,bfvs,_) =
	seq_fvs quantified (Rule_elem header) (StatementDots body) in
      let function_header = quantify hfvs (make_match header) in
      let new_quantified = Common.union_set bfvs quantified in
      let v = fresh_label_var "p" in
      let paren_pred = wrapPred(Lib_engine.Paren v,CTL.Control) in
      let start_brace = wrapAnd(make_match lbrace,paren_pred) in
      let end_brace = wrapAnd(make_match rbrace,paren_pred) in
      quantify bfvs
	(make_seq function_header
	   (Some
	      (wrapExists
		 (v,(make_seq start_brace
		       (Some(dots_stmt new_quantified body
			       (Some(make_seq end_brace after)))))))))
  | Ast.OptStm(stm) ->
      failwith "OptStm should have been compiled away\n";
  | Ast.UniqueStm(stm) ->
      warning "arities not yet supported";
      statement quantified stm after
  | Ast.MultiStm(stm) ->
      warning "arities not yet supported";
      statement quantified stm after
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
  | CTL.AF(phi) -> collect_duplicates phi
  | CTL.AX(phi) -> collect_duplicates phi
  | CTL.AG(phi) -> collect_duplicates phi
  | CTL.AU(phi1,phi2) -> collect_duplicates phi1; collect_duplicates phi2
  | CTL.EF(phi) -> collect_duplicates phi
  | CTL.EX(phi) -> collect_duplicates phi
  | CTL.EG(phi) -> collect_duplicates phi
  | CTL.EU(phi1,phi2) -> collect_duplicates phi1; collect_duplicates phi2
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
  | CTL.AF(phi) ->
      let (acc,new_phi) = replace_formulas dec phi in
      (acc,CTL.rewrap f (CTL.AF(new_phi)))
  | CTL.AX(phi) ->
      let (acc,new_phi) = replace_formulas dec phi in
      (acc,CTL.rewrap f (CTL.AX(new_phi)))
  | CTL.AG(phi) ->
      let (acc,new_phi) = replace_formulas dec phi in
      (acc,CTL.rewrap f (CTL.AG(new_phi)))
  | CTL.AU(phi1,phi2) ->
      let (acc1,new_phi1) = replace_formulas dec phi1 in
      let (acc2,new_phi2) = replace_formulas dec phi2 in
      (acc1@acc2,CTL.rewrap f (CTL.AU(new_phi1,new_phi2)))
  | CTL.EF(phi) ->
      let (acc,new_phi) = replace_formulas dec phi in
      (acc,CTL.rewrap f (CTL.EF(new_phi)))
  | CTL.EX(phi) ->
      let (acc,new_phi) = replace_formulas dec phi in
      (acc,CTL.rewrap f (CTL.EX(new_phi)))
  | CTL.EG(phi) -> 
      let (acc,new_phi) = replace_formulas dec phi in
      (acc,CTL.rewrap f (CTL.EG(new_phi)))
  | CTL.EU(phi1,phi2) ->
      let (acc1,new_phi1) = replace_formulas dec phi1 in
      let (acc2,new_phi2) = replace_formulas dec phi2 in
      (acc1@acc2,CTL.rewrap f (CTL.EU(new_phi1,new_phi2)))
  | _ -> failwith "not possible"

let ctlfv_table =
  (Hashtbl.create(50) :
     ((cocci_predicate,string,Wrapper_ctl.info) CTL.generic_ctl,
      string list (* fvs *) *
	string list (* intersection of fvs of subterms *))
     Hashtbl.t)

let intersect l1 l2 = List.filter (function x -> List.mem x l2) l1
let subset l1 l2 = List.for_all (function x -> List.mem x l2) l1

let rec ctl_fvs f =
  try let (fvs,_) = Hashtbl.find ctlfv_table f in fvs
  with Not_found ->
    let ((fvs,_) as res) =
      match CTL.unwrap f with
	CTL.False | CTL.True | CTL.Pred(_) -> ([],[])
      | CTL.Not(phi) | CTL.Exists(_,phi)
      | CTL.AF(phi) | CTL.AX(phi) | CTL.AG(phi)
      | CTL.EF(phi) | CTL.EX(phi) | CTL.EG(phi) -> (ctl_fvs phi,[])
      | CTL.And(phi1,phi2) | CTL.Or(phi1,phi2) | CTL.Implies(phi1,phi2)
      | CTL.AU(phi1,phi2) | CTL.EU(phi1,phi2) ->
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
    | CTL.AF(phi) -> CTL.rewrap f (CTL.AF(drop_one nm term phi))
    | CTL.AX(phi) -> CTL.rewrap f (CTL.AX(drop_one nm term phi))
    | CTL.AG(phi) -> CTL.rewrap f (CTL.AG(drop_one nm term phi))
    | CTL.AU(phi1,phi2) ->
	let (ffvs,inter) = find_fvs f in
	process_binary f ffvs inter nm term
	  (function _ -> CTL.AU(drop_one nm term phi1,drop_one nm term phi2))
    | CTL.EF(phi) -> CTL.rewrap f (CTL.EF(drop_one nm term phi))
    | CTL.EX(phi) -> CTL.rewrap f (CTL.EX(drop_one nm term phi))
    | CTL.EG(phi) -> CTL.rewrap f (CTL.EG(drop_one nm term phi))
    | CTL.EU(phi1,phi2) ->
	let (ffvs,inter) = find_fvs f in
	process_binary f ffvs inter nm term
	  (function _ -> CTL.EU(drop_one nm term phi1,drop_one nm term phi2))
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

let rec pp_ctl (pp_pred, pp_mvar) ctl =
  let rec pp_aux = fun (ctl,index) ->
    pp (string_of_int index);
    match ctl with
    | CTL.False              -> pp "False"
    | CTL.True               -> pp "True"
    | CTL.Pred(p)            -> pp_pred p
    | CTL.Not(phi)           -> pp char_not; box (fun () -> pp_aux phi)
    | CTL.Exists(v,phi)      ->  
	pp "(";
	pp ("Ex ");
	pp_mvar v;
	pp " . "; 
	Format.print_cut();
	box (fun () -> pp_aux phi); 
	pp ")"
    | CTL.And(phi1,phi2)     ->  pp_2args char_and phi1 phi2; 
    | CTL.Or(phi1,phi2)      ->  pp_2args char_or phi1 phi2; 
    | CTL.Implies(phi1,phi2) ->   pp_2args "=>" phi1 phi2;
    | CTL.AF(phi1)             -> pp "AF("; pp_arg phi1; pp ")"
    | CTL.AX(phi1)             -> pp "AX("; pp_arg phi1; pp ")"
    | CTL.AG(phi1)             -> pp "AG("; pp_arg phi1; pp ")"
    | CTL.EF(phi1)             -> pp "EF("; pp_arg phi1; pp ")"
    | CTL.EX(phi1)	         -> pp "EX("; pp_arg phi1; pp ")"
    | CTL.EG(phi1)		 -> pp "EG("; pp_arg phi1; pp ")"
    | CTL.AU(phi1,phi2)        -> pp "A[";pp_2args_bis "U" phi1 phi2; pp "]" 
    | CTL.EU(phi1,phi2)	 -> pp "E[";pp_2args_bis "U" phi1 phi2; pp "]" 
    | CTL.Let (x,phi1,phi2)  -> 
	pp ("Let"^" "^x); 
	Format.print_space ();
	pp "="; 
	Format.print_space ();
	box (fun () -> pp_aux phi1);
	Format.print_space ();
	pp "in"; 
	Format.print_space ();
	box (fun () -> pp_aux phi2);
    | CTL.Ref(s)             -> 
       (* pp "Ref(";  *)
	pp s; 
       (* pp ")" *)
	
  and pp_2args sym phi1 phi2 = 
    pp "(";
    box (fun () -> pp_aux phi1); 
    Format.print_space();
    pp sym;
    Format.print_space ();
    box (fun () -> pp_aux phi2);
    pp ")";
  and pp_2args_bis sym phi1 phi2 = 
    box (fun () -> pp_aux phi1); 
    Format.print_space();
    pp sym;
    Format.print_space();
    box (fun () -> pp_aux phi2)
      
  and pp_arg phi = box (fun () -> pp_aux phi) in
  
  Format.open_box 0;
  pp_aux ctl;
  Format.close_box ()
    

let letify f =
  Hashtbl.clear formula_table;
  Hashtbl.clear ctlfv_table;
  (* create a count of the number of occurrences of each subformula *)
  collect_duplicates f;
  (* give names to things that appear more than once *)
  assign_variables();
  (* replace duplicated formulas by their variables *)
  let (bindings,new_f) = replace_formulas 0 f in
(*
  Printf.printf "original formula\n";
  pp_ctl
    ((function (x,_) -> Lib_engine.pp_predicate x),
     (fun s -> Format.print_string s))
    f;
  Format.print_newline();
  Printf.printf "updated formula\n";
  pp_ctl
    ((function (x,_) -> Lib_engine.pp_predicate x),
     (fun s -> Format.print_string s))
    new_f;
  Format.print_newline();
  Printf.printf "bindings used in the updated formula\n";
  List.iter
    (function (nm,b) ->
      Printf.printf "%s: " nm;
      pp_ctl
	((function (x,_) -> Lib_engine.pp_predicate x),
	 (fun s -> Format.print_string s))
	b;
      Format.print_newline())
    bindings;
*)
  (* collect fvs of terms in bindings and new_f *)
  List.iter (function f -> let _ = ctl_fvs f in ())
    (new_f::(List.map (function (_,term) -> term) bindings));
  (* sort bindings with uses before defs *)
  let bindings = rev_order_bindings bindings in
  (* insert bindings as lets into the formula *)
  let res = drop_bindings bindings new_f in
(*
  Printf.printf "result\n";
  pp_ctl
    ((function (x,_) -> Lib_engine.pp_predicate x),
     (fun s -> Format.print_string s))
    res;
  Format.print_newline();
*)
  res

(* --------------------------------------------------------------------- *)
(* Function declaration *)

let top_level t =
  match Ast.unwrap t with
    Ast.DECL(decl) -> failwith "not supported decl"
  | Ast.INCLUDE(inc,s) -> failwith "not supported include"
  | Ast.FILEINFO(old_file,new_file) -> failwith "not supported fileinfo"
  | Ast.FUNCTION(stmt) ->
      let unopt = elim_opt.V.rebuilder_statement stmt in
      let when_added = when_statement None None unopt in
      let _ = aststmfvs when_added in
      letify(statement [] when_added None)
  | Ast.CODE(stmt_dots) ->
      let unopt = elim_opt.V.rebuilder_statement_dots stmt_dots in
      let when_added = when_dots None None unopt in
      List.iter
	(function x -> let _ = aststmfvs x in ())
	(Ast.undots when_added);
      letify(dots_stmt [] when_added None)
  | Ast.ERRORWORDS(exps) -> failwith "not supported errorwords"

(* --------------------------------------------------------------------- *)
(* Contains dots *)

let contains_dots =
  let bind x y = x or y in
  let option_default = false in
  let mcode x = false in
  let statement r k s =
    match Ast.unwrap s with Ast.Dots(_,_,_) -> true | _ -> k s in
  let continue r k e = k e in
  let stop r k e = false in
  let res =
    V.combiner bind option_default
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      donothing donothing donothing
      stop stop stop stop stop stop stop statement continue continue in
  res.V.combiner_top_level

(* --------------------------------------------------------------------- *)
(* Entry points *)

let asttoctl l =
  ctr := 0;
  lctr := 0;
  sctr := 0;
  let l =
    List.filter
      (function t ->
	match Ast.unwrap t with Ast.ERRORWORDS(exps) -> false | _ -> true)
      l in
  List.map top_level l

let pp_cocci_predicate (pred,modif) =
  Pretty_print_engine.pp_predicate pred

let cocci_predicate_to_string (pred,modif) =
  Pretty_print_engine.predicate_to_string pred
