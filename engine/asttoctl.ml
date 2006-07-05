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

let aftret = "_aftret" (* assumed to be a fresh variable *)

(* --------------------------------------------------------------------- *)

let wrap n ctl = (ctl,n)

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
(* Whenify *)
(* For A ... B, neither A nor B should occur in the code matched by the ...
We add these to any when code associated with the dots *)

let rec when_dots before after d =
  Ast.rewrap d
    (match Ast.unwrap d with
      Ast.DOTS(l) -> Ast.DOTS(dots_list before after l)
    | Ast.CIRCLES(l) -> Ast.CIRCLES(dots_list before after l)
    | Ast.STARS(l) -> Ast.STARS(dots_list before after l))

and dots_list before after = function
    [] -> []
  | [x] -> [when_statement before after x]
  | (cur::((aft::_) as rest)) ->
      (when_statement before (Some aft) cur)::
      (dots_list (Some cur) after rest)

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
    | Ast.Dots(d,whencode,_) as x ->
	(match (before,after) with
	  (None,None) -> x
	| (None,Some aft) -> Ast.Dots(d,whencode,[aft])
	| (Some bef,None) -> Ast.Dots(d,whencode,[bef])
	| (Some bef,Some aft) -> Ast.Dots(d,whencode,[bef;aft]))
    | Ast.FunDecl(header,lbrace,body,rbrace) ->
	Ast.FunDecl(header,lbrace,when_dots None None body,rbrace)
    | Ast.OptStm(stm) -> Ast.OptStm(when_statement None None stm)
    | Ast.UniqueStm(stm) -> Ast.UniqueStm(when_statement None None stm)
    | Ast.MultiStm(stm) -> Ast.MultiStm(when_statement None None stm)
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
  ctr := !ctr + 1;
  Printf.sprintf "v%d" c

let lctr = ref 0
let fresh_let_var _ =
  let c = !lctr in
  lctr := !lctr + 1;
  Printf.sprintf "l%d" c

let sctr = ref 0
let fresh_metavar _ =
  let c = !sctr in
  sctr := !sctr + 1;
  Printf.sprintf "_S%d" c

let get_unquantified quantified vars =
  List.filter (function x -> not (List.mem x quantified)) vars

let make_seq n first = function
    None -> first
  | Some rest -> wrapAnd n (first,wrapAX n rest)

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
  let wrapLet = wrapLet n in
  let wrapRef = wrapRef n in
  let make_seq = make_seq n in
  let make_cond = make_cond n in
  let quantify = quantify n in
  let make_match = make_match n in

  match Ast.unwrap stmt with
    Ast.Atomic(ast) ->
      let stmt_fvs = Hashtbl.find free_table (Statement stmt) in
      let fvs = get_unquantified quantified stmt_fvs in
      make_seq (quantify fvs (make_match ast)) after
  | Ast.Seq(lbrace,body,rbrace) ->
      let v = fresh_var() in
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
      let rec loop = function
	  [] -> wrap n CTL.False
	| [cur] -> dots_stmt quantified cur None
	| cur::rest ->
	    wrapOr(dots_stmt quantified cur None, loop rest) in
      loop stmt_dots_list
  | Ast.Nest(stmt_dots) ->
      let dots_pattern = dots_stmt quantified stmt_dots None in
      (match after with
	None -> wrapAG(wrapOr(dots_pattern,wrapNot dots_pattern))
      |	Some after -> wrapAU(wrapOr(dots_pattern,wrapNot dots_pattern),after))
  | Ast.Dots((_,i,d),whencode,tmp_whencode) ->
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
	match (whencode,phi1) with (* add whencode *)
	  (None,None) -> None
	| (Some whencode,None) ->
	    Some (wrapNot(dots_stmt quantified whencode None))
	| (None,Some phi) -> Some (wrapNot(phi))
	| (Some whencode,Some phi) ->
	    Some (wrapAnd
		    (wrapNot(dots_stmt quantified whencode None),
		     wrapNot(phi))) in
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
	    let v = fresh_let_var() in
	    wrapLet
	      (v,after,
	       wrapAnd(wrapAF(wrapOr(wrapRef v,wrapRef aftret)),
		       wrapEF(wrapRef v)))
	| (None,Some whencode) -> wrapAU(whencode,wrapRef aftret)
	| (Some after,Some whencode) ->
	    let v = fresh_let_var() in
	    let w = fresh_let_var() in
	    if !useEU
	    then
	      wrapLet(v,after,
		      (wrapLet(w,whencode,
			       wrapAnd(wrapAU
					 (wrapRef w,
					  wrapOr(wrapRef v,wrapRef aftret)),
				       wrapEU(wrapRef w,wrapRef v)))))
	    else wrapAU(whencode,wrapOr(after,wrapRef aftret)))
  | Ast.FunDecl(header,lbrace,body,rbrace) ->
      let (hfvs,bfvs,_) =
	seq_fvs quantified (Rule_elem header) (StatementDots body) in
      let function_header = quantify hfvs (make_match header) in
      let new_quantified = Common.union_set bfvs quantified in
      let v = fresh_var() in
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
      (* doesn't work for ?f(); f();, ie when the optional thing is the same
	 as the thing that comes immediately after *)
      let pattern = statement quantified stm after in
      (match after with
	None -> wrapOr(pattern,wrapNot pattern)
      |	Some after ->
	  wrapOr(pattern,
		 wrapAnd(wrapNot (statement quantified stm None),after)))
  | Ast.UniqueStm(stm) ->
      warning "arities not yet supported";
      statement quantified stm after
  | Ast.MultiStm(stm) ->
      warning "arities not yet supported";
      statement quantified stm after
  | _ -> failwith "not supported"

(* --------------------------------------------------------------------- *)
(* Function declaration *)

let top_level t =
  match Ast.unwrap t with
    Ast.DECL(decl) -> failwith "not supported"
  | Ast.INCLUDE(inc,s) -> failwith "not supported"
  | Ast.FILEINFO(old_file,new_file) -> failwith "not supported"
  | Ast.FUNCTION(stmt) ->
      let when_added = when_statement None None stmt in
      let _ = aststmfvs when_added in
      statement [] when_added None
  | Ast.CODE(stmt_dots) ->
      let when_added = when_dots None None stmt_dots in
      List.iter
	(function x -> let _ = aststmfvs x in ())
	(Ast.undots when_added);
      dots_stmt [] when_added None
  | Ast.ERRORWORDS(exps) -> failwith "not supported"

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
  List.map
    (function x ->
      if contains_dots x
      then
	let n = Ast.get_line x in
	wrapLet n (aftret,
		   wrapOr n (wrapPred n (Lib_engine.After,CTL.Control),
			     wrapPred n (Lib_engine.Return,CTL.Control)),
		   top_level x)
      else top_level x)
    l
