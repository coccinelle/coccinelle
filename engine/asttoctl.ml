(* Question: where do we put the existential quantifier for or.  At the
moment, let it float inwards. *)

module Ast = Ast_cocci
module V = Visitor_ast
(* module CTL = Ast_ctl *)

let warning s = Printf.fprintf stderr "warning: %s\n" s

(* --------------------------------------------------------------------- *)

let texify s =
  let len = String.length s in
  let rec loop n =
    if n = len
    then ""
    else
      match String.get s n with
	'_' -> Printf.sprintf "\\_%s" (loop (n+1))
      | '{' -> Printf.sprintf "{\\ttlb}%s" (loop (n+1))
      | '}' -> Printf.sprintf "{\\ttrb}%s" (loop (n+1))
      | '>' -> Printf.sprintf "\\mth{>}%s" (loop (n+1))
      | c -> Printf.sprintf "%c%s" c (loop (n+1)) in
  Printf.sprintf "\\mita{%s}" (loop 0)

let pred2c = function
    Lib_engine.TrueBranch -> ("\\msf{TrueBranch}",10)
  | Lib_engine.FalseBranch -> ("\\msf{FalseBranch}",11)
  | Lib_engine.After -> ("\\msf{After}",5)
  | Lib_engine.Paren(s) -> ("\\msf{Paren}("^s^")",7+(String.length s))
  | Lib_engine.Match(re) ->
      let s = Unparse_cocci.rule_elem_to_string re in
      (Printf.sprintf "%s" (texify s),String.length s)

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

let rec when_dots before after = function
    Ast.DOTS(l) -> Ast.DOTS(dots_list before after l)
  | Ast.CIRCLES(l) -> Ast.CIRCLES(dots_list before after l)
  | Ast.STARS(l) -> Ast.STARS(dots_list before after l)

and dots_list before after = function
    [] -> []
  | [x] -> [when_statement before after x]
  | (cur::((aft::_) as rest)) ->
      (when_statement before (Some aft) cur)::
      (dots_list (Some cur) after rest)

and when_statement before after = function
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
  | _ -> failwith "not supported"

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

let rec astfvident recursor k = function
    Ast.MetaId(name) | Ast.MetaFunc(name) | Ast.MetaLocalFunc(name) ->
      Common.union_set [metaid name] (mcode astfvs name)
  | i -> k i

and astfvexpr recursor k = function
    Ast.MetaConst(name,_) | Ast.MetaErr(name) | Ast.MetaExpr(name,_)
  | Ast.MetaExprList(name) ->
      Common.union_set [metaid name] (mcode astfvs name)
  | e -> k e

and astfvtypeC recursor k = function
    Ast.MetaType(name) -> Common.union_set [metaid name] (mcode astfvs name)
  | t -> k t

and astfvparam recursor k = function
    Ast.MetaParam(name) | Ast.MetaParamList(name) ->
      Common.union_set [metaid name] (mcode astfvs name)
  | p -> k p

and astfvrule_elem recursor k re =
  let res =
    match re with
      Ast.MetaStmt(name) | Ast.MetaStmtList(name) ->
	Common.union_set [metaid name] (mcode astfvs name)
    | _ -> k re in
  Hashtbl.add free_table (Rule_elem re) res;
  res

and astfvstatement recursor k s =
  let res =
    match s with
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

let sctr = ref 0
let fresh_metavar _ =
  let c = !sctr in
  ctr := !sctr + 1;
  Printf.sprintf "_S%d" c

let get_unquantified quantified vars =
  List.filter (function x -> not (List.mem x quantified)) vars

let make_seq first = function
    None -> first
  | Some rest ->
      let after_line = Ast_ctl.Pred(Lib_engine.After,Ast_ctl.Control) in
      Ast_ctl.And(first,Ast_ctl.AX(Ast_ctl.Or(rest,after_line)))

let make_cond branch re = Ast_ctl.Implies(branch,Ast_ctl.AX(re))

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

let make_match code =
  let v = fresh_var() in
  if contains_modif code
  then Ast_ctl.Exists(v,Ast_ctl.Pred(Lib_engine.Match(code),Ast_ctl.Modif v))
  else Ast_ctl.Exists(v,Ast_ctl.Pred(Lib_engine.Match(code),Ast_ctl.UnModif v))

let seq_fvs quantified term1 term2 =
  let t1fvs = get_unquantified quantified (Hashtbl.find free_table term1) in
  let t2fvs = get_unquantified quantified (Hashtbl.find free_table term2) in
  let bothfvs = Common.inter_set t1fvs t2fvs in
  let t1onlyfvs = Common.minus_set t1fvs bothfvs in
  let t2onlyfvs = Common.minus_set t2fvs bothfvs in
  (t1onlyfvs,bothfvs,t2onlyfvs)

let quantify =
  List.fold_right (function cur -> function code -> Ast_ctl.Exists(cur,code))

let intersectll lst nested_list =
  List.filter (function x -> List.exists (List.mem x) nested_list) lst

let rec dots_stmt quantified l after =
  match l with
    Ast.DOTS(x) ->
      let fvs =
	List.map (function x -> Hashtbl.find free_table (Statement x)) x in
      let rec loop quantified = function
	  ([],[]) -> (match after with Some x -> x | None -> Ast_ctl.True)
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
  match stmt with
    Ast.Atomic(ast) ->
      let stmt_fvs = Hashtbl.find free_table (Statement stmt) in
      let fvs = get_unquantified quantified stmt_fvs in
      make_seq (quantify fvs (make_match ast)) after
  | Ast.Seq(lbrace,body,rbrace) ->
      let v = fresh_var() in
      let paren_pred = Ast_ctl.Pred(Lib_engine.Paren v,Ast_ctl.Control) in
      let start_brace = Ast_ctl.And(make_match lbrace,paren_pred) in
      let end_brace = Ast_ctl.And(make_match rbrace,paren_pred) in
      Ast_ctl.Exists
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
       let true_branch =
	 Ast_ctl.Pred(Lib_engine.TrueBranch,Ast_ctl.Control) in
       let after_branch =
	 Ast_ctl.Pred(Lib_engine.After,Ast_ctl.Control) in
       let then_line =
	 make_cond true_branch (statement new_quantified branch None) in
       let or_cases = Ast_ctl.Or(true_branch,after_branch) in
       (* the code *)
       (match after with
	 None ->
	   quantify bfvs
	     (make_seq if_header (Some(Ast_ctl.And(or_cases,then_line))))
       | Some after ->
	   let after_line = make_cond after_branch after in
	   quantify bfvs
	     (Ast_ctl.And
		(if_header,
		  (Ast_ctl.And
		     (Ast_ctl.AX
			(Ast_ctl.And(or_cases,
				     Ast_ctl.And(then_line,after_line))),
		      Ast_ctl.EX(after_branch))))))

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
	 Ast_ctl.Pred(Lib_engine.TrueBranch,Ast_ctl.Control) in
       let false_branch =
	 Ast_ctl.Pred(Lib_engine.FalseBranch,Ast_ctl.Control) in
       let then_line =
	 make_cond true_branch (statement new_quantified branch1 None) in
       let else_line =
	 make_cond false_branch (statement new_quantified branch2 None) in
       (* the code *)
       (match after with
	None ->
	  quantify bothfvs
	    (Ast_ctl.And
	       (if_header,
		 Ast_ctl.And(Ast_ctl.AX(Ast_ctl.And(then_line,else_line)),
			     Ast_ctl.EX(false_branch))))
      |	Some after ->
	  let after_branch =
	    Ast_ctl.Pred(Lib_engine.After,Ast_ctl.Control) in
	  let after_line = make_cond after_branch after in
	  quantify bothfvs
	    (Ast_ctl.And
	       (if_header,
		 Ast_ctl.And(Ast_ctl.AX(Ast_ctl.And
					  (Ast_ctl.And(then_line,else_line),
					   after_line)),
			     Ast_ctl.And(Ast_ctl.EX(false_branch),
					 Ast_ctl.EX(after_branch))))))

  | Ast.While(header,body) ->
   (* the translation in this case is similar to that of an if with no else *)
      let (efvs,bfvs,_) =
	seq_fvs quantified (Rule_elem header) (Statement body) in
      let new_quantified = Common.union_set bfvs quantified in
      let while_header = quantify efvs (make_match header) in
      let true_branch =
	Ast_ctl.Pred(Lib_engine.TrueBranch,Ast_ctl.Control) in
      let body_line =
	make_cond true_branch (statement new_quantified body None) in
      (match after with
	None -> quantify bfvs (make_seq while_header (Some body_line))
      | Some after ->
	  let after_branch = Ast_ctl.Pred(Lib_engine.After,Ast_ctl.Control) in
	  let after_line = make_cond after_branch after in
	  quantify bfvs
	    (Ast_ctl.And
	       (while_header,
		(Ast_ctl.And
		   (Ast_ctl.AX(Ast_ctl.And(body_line,after_line)),
		    Ast_ctl.EX(after_branch))))))
  | Ast.Disj(stmt_dots_list) ->
      let rec loop = function
	  [] -> Ast_ctl.False
	| [cur] -> dots_stmt quantified cur None
	| cur::rest -> Ast_ctl.Or(dots_stmt quantified cur None, loop rest) in
      loop stmt_dots_list
  | Ast.Nest(stmt_dots) ->
      let dots_pattern = dots_stmt quantified stmt_dots None in
      (match after with
	None ->
	  Ast_ctl.AG(Ast_ctl.Or(dots_pattern,Ast_ctl.Not dots_pattern))
      |	Some after ->
	  Ast_ctl.AU(Ast_ctl.Or(dots_pattern,Ast_ctl.Not dots_pattern),after))
  | Ast.Dots((_,i,d),whencode,tmp_whencode) ->
      let dot_code =
	match d with
	  Ast.MINUS(_) ->
            (* no need for the fresh metavar, but ... is a bit wierd as a
	       variable name *)
	    let s = fresh_metavar() in
	    Some(make_match (Ast.MetaStmt(s,i,d)))
	| _ -> None in
      let tmp_whencode =
	List.map (function s -> statement quantified s None) tmp_whencode in
      let phi1 =
	match tmp_whencode with (* start with tmp_whencode *)
	  [] -> None
	| x::xs ->
	    Some
	      (List.fold_left
		 (function rest -> function cur -> Ast_ctl.Or(cur,rest))
		 x xs) in
      let phi2 =
	match (whencode,phi1) with (* add whencode *)
	  (None,None) -> None
	| (Some whencode,None) ->
	    Some (Ast_ctl.Not(dots_stmt quantified whencode None))
	| (None,Some phi) -> Some (Ast_ctl.Not(phi))
	| (Some whencode,Some phi) ->
	    Some (Ast_ctl.And
		    (Ast_ctl.Not(dots_stmt quantified whencode None),
		     Ast_ctl.Not(phi))) in
      let phi3 =
	match (dot_code,phi2) with (* add - on dots, if any *)
	  (None,None) -> None
	| (Some dotcode,None) -> Some dotcode
	| (None,Some whencode) -> Some whencode
	| (Some dotcode,Some whencode) ->
	    Some(Ast_ctl.And(dotcode,whencode)) in
      let after_line = Ast_ctl.Pred(Lib_engine.After,Ast_ctl.Control) in
      (match (after,phi3) with (* add in the after code to make the result *)
	  (None,None) -> Ast_ctl.True
	| (Some after,None) -> Ast_ctl.AF(Ast_ctl.Or(after,after_line))
	| (None,Some whencode) -> Ast_ctl.AG(whencode)
	| (Some after,Some whencode) ->
	    Ast_ctl.AU(whencode,Ast_ctl.Or(after,after_line)))
  | Ast.FunDecl(header,lbrace,body,rbrace) ->
      let (hfvs,bfvs,_) =
	seq_fvs quantified (Rule_elem header) (StatementDots body) in
      let function_header = quantify hfvs (make_match header) in
      let new_quantified = Common.union_set bfvs quantified in
      let v = fresh_var() in
      let paren_pred = Ast_ctl.Pred(Lib_engine.Paren v,Ast_ctl.Control) in
      let start_brace = Ast_ctl.And(make_match lbrace,paren_pred) in
      let end_brace = Ast_ctl.And(make_match rbrace,paren_pred) in
      quantify bfvs
	(make_seq function_header
	   (Some
	      (Ast_ctl.Exists
		 (v,(make_seq start_brace
		       (Some(dots_stmt new_quantified body
			       (Some(make_seq end_brace after)))))))))
  | Ast.OptStm(stm) ->
      (* doesn't work for ?f(); f();, ie when the optional thing is the same
	 as the thing that comes immediately after *)
      let pattern = statement quantified stm after in
      (match after with
	None -> Ast_ctl.Or(pattern,Ast_ctl.Not pattern)
      |	Some after ->
	  Ast_ctl.Or(pattern,
		     Ast_ctl.And(Ast_ctl.Not (statement quantified stm None),
				 after)))
  | Ast.UniqueStm(stm) ->
      warning "arities not yet supported";
      statement quantified stm after
  | Ast.MultiStm(stm) ->
      warning "arities not yet supported";
      statement quantified stm after
  | _ -> failwith "not supported"

(* --------------------------------------------------------------------- *)
(* Function declaration *)

let top_level = function
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
(* Entry points *)

let asttoctl l =
  ctr := 0;
  let l = List.filter (function Ast.ERRORWORDS(exps) -> false | _ -> true) l in
  List.map top_level l
