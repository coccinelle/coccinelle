(* detect statements that are between dots in the minus code, because they
may need a special treatment if they are if branches *)

module Ast0 = Ast0_cocci
module Ast = Ast_cocci
module V0 = Visitor_ast0

(* A very coarse approximation.  We would really only like to return true
if a new statement is added.  For this it would be best to correlate with the
plus slice. Or at least be sure that the new stuff is on the far left or
far right. *)

let rec adding_something s =
  match Ast0.get_mcodekind s with
    Ast0.MINUS(mc) ->
      (match !mc with
	(* do better for the common case of replacing a stmt by another one *)
	([[Ast.StatementTag(_)]],tinfo) -> false
      |	(_,_) -> true)
  | Ast0.CONTEXT(mc) ->
      let (text,tinfo1,tinfo2) = !mc in
      (match text with Ast.NOTHING -> false | _ -> true)
  | Ast0.MIXED(_) -> not(contains_only_minus.V0.combiner_statement s)
  | _ -> failwith "unexpected plus code"

(* needs a special case when there is a Disj *)
and contains_only_minus =
  let bind x y = x && y in
  let option_default = true in
  let mcodekind = function
      Ast0.MINUS(mc) ->
	(match !mc with
	  ([],_) -> true
	| _ -> false)
    | Ast0.CONTEXT(mc) -> false
    | _ -> false in
  let mcode (_,_,_,mc) = mcodekind mc in

  let donothing r k e = mcodekind (Ast0.get_mcodekind e) && k e in

  let expression r k e =
    mcodekind (Ast0.get_mcodekind e) &&
    match Ast0.unwrap e with
      Ast0.DisjExpr(starter,expr_list,mids,ender) ->
	List.for_all r.V0.combiner_expression expr_list
    | _ -> k e in

  let declaration r k e =
    mcodekind (Ast0.get_mcodekind e) &&
    match Ast0.unwrap e with
      Ast0.DisjDecl(starter,decls,mids,ender) ->
	List.for_all r.V0.combiner_declaration decls
    | _ -> k e in

  let typeC r k e =
    mcodekind (Ast0.get_mcodekind e) &&
    match Ast0.unwrap e with
      Ast0.DisjType(starter,types,mids,ender) ->
	List.for_all r.V0.combiner_typeC types
    | _ -> k e in

  let statement r k e =
    mcodekind (Ast0.get_mcodekind e) &&
    match Ast0.unwrap e with
      Ast0.Disj(starter,statement_dots_list,mids,ender) ->
	List.for_all r.V0.combiner_statement_dots statement_dots_list
    | _ -> k e in

  V0.combiner bind option_default
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    donothing donothing donothing donothing donothing
    donothing expression typeC donothing donothing declaration
    statement donothing donothing

(* ---------------------------------------------------------------------- *)

(*
Doesn't really work:

  if (acpi_device_dir(device))
+ {
    remove_proc_entry(acpi_device_bid(device), acpi_ac_dir);
+   acpi_device_dir(device) = NULL;
+ }

The last two + lines get associated with the end of the if, not with the
branch, so the braces get added in oddly.
*)

let add_braces orig_s =
  let s =
    (Iso_pattern.rebuild_mcode None).V0.rebuilder_statement orig_s in
  let new_mcodekind =
    match Ast0.get_mcodekind s with
      Ast0.MINUS(mc) ->
	let (text,tinfo) = !mc in
	Ast0.MINUS(ref([Ast.Token "{"]::text@[[Ast.Token "}"]],tinfo))
    | Ast0.CONTEXT(mc) ->
	let (text,tinfo1,tinfo2) = !mc in
	let new_text =
	  match text with
	    Ast.BEFORE(bef) ->
	      Ast.BEFOREAFTER([Ast.Token "{"]::bef,[[Ast.Token "}"]])
	  | Ast.AFTER(aft) ->
	      Ast.BEFOREAFTER([[Ast.Token "{"]],aft@[[Ast.Token "}"]])
	  | Ast.BEFOREAFTER(bef,aft) ->
	      Ast.BEFOREAFTER([Ast.Token "{"]::bef,aft@[[Ast.Token "}"]])
	  | Ast.NOTHING ->
	      Ast.BEFOREAFTER([[Ast.Token "{"]],[[Ast.Token "}"]]) in
	Ast0.CONTEXT(ref(new_text,tinfo1,tinfo2))
    | Ast0.MIXED(mc) ->
	let (text,tinfo1,tinfo2) = !mc in
	let new_text =
	  match text with
	    Ast.BEFORE(bef) ->
	      Ast.BEFOREAFTER([Ast.Token "{"]::bef,[[Ast.Token "}"]])
	  | Ast.AFTER(aft) ->
	      Ast.BEFOREAFTER([[Ast.Token "{"]],aft@[[Ast.Token "}"]])
	  | Ast.BEFOREAFTER(bef,aft) ->
	      Ast.BEFOREAFTER([Ast.Token "{"]::bef,aft@[[Ast.Token "}"]])
	  | Ast.NOTHING ->
	      Ast.BEFOREAFTER([[Ast.Token "{"]],[[Ast.Token "}"]]) in
	Ast0.MIXED(ref(new_text,tinfo1,tinfo2))
    | _ -> failwith "unexpected plus code" in
  Ast0.set_mcodekind s new_mcodekind;
  let s = Compute_lines.statement s in
  s

(* ---------------------------------------------------------------------- *)

let is_dots x =
  match Ast0.unwrap x with
    Ast0.Dots(_,_) | Ast0.Circles(_,_) | Ast0.Stars(_,_) -> true
  | _ -> false

let all_minus s =
  match Ast0.get_mcodekind s with
    Ast0.MINUS(_) -> true
  | _ -> false

let rec statement dots_before dots_after s =
  let do_one s =
    if dots_before && dots_after &&
      (adding_something s or contains_only_minus.V0.combiner_statement s)
    then Ast0.set_dots_bef_aft s (Ast0.BetweenDots(add_braces s))
    else s in

  match Ast0.unwrap s with
    Ast0.FunDecl(x,stg,ty,name,lp,params,rp,lbrace,body,rbrace) ->
      (* true for close brace, because that represents any way we can
	 exit the function, which is not necessarily followed by an explicit
	 close brace. *)
      Ast0.rewrap s
	(Ast0.FunDecl(x,stg,ty,name,lp,params,rp,lbrace,
		      statement_dots false true body,
		      rbrace))
  | Ast0.Decl(_,_) -> s
  | Ast0.Seq(lbrace,body,rbrace) ->
      Ast0.rewrap s
	(Ast0.Seq(lbrace,statement_dots false false body,rbrace))
  | Ast0.ExprStatement(exp,sem) -> do_one s
  | Ast0.IfThen(iff,lp,exp,rp,branch1,x) ->
      do_one
	(Ast0.rewrap s
	   (Ast0.IfThen(iff,lp,exp,rp,statement false false branch1,x)))
  | Ast0.IfThenElse(iff,lp,exp,rp,branch1,els,branch2,x) ->
      do_one
	(Ast0.rewrap s
	   (Ast0.IfThenElse
	      (iff,lp,exp,rp,statement false false branch1,els,
		statement false false branch2,x)))
  | Ast0.While(whl,lp,exp,rp,body,x) ->
      do_one
	(Ast0.rewrap s
	   (Ast0.While(whl,lp,exp,rp,statement false false body,x)))
  | Ast0.Do(d,body,whl,lp,exp,rp,sem) ->
      do_one
	(Ast0.rewrap s
	   (Ast0.Do(d,statement false false body,whl,lp,exp,rp,sem)))
  | Ast0.For(fr,lp,e1,sem1,e2,sem2,e3,rp,body,x) ->
      do_one
	(Ast0.rewrap s
	   (Ast0.For(fr,lp,e1,sem1,e2,sem2,e3,rp,statement false false body,
		     x)))
  | Ast0.Switch(switch,lp,exp,rp,lb,cases,rb) ->
      do_one
	(Ast0.rewrap s
	   (Ast0.Switch(switch,lp,exp,rp,lb,List.map case_line cases,rb)))
  | Ast0.Break(br,sem) -> do_one s
  | Ast0.Continue(cont,sem) -> do_one s
  | Ast0.Return(ret,sem) -> do_one s
  | Ast0.ReturnExpr(ret,exp,sem) -> do_one s
  | Ast0.MetaStmt(name,_) -> do_one s
  | Ast0.MetaStmtList(name,_) -> do_one s
  | Ast0.Disj(starter,statement_dots_list,mids,ender) ->
      Ast0.rewrap s
	(Ast0.Disj(starter,
		   List.map (statement_dots dots_before dots_after)
		     statement_dots_list,
		   mids,ender))
  | Ast0.Nest(starter,stmt_dots,ender,whencode) ->
      Ast0.rewrap s
	(Ast0.Nest(starter,statement_dots true true stmt_dots,ender,whencode))
  | Ast0.Exp(exp) -> s
  | Ast0.Ty(ty) -> s
  | Ast0.Dots(d,whn) | Ast0.Circles(d,whn) | Ast0.Stars(d,whn) -> s
  | Ast0.Include(inc,string) -> do_one s
  | Ast0.Define(def,id,body) -> do_one s
  | Ast0.OptStm(re) -> 
      Ast0.rewrap s
	(Ast0.OptStm(statement dots_before dots_after re))
  | Ast0.UniqueStm(re) ->
      Ast0.rewrap s
	(Ast0.UniqueStm(statement dots_before dots_after re))
  | Ast0.MultiStm(re) ->
      Ast0.rewrap s
	(Ast0.MultiStm(statement dots_before dots_after re))

and case_line c =
  Ast0.rewrap c
    (match Ast0.unwrap c with
      Ast0.Default(def,colon,code) ->
	Ast0.Default(def,colon,statement_dots false false code)
    | Ast0.Case(case,exp,colon,code) ->
	Ast0.Case(case,exp,colon,statement_dots false false code)
    | Ast0.OptCase(case) -> Ast0.OptCase(case_line c))
  
and do_statement_dots dots_before dots_after = function
    [] -> []
  | dots::rest when is_dots dots ->
      dots::(do_statement_dots true dots_after rest)
  | [x] -> [statement dots_before dots_after x]
  | x::(dots::_ as rest) when is_dots dots ->
      (statement dots_before dots_after x)::
      do_statement_dots false dots_after rest
  | x::rest ->
      (statement dots_before dots_after x)::
      do_statement_dots false dots_after rest
	
and statement_dots dots_before dots_after d =
  Ast0.rewrap d
    (match Ast0.unwrap d with
      Ast0.DOTS(l) ->
	Ast0.DOTS(do_statement_dots dots_before dots_after l)
    | Ast0.CIRCLES(l) ->
	Ast0.CIRCLES(do_statement_dots dots_before dots_after l)
    | Ast0.STARS(l) ->
	Ast0.STARS(do_statement_dots dots_before dots_after l))

let top_level t =
  Ast0.rewrap t
    (match Ast0.unwrap t with
      Ast0.DECL(stmt_dots) -> Ast0.DECL(statement true true stmt_dots)
    | Ast0.CODE(stmt_dots) -> Ast0.CODE(statement_dots true true stmt_dots)
    | t -> t)

let single_statement l =
  if !Flag_parsing_cocci.sgrep_mode then l else List.map top_level l
