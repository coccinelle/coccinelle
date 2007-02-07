(* Arities matter for the minus slice, but not for the plus slice. *)

(* + only allowed on code in a nest (in_nest = true).  ? only allowed on
rule_elems, and on subterms if the context is ? also. *)

module Ast0 = Ast0_cocci
module Ast = Ast_cocci
module V0 = Visitor_ast0

(* --------------------------------------------------------------------- *)
(* Move plus tokens from the MINUS and CONTEXT structured nodes to the
corresponding leftmost and rightmost mcodes *)

let inline_mcodes =
  let bind x y = () in
  let option_default = () in
  let mcode _ = () in
  let do_nothing r k e =
    k e;
    let einfo = Ast0.get_info e in
    match (Ast0.get_mcodekind e) with
      Ast0.MINUS(replacements) ->
	(match !replacements with
	  ([],_) -> ()
	| replacements ->
	    let minus_try = function
		(true,mc) ->
		  if List.for_all
		      (function
			  Ast0.MINUS(mreplacements) -> true | _ -> false)
		      mc
		  then
		    (List.iter
		       (function
			   Ast0.MINUS(mreplacements) ->
			     mreplacements := replacements
			 | _ -> ())
		       mc;
		     true)
		  else false
	      | _ -> false in
	    if not (minus_try(einfo.Ast0.attachable_end,
			      einfo.Ast0.mcode_end)
		      or
    		    minus_try(einfo.Ast0.attachable_start,
			      einfo.Ast0.mcode_start))
	    then
	      failwith "minus tree should not have bad code on both sides")
    | Ast0.CONTEXT(befaft)
    | Ast0.MIXED(befaft) ->
	let concat starter startinfo ender endinfo =
	  let lst =
	    if not(ender = []) &&
	      startinfo.Ast0.tline_end = endinfo.Ast0.tline_start
	    then
	      let last = List.hd (List.rev starter) in
	      let butlast = List.rev(List.tl(List.rev starter)) in
	      butlast @ (last@(List.hd ender)) :: (List.tl ender)
	    else starter @ ender in
	  (lst,
	   {endinfo with Ast0.tline_start = startinfo.Ast0.tline_start}) in
	let attach_bef bef beforeinfo = function
	    (true,mcl) ->
	      List.iter
		(function
		    Ast0.MINUS(mreplacements) ->
		      let (mrepl,tokeninfo) = !mreplacements in
		      mreplacements := concat bef beforeinfo mrepl tokeninfo
		  | Ast0.CONTEXT(mbefaft) ->
		      (match !mbefaft with
			(Ast.BEFORE(mbef),mbeforeinfo,a) ->
			  let (newbef,newinfo) =
			    concat bef beforeinfo mbef mbeforeinfo in
			  mbefaft := (Ast.BEFORE(newbef),newinfo,a)
		      | (Ast.AFTER(maft),_,a) ->
			  mbefaft :=
			    (Ast.BEFOREAFTER(bef,maft),beforeinfo,a)
		      | (Ast.BEFOREAFTER(mbef,maft),mbeforeinfo,a) ->
			  let (newbef,newinfo) =
			    concat bef beforeinfo mbef mbeforeinfo in
			  mbefaft :=
			    (Ast.BEFOREAFTER(newbef,maft),newinfo,a)
		      | (Ast.NOTHING,_,a) ->
			  mbefaft := (Ast.BEFORE(bef),beforeinfo,a))
		  |	_ -> failwith "unexpected annotation")
		mcl
	  | _ ->
	      failwith
		"context tree should not have bad code on both sides" in
	let attach_aft aft afterinfo = function
	    (true,mcl) ->
	      List.iter
		(function
		    Ast0.MINUS(mreplacements) ->
		      let (mrepl,tokeninfo) = !mreplacements in
		      mreplacements := concat mrepl tokeninfo aft afterinfo
		  | Ast0.CONTEXT(mbefaft) ->
		      (match !mbefaft with
			(Ast.BEFORE(mbef),b,_) ->
			  mbefaft :=
			    (Ast.BEFOREAFTER(mbef,aft),b,afterinfo)
		      | (Ast.AFTER(maft),b,mafterinfo) ->
			  let (newaft,newinfo) =
			    concat maft mafterinfo aft afterinfo in
			  mbefaft := (Ast.AFTER(newaft),b,newinfo)
		      | (Ast.BEFOREAFTER(mbef,maft),b,mafterinfo) ->
			  let (newaft,newinfo) =
			    concat maft mafterinfo aft afterinfo in
			  mbefaft :=
			    (Ast.BEFOREAFTER(mbef,newaft),b,newinfo)
		      | (Ast.NOTHING,b,_) ->
			  mbefaft := (Ast.AFTER(aft),b,afterinfo))
		  |	_ -> failwith "unexpected annotation")
		mcl
	  | _ ->
	      failwith
		"context tree should not have bad code on both sides" in
	(match !befaft with
	  (Ast.BEFORE(bef),beforeinfo,_) ->
	    attach_bef bef beforeinfo
	      (einfo.Ast0.attachable_start,einfo.Ast0.mcode_start)
	| (Ast.AFTER(aft),_,afterinfo) ->
	    attach_aft aft afterinfo
	      (einfo.Ast0.attachable_end,einfo.Ast0.mcode_end)
	| (Ast.BEFOREAFTER(bef,aft),beforeinfo,afterinfo) ->
	    attach_bef bef beforeinfo
	      (einfo.Ast0.attachable_start,einfo.Ast0.mcode_start);
	    attach_aft aft afterinfo
	      (einfo.Ast0.attachable_end,einfo.Ast0.mcode_end)
	| (Ast.NOTHING,_,_) -> ())
    | Ast0.PLUS -> () in
  V0.combiner bind option_default
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    do_nothing do_nothing do_nothing do_nothing
    do_nothing do_nothing do_nothing do_nothing do_nothing do_nothing
    do_nothing do_nothing do_nothing

(* --------------------------------------------------------------------- *)
(* For function declarations.  Can't use the mcode at the root, because that
might be mixed when the function contains ()s, where agglomeration of -s is
not possible. *)

let check_allminus s =
  let donothing r k e = k e in
  let bind x y = x && y in
  let option_default = true in
  let mcode (_,_,_,mc) = match mc with Ast0.MINUS(_) -> true | _ -> false in

  (* special case for disj *)
  let expression r k e =
    match Ast0.unwrap e with
      Ast0.DisjExpr(starter,expr_list,mids,ender) ->
	List.for_all r.V0.combiner_expression expr_list
    | _ -> k e in

  let declaration r k e =
    match Ast0.unwrap e with
      Ast0.DisjDecl(starter,decls,mids,ender) ->
	List.for_all r.V0.combiner_declaration decls
    | _ -> k e in

  let statement r k e =
    match Ast0.unwrap e with
      Ast0.Disj(starter,statement_dots_list,mids,ender) ->
	List.for_all r.V0.combiner_statement_dots statement_dots_list
    | _ -> k e in

  let combiner = 
    V0.combiner bind option_default
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      donothing donothing donothing donothing
      donothing expression donothing donothing donothing declaration
      statement donothing donothing in
  combiner.V0.combiner_statement s
    
(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
    
let get_option fn = function
    None -> None
  | Some x -> Some (fn x)
	
(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
(* Mcode *)
	
let convert_info info =
  { Ast.line = info.Ast0.line_start; Ast.column = info.Ast0.column }

let convert_mcodekind = function
    Ast0.MINUS(replacements) ->
      let (replacements,_) = !replacements in 
      Ast.MINUS(Ast.NoPos,replacements)
  | Ast0.PLUS -> Ast.PLUS
  | Ast0.CONTEXT(befaft) ->
      let (befaft,_,_) = !befaft in Ast.CONTEXT(Ast.NoPos,befaft)
  | Ast0.MIXED(_) -> failwith "not possible for mcode"

let mcode(term,_,info,mcodekind) =
  (term,convert_info info,convert_mcodekind mcodekind)

(* --------------------------------------------------------------------- *)
(* Dots *)

let rewrap ast0 ast =
  (ast, (Ast0.get_info ast0).Ast0.line_start, [], [], [], Ast.NoDots)
let tokenwrap (_,info,_) ast = (ast, info.Ast.line, [], [], [], Ast.NoDots)

let dots fn d =
  rewrap d
    (match Ast0.unwrap d with
      Ast0.DOTS(x) -> Ast.DOTS(List.map fn x)
    | Ast0.CIRCLES(x) -> Ast.CIRCLES(List.map fn x)
    | Ast0.STARS(x) -> Ast.STARS(List.map fn x))

(* --------------------------------------------------------------------- *)
(* Identifier *)

let rec ident i =
  rewrap i
    (match Ast0.unwrap i with
      Ast0.Id(name) -> Ast.Id(mcode name)
    | Ast0.MetaId(name,_) -> Ast.MetaId(mcode name,true,false)
    | Ast0.MetaFunc(name,_) -> Ast.MetaFunc(mcode name,true,false)
    | Ast0.MetaLocalFunc(name,_) ->
	Ast.MetaLocalFunc(mcode name,true,false)
    | Ast0.OptIdent(id) -> Ast.OptIdent(ident id)
    | Ast0.UniqueIdent(id) -> Ast.UniqueIdent(ident id)
    | Ast0.MultiIdent(id) -> Ast.MultiIdent(ident id))

(* --------------------------------------------------------------------- *)
(* Expression *)

let rec expression e =
  rewrap e
    (match Ast0.unwrap e with
      Ast0.Ident(id) -> Ast.Ident(ident id)
    | Ast0.Constant(const) ->
	Ast.Constant(mcode const)
    | Ast0.FunCall(fn,lp,args,rp) ->
	let fn = expression fn in
	let lp = mcode lp in
	let args = dots expression args in
	let rp = mcode rp in
	Ast.FunCall(fn,lp,args,rp)
    | Ast0.Assignment(left,op,right) ->
	Ast.Assignment(expression left,mcode op,expression right)
    | Ast0.CondExpr(exp1,why,exp2,colon,exp3) ->
	let exp1 = expression exp1 in
	let why = mcode why in
	let exp2 = get_option expression exp2 in
	let colon = mcode colon in
	let exp3 = expression exp3 in
	Ast.CondExpr(exp1,why,exp2,colon,exp3)
    | Ast0.Postfix(exp,op) ->
	Ast.Postfix(expression exp,mcode op)
    | Ast0.Infix(exp,op) ->
	Ast.Infix(expression exp,mcode op)
    | Ast0.Unary(exp,op) ->
	Ast.Unary(expression exp,mcode op)
    | Ast0.Binary(left,op,right) ->
	Ast.Binary(expression left,mcode op,expression right)
    | Ast0.Paren(lp,exp,rp) ->
	Ast.Paren(mcode lp,expression exp,mcode rp)
    | Ast0.ArrayAccess(exp1,lb,exp2,rb) ->
	Ast.ArrayAccess(expression exp1,mcode lb,expression exp2,mcode rb)
    | Ast0.RecordAccess(exp,pt,field) ->
	Ast.RecordAccess(expression exp,mcode pt,ident field)
    | Ast0.RecordPtAccess(exp,ar,field) ->
	Ast.RecordPtAccess(expression exp,mcode ar,ident field)
    | Ast0.Cast(lp,ty,rp,exp) ->
	Ast.Cast(mcode lp,typeC ty,mcode rp,expression exp)
    | Ast0.SizeOfExpr(szf,exp) ->
	Ast.SizeOfExpr(mcode szf,expression exp)
    | Ast0.SizeOfType(szf,lp,ty,rp) ->
	Ast.SizeOfType(mcode szf, mcode lp,typeC ty,mcode rp)
    | Ast0.TypeExp(ty) -> Ast.TypeExp(typeC ty)
    | Ast0.MetaConst(name,ty,_) ->
	Ast.MetaConst(mcode name,true,ty,false)
    | Ast0.MetaErr(name,_)  -> Ast.MetaErr(mcode name,true,false)
    | Ast0.MetaExpr(name,ty,_)  ->
	Ast.MetaExpr(mcode name,true,ty,false)
    | Ast0.MetaExprList(name,_) ->
	Ast.MetaExprList(mcode name,true,false)
    | Ast0.EComma(cm)         -> Ast.EComma(mcode cm)
    | Ast0.DisjExpr(_,exps,_,_)     -> Ast.DisjExpr(List.map expression exps)
    | Ast0.NestExpr(_,exp_dots,_,whencode) ->
	let whencode = get_option expression whencode in
	Ast.NestExpr(dots expression exp_dots,whencode)
    | Ast0.Edots(dots,whencode) ->
	let dots = mcode dots in
	let whencode = get_option expression whencode in
	Ast.Edots(dots,whencode)
    | Ast0.Ecircles(dots,whencode) ->
	let dots = mcode dots in
	let whencode = get_option expression whencode in
	Ast.Ecircles(dots,whencode)
    | Ast0.Estars(dots,whencode) ->
	let dots = mcode dots in
	let whencode = get_option expression whencode in
	Ast.Estars(dots,whencode)
    | Ast0.OptExp(exp) -> Ast.OptExp(expression exp)
    | Ast0.UniqueExp(exp) -> Ast.UniqueExp(expression exp)
    | Ast0.MultiExp(exp) -> Ast.MultiExp(expression exp))

and expression_dots ed = dots expression ed
  
(* --------------------------------------------------------------------- *)
(* Types *)

and typeC t =
  rewrap t
    (match Ast0.unwrap t with
      Ast0.ConstVol(cv,ty) -> Ast.Type(Some (mcode cv),base_typeC ty)
    | Ast0.BaseType(ty,sign) ->
	Ast.Type(None,rewrap t(Ast.BaseType(mcode ty,get_option mcode sign)))
    | Ast0.Pointer(ty,star) ->
	Ast.Type
	  (None,rewrap t(Ast.Pointer(typeC ty,mcode star)))
    | Ast0.Array(ty,lb,size,rb) ->
	Ast.Type(None,
		 rewrap t
		   (Ast.Array(typeC ty,mcode lb,get_option expression size,
			      mcode rb)))
    | Ast0.StructUnionName(kind,name) ->
	Ast.Type(None,rewrap t (Ast.StructUnionName(mcode kind,ident name)))
    | Ast0.StructUnionDef(kind,name,lb,decls,rb) ->
	Ast.Type(None,
		 rewrap t
		   (Ast.StructUnionDef(mcode kind,ident name,mcode lb,
				       List.map declaration decls,mcode rb)))
    | Ast0.TypeName(name) -> Ast.Type(None,rewrap t (Ast.TypeName(mcode name)))
    | Ast0.MetaType(name,_) ->
	Ast.Type(None,rewrap t (Ast.MetaType(mcode name,true,false)))
    | Ast0.OptType(ty) -> Ast.OptType(typeC ty)
    | Ast0.UniqueType(ty) -> Ast.UniqueType(typeC ty)
    | Ast0.MultiType(ty) -> Ast.MultiType(typeC ty))
    
and base_typeC t =
  rewrap t
    (match Ast0.unwrap t with
      Ast0.BaseType(ty,sign) ->
	Ast.BaseType(mcode ty,get_option mcode sign)
    | Ast0.Pointer(ty,star) -> Ast.Pointer(typeC ty,mcode star)
    | Ast0.Array(ty,lb,size,rb) ->
	Ast.Array(typeC ty,mcode lb,get_option expression size,mcode rb)
    | Ast0.StructUnionName(kind,name) ->
	Ast.StructUnionName(mcode kind,ident name)
    | Ast0.StructUnionDef(kind,name,lb,decls,rb) ->
	Ast.StructUnionDef(mcode kind,ident name,
			   mcode lb,List.map declaration decls,mcode rb)
    | Ast0.TypeName(name) -> Ast.TypeName(mcode name)
    | Ast0.MetaType(name,_) -> Ast.MetaType(mcode name,true,false)
    | _ -> failwith "unexpected type")
    
(* --------------------------------------------------------------------- *)
(* Variable declaration *)
(* Even if the Cocci program specifies a list of declarations, they are
   split out into multiple declarations of a single variable each. *)
    
and declaration d =
  rewrap d
    (match Ast0.unwrap d with
      Ast0.Init(stg,ty,id,eq,ini,sem) ->
	let stg = get_option mcode stg in
	let ty = typeC ty in
	let id = ident id in
	let eq = mcode eq in
	let ini = initialiser ini in
	let sem = mcode sem in
	Ast.Init(stg,ty,id,eq,ini,sem)
    | Ast0.UnInit(stg,ty,id,sem) ->
	let stg = get_option mcode stg in
	Ast.UnInit(stg,typeC ty,ident id,mcode sem)
    | Ast0.TyDecl(ty,sem) -> Ast.TyDecl(typeC ty,mcode sem)
    | Ast0.DisjDecl(_,decls,_,_) -> Ast.DisjDecl(List.map declaration decls)
    | Ast0.OptDecl(decl) -> Ast.OptDecl(declaration decl)
    | Ast0.UniqueDecl(decl) -> Ast.UniqueDecl(declaration decl)
    | Ast0.MultiDecl(decl) -> Ast.MultiDecl(declaration decl))

(* --------------------------------------------------------------------- *)
(* Initialiser *)

and strip_idots initlist =
  match Ast0.unwrap initlist with
    Ast0.DOTS(x) ->
      let (whencode,init) = 
	List.fold_left
	  (function (prevwhen,previnit) ->
	    function cur ->
	      match Ast0.unwrap cur with
		Ast0.Idots(dots,Some whencode) ->
		  (whencode :: prevwhen, previnit)
	      | Ast0.Idots(dots,None) -> (prevwhen,previnit)
	      | _ -> (prevwhen, cur :: previnit))
	  ([],[]) x in
      (List.rev whencode, List.rev init)
  | Ast0.CIRCLES(x) | Ast0.STARS(x) -> failwith "not possible for an initlist"

and initialiser i =
  rewrap i
    (match Ast0.unwrap i with
      Ast0.InitExpr(exp) -> Ast.InitExpr(expression exp)
    | Ast0.InitList(lb,initlist,rb) ->
	let (whencode,initlist) = strip_idots initlist in
	Ast.InitList(mcode lb,List.map initialiser initlist,mcode rb,
		     List.map initialiser whencode)
    | Ast0.InitGccDotName(dot,name,eq,ini) ->
	Ast.InitGccDotName(mcode dot,ident name,mcode eq,initialiser ini)
    | Ast0.InitGccName(name,eq,ini) ->
	Ast.InitGccName(ident name,mcode eq,initialiser ini)
    | Ast0.InitGccIndex(lb,exp,rb,eq,ini) ->
	Ast.InitGccIndex(mcode lb,expression exp,mcode rb,mcode eq,
			  initialiser ini)
    | Ast0.InitGccRange(lb,exp1,dots,exp2,rb,eq,ini) ->
	Ast.InitGccRange(mcode lb,expression exp1,mcode dots,
			  expression exp2,mcode rb,mcode eq,initialiser ini)
    | Ast0.Idots(_,_) -> failwith "Idots should have been removed"
    | Ast0.OptIni(ini) -> Ast.OptIni(initialiser ini)
    | Ast0.UniqueIni(ini) -> Ast.UniqueIni(initialiser ini)
    | Ast0.MultiIni(ini) -> Ast.MultiIni(initialiser ini))

let initialiser_dots = dots initialiser
    
(* --------------------------------------------------------------------- *)
(* Parameter *)
    
let rec parameterTypeDef p =
  rewrap p
    (match Ast0.unwrap p with
      Ast0.VoidParam(ty) -> Ast.VoidParam(typeC ty)
    | Ast0.Param(id,ty) -> Ast.Param(ident id,typeC ty)
    | Ast0.MetaParam(name,_) -> Ast.MetaParam(mcode name,true,false)
    | Ast0.MetaParamList(name,_) -> Ast.MetaParamList(mcode name,true,false)
    | Ast0.PComma(cm) -> Ast.PComma(mcode cm)
    | Ast0.Pdots(dots) -> Ast.Pdots(mcode dots)
    | Ast0.Pcircles(dots) -> Ast.Pcircles(mcode dots)
    | Ast0.OptParam(param) -> Ast.OptParam(parameterTypeDef param)
    | Ast0.UniqueParam(param) -> Ast.UniqueParam(parameterTypeDef param))

let parameter_list = dots parameterTypeDef

(* --------------------------------------------------------------------- *)
(* CPP code *)

let define_body m =
  rewrap m
    (match Ast0.unwrap m with
      Ast0.DMetaId(name,_) -> Ast.DMetaId(mcode name,true)
    | Ast0.Ddots(dots) -> Ast.Ddots(mcode dots))

(* --------------------------------------------------------------------- *)
(* Top-level code *)

let ctr = ref 0
let get_ctr _ =
  let c = !ctr in
  ctr := !ctr + 1;
  c

let rec statement s =
  let rec statement seqible s =
    let rewrap ast0 ast =
      (ast, (Ast0.get_info ast0).Ast0.line_start, [], [], [],
       match Ast0.get_dots_bef_aft s with
	 Ast0.NoDots -> Ast.NoDots
       | Ast0.BetweenDots s ->
	   Ast.BetweenDots (statement seqible s,get_ctr())) in
    rewrap s
      (match Ast0.unwrap s with
	Ast0.Decl((_,bef),decl) ->
	  Ast.Atomic(rewrap s
		       (Ast.Decl(convert_mcodekind bef,declaration decl)))
      | Ast0.Seq(lbrace,body,rbrace) -> 
	  let lbrace = mcode lbrace in
	  let (decls,dots,body) = separate_decls seqible body in
	  let rbrace = mcode rbrace in
	  Ast.Seq(tokenwrap lbrace (Ast.SeqStart(lbrace)),decls,dots,body,
		  tokenwrap rbrace (Ast.SeqEnd(rbrace)))
      | Ast0.ExprStatement(exp,sem) ->
	  Ast.Atomic(rewrap s(Ast.ExprStatement(expression exp,mcode sem)))
      | Ast0.IfThen(iff,lp,exp,rp,branch,(_,aft)) ->
	  Ast.IfThen
	    (rewrap s
	       (Ast.IfHeader(mcode iff,mcode lp,expression exp,mcode rp)),
	     statement Ast.NotSequencible branch,convert_mcodekind aft)
      | Ast0.IfThenElse(iff,lp,exp,rp,branch1,els,branch2,(_,aft)) ->
	  let els = mcode els in
	  Ast.IfThenElse
	    (rewrap s
	       (Ast.IfHeader(mcode iff,mcode lp,expression exp,mcode rp)),
	     statement Ast.NotSequencible branch1,
	     tokenwrap els (Ast.Else(els)),
	     statement Ast.NotSequencible branch2,
	     convert_mcodekind aft)
      | Ast0.While(wh,lp,exp,rp,body,(_,aft)) ->
	  Ast.While(rewrap s
		      (Ast.WhileHeader
			 (mcode wh,mcode lp,expression exp,mcode rp)),
		    statement Ast.NotSequencible body,convert_mcodekind aft)
      | Ast0.Do(d,body,wh,lp,exp,rp,sem) ->
	  let wh = mcode wh in
	  Ast.Do(rewrap s (Ast.DoHeader(mcode d)),
		 statement Ast.NotSequencible body,
		 tokenwrap wh
		   (Ast.WhileTail(wh,mcode lp,expression exp,mcode rp,
				  mcode sem)))
      | Ast0.For(fr,lp,exp1,sem1,exp2,sem2,exp3,rp,body,(_,aft)) ->
	  let fr = mcode fr in
	  let lp = mcode lp in
	  let exp1 = get_option expression exp1 in
	  let sem1 = mcode sem1 in
	  let exp2 = get_option expression exp2 in
	  let sem2= mcode sem2 in
	  let exp3 = get_option expression exp3 in
	  let rp = mcode rp in
	  let body = statement Ast.NotSequencible body in
	  Ast.For(rewrap s (Ast.ForHeader(fr,lp,exp1,sem1,exp2,sem2,exp3,rp)),
		  body,convert_mcodekind aft)
      |	Ast0.Switch(switch,lp,exp,rp,lb,cases,rb) ->
	  let switch = mcode switch in
	  let lp = mcode lp in
	  let exp = expression exp in
	  let rp = mcode rp in
	  let lb = mcode lb in
	  let cases = List.map case_line cases in
	  let rb = mcode rb in
	  Ast.Switch(rewrap s (Ast.SwitchHeader(switch,lp,exp,rp)),
		     tokenwrap lb (Ast.SeqStart(lb)),cases,
		     tokenwrap rb (Ast.SeqEnd(rb)))
      | Ast0.Break(br,sem) ->
	  Ast.Atomic(rewrap s (Ast.Break(mcode br,mcode sem)))
      | Ast0.Continue(cont,sem) ->
	  Ast.Atomic(rewrap s (Ast.Continue(mcode cont,mcode sem)))
      | Ast0.Return(ret,sem) ->
	  Ast.Atomic(rewrap s (Ast.Return(mcode ret,mcode sem)))
      | Ast0.ReturnExpr(ret,exp,sem) ->
	  Ast.Atomic
	    (rewrap s (Ast.ReturnExpr(mcode ret,expression exp,mcode sem)))
      | Ast0.MetaStmt(name,_) ->
	  Ast.Atomic(rewrap s (Ast.MetaStmt(mcode name,true,seqible,false)))
      | Ast0.MetaStmtList(name,_) ->
	  Ast.Atomic(rewrap s (Ast.MetaStmtList(mcode name,true,false)))
      | Ast0.Exp(exp) ->
	  Ast.Atomic(rewrap s (Ast.Exp(expression exp)))
      | Ast0.Ty(ty) ->
	  Ast.Atomic(rewrap s (Ast.Ty(typeC ty)))
      | Ast0.Disj(_,rule_elem_dots_list,_,_) ->
	  Ast.Disj(List.map (function x -> statement_dots seqible x)
		     rule_elem_dots_list)
      | Ast0.Nest(_,rule_elem_dots,_,whencode) ->
	  Ast.Nest
	    (statement_dots Ast.Sequencible rule_elem_dots,
	     (match whencode with
	       None -> Ast.NoWhen
	     | Some x -> Ast.WhenNot (statement_dots Ast.Sequencible x)),
	     [])
      | Ast0.Dots(d,whn) ->
	  let d = mcode d in
	  let whn =
	    whencode (statement_dots Ast.Sequencible)
	      (statement Ast.NotSequencible) whn in
	  Ast.Dots(d,whn,[])
      | Ast0.Circles(d,whn) ->
	  let d = mcode d in
	  let whn =
	    whencode (statement_dots Ast.Sequencible)
	      (statement Ast.NotSequencible) whn in
	  Ast.Circles(d,whn,[])
      | Ast0.Stars(d,whn) ->
	  let d = mcode d in
	  let whn =
	    whencode (statement_dots Ast.Sequencible)
	      (statement Ast.NotSequencible) whn in
	  Ast.Stars(d,whn,[])
      | Ast0.FunDecl((_,bef),stg,ty,name,lp,params,rp,lbrace,body,rbrace) ->
	  let stg = get_option mcode stg in
	  let ty = get_option typeC ty in
	  let name = ident name in
	  let lp = mcode lp in
	  let params = parameter_list params in
	  let rp = mcode rp in
	  let lbrace = mcode lbrace in
	  let (decls,dots,body) = separate_decls seqible body in
	  let rbrace = mcode rbrace in
	  let allminus = check_allminus s in
	  Ast.FunDecl(rewrap s
			(Ast.FunHeader(convert_mcodekind bef,
				       allminus,stg,ty,name,lp,params,rp)),
		      tokenwrap lbrace (Ast.SeqStart(lbrace)),
		      decls,dots,body,
		      tokenwrap rbrace (Ast.SeqEnd(rbrace)))
      |	Ast0.Include(inc,str) ->
	  Ast.Atomic(rewrap s (Ast.Include(mcode inc,mcode str)))
      | Ast0.Define(def,id,body) ->
	  Ast.Atomic
	    (rewrap s (Ast.Define(mcode def,ident id,define_body body)))
      | Ast0.OptStm(stm) -> Ast.OptStm(statement seqible stm)
      | Ast0.UniqueStm(stm) -> Ast.UniqueStm(statement seqible stm)
      | Ast0.MultiStm(stm) -> Ast.MultiStm(statement seqible stm))

  and whencode notfn alwaysfn = function
      Ast0.NoWhen -> Ast.NoWhen
    | Ast0.WhenNot a -> Ast.WhenNot (notfn a)
    | Ast0.WhenAlways a -> Ast.WhenAlways (alwaysfn a)

  and process_list seqible = function
      [] -> []
    | x::rest ->
	(match Ast0.unwrap x with
	  Ast0.Dots(_,_) | Ast0.Nest(_) ->
	    (statement seqible x)::
	    (process_list (Ast.SequencibleAfterDots []) rest)
	| _ -> (statement seqible x)::(process_list Ast.Sequencible rest))

  and statement_dots seqible d =
    rewrap d
      (match Ast0.unwrap d with
	Ast0.DOTS(x) -> Ast.DOTS(process_list seqible x)
      | Ast0.CIRCLES(x) -> Ast.CIRCLES(process_list seqible x)
      | Ast0.STARS(x) -> Ast.STARS(process_list seqible x))

  and separate_decls seqible d =
    let rec collect_decls = function
	[] -> ([],false,[])
      | (x::xs) as l ->
	  (match Ast0.unwrap x with
	    Ast0.Decl(_) ->
	      let (decls,dots,other) = collect_decls xs in
	      (x :: decls,dots,other)
	  | Ast0.Dots(_,_) | Ast0.Nest(_,_,_,_) ->
	      let (decls,dots,other) = collect_decls xs in
	      (match decls with
		[] -> ([],true,x::other)
	      | _ -> (x :: decls,dots,other))
	  | Ast0.Disj(starter,stmt_dots_list,mids,ender) ->
	      let disjs = List.map collect_dot_decls stmt_dots_list in
	      let all_decls = List.for_all (function (_,_,s) -> s=[]) disjs in
	      let all_dots = List.for_all (function (_,d,_) -> d) disjs in
	      let all_not_dots =
		List.for_all (function (_,d,_) -> not d) disjs in
	      let all_stmts = List.for_all (function (d,_,_) -> d=[]) disjs in
	      if all_decls
	      then
		let (decls,dots,other) = collect_decls xs in
		(x :: decls,dots,other)
	      else
		if all_stmts && all_dots
		then ([],true,l)
		else
		  if all_stmts && all_not_dots
		  then ([],false,l)
		  else
		    begin
		      Printf.printf
			"warning: mixes stmts and decls, not skipping initial decls\n";
		      ([],true,l)
		    end
	  | _ -> ([],false,l))

    and collect_dot_decls d =
      match Ast0.unwrap d with
	Ast0.DOTS(x) -> collect_decls x
      | Ast0.CIRCLES(x) -> collect_decls x
      | Ast0.STARS(x) -> collect_decls x in

    let process l d fn =
      let (decls,dots,other) = collect_decls l in
      (rewrap d (fn (List.map (statement seqible) decls)), dots,
       rewrap d (fn (process_list seqible other))) in
    match Ast0.unwrap d with
      Ast0.DOTS(x) -> process x d (function x -> Ast.DOTS x)
    | Ast0.CIRCLES(x) -> process x d (function x -> Ast.CIRCLES x)
    | Ast0.STARS(x) -> process x d (function x -> Ast.STARS x) in

  statement Ast.Sequencible s
    

and option_to_list = function
    Some x -> [x]
  | None -> []

and case_line c =
  rewrap c
    (match Ast0.unwrap c with
      Ast0.Default(def,colon,code) ->
	let def = mcode def in
	let colon = mcode colon in
	let code = dots statement code in
	Ast.CaseLine(rewrap c (Ast.Default(def,colon)),code)
    | Ast0.Case(case,exp,colon,code) ->
	let case = mcode case in
	let exp = expression exp in
	let colon = mcode colon in
	let code = dots statement code in
	Ast.CaseLine(rewrap c (Ast.Case(case,exp,colon)),code)
    | Ast0.OptCase(case) -> Ast.OptCase(case_line case))

let statement_dots l = dots statement l
    
(* --------------------------------------------------------------------- *)
(* Function declaration *)
    
let top_level t =
  rewrap t
    (match Ast0.unwrap t with
      Ast0.FILEINFO(old_file,new_file) ->
	Ast.FILEINFO(mcode old_file,mcode new_file)
    | Ast0.DECL(stmt) -> Ast.DECL(statement stmt)
    | Ast0.CODE(rule_elem_dots) ->
	Ast.CODE(dots statement rule_elem_dots)
    | Ast0.ERRORWORDS(exps) -> Ast.ERRORWORDS(List.map expression exps)
    | Ast0.OTHER(_) -> failwith "eliminated by top_level")

(* --------------------------------------------------------------------- *)
(* Entry point for minus code *)

let ast0toast x =
  List.iter inline_mcodes.V0.combiner_top_level x;
  List.map top_level x
