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
	    if startinfo.Ast0.tline_end = endinfo.Ast0.tline_start
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
		      mreplacements :=
			concat bef beforeinfo mrepl tokeninfo
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
    do_nothing do_nothing do_nothing
    do_nothing do_nothing do_nothing do_nothing do_nothing
    do_nothing do_nothing
    
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
      let (replacements,_) = !replacements in Ast.MINUS(replacements)
  | Ast0.PLUS -> Ast.PLUS
  | Ast0.CONTEXT(befaft) ->
      let (befaft,_,_) = !befaft in Ast.CONTEXT(befaft)
  | Ast0.MIXED(_) -> failwith "not possible for mcode"

let mcode(term,_,info,mcodekind) =
  (term,convert_info info,convert_mcodekind mcodekind)

(* --------------------------------------------------------------------- *)
(* Dots *)

let rewrap ast0 ast = (ast, (Ast0.get_info ast0).Ast0.line_start, [])
let tokenwrap (_,info,_) ast = (ast, info.Ast.line, [])

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
    | Ast0.MetaId(name) -> Ast.MetaId(mcode name)
    | Ast0.MetaFunc(name) -> Ast.MetaFunc(mcode name)
    | Ast0.MetaLocalFunc(name) -> Ast.MetaLocalFunc(mcode name)
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
    | Ast0.MetaConst(name,ty)  -> Ast.MetaConst(mcode name,ty)
    | Ast0.MetaErr(name)  -> Ast.MetaErr(mcode name)
    | Ast0.MetaExpr(name,ty)  -> Ast.MetaExpr(mcode name,ty)
    | Ast0.MetaExprList(name) -> Ast.MetaExprList(mcode name)
    | Ast0.EComma(cm)         -> Ast.EComma(mcode cm)
    | Ast0.DisjExpr(_,exps,_)     -> Ast.DisjExpr(List.map expression exps)
    | Ast0.NestExpr(_,exp_dots,_) -> Ast.NestExpr(dots expression exp_dots)
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
    | Ast0.StructUnionName(name,kind) ->
	Ast.Type(None,rewrap t (Ast.StructUnionName(mcode name,mcode kind)))
    | Ast0.TypeName(name) -> Ast.Type(None,rewrap t (Ast.TypeName(mcode name)))
    | Ast0.MetaType(name) -> Ast.Type(None,rewrap t (Ast.MetaType(mcode name)))
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
    | Ast0.StructUnionName(name,kind) ->
	Ast.StructUnionName(mcode name,mcode kind)
    | Ast0.TypeName(name) -> Ast.TypeName(mcode name)
    | Ast0.MetaType(name) -> Ast.MetaType(mcode name)
    | _ -> failwith "unexpected type")
    
(* --------------------------------------------------------------------- *)
(* Variable declaration *)
(* Even if the Cocci program specifies a list of declarations, they are
   split out into multiple declarations of a single variable each. *)
    
let rec declaration d =
  rewrap d
    (match Ast0.unwrap d with
      Ast0.Init(ty,id,eq,exp,sem) ->
	let ty = typeC ty in
	let id = ident id in
	let eq = mcode eq in
	let exp = expression exp in
	let sem = mcode sem in
	Ast.Init(ty,id,eq,exp,sem)
    | Ast0.UnInit(ty,id,sem) -> Ast.UnInit(typeC ty,ident id,mcode sem)
    | Ast0.DisjDecl(_,decls,_)     -> Ast.DisjDecl(List.map declaration decls)
    | Ast0.OptDecl(decl) -> Ast.OptDecl(declaration decl)
    | Ast0.UniqueDecl(decl) -> Ast.UniqueDecl(declaration decl)
    | Ast0.MultiDecl(decl) -> Ast.MultiDecl(declaration decl))
    
(* --------------------------------------------------------------------- *)
(* Parameter *)
    
let rec parameterTypeDef p =
  rewrap p
    (match Ast0.unwrap p with
      Ast0.VoidParam(ty) -> Ast.VoidParam(typeC ty)
    | Ast0.Param(id,ty) -> Ast.Param(ident id,typeC ty)
    | Ast0.MetaParam(name) -> Ast.MetaParam(mcode name)
    | Ast0.MetaParamList(name) -> Ast.MetaParamList(mcode name)
    | Ast0.PComma(cm) -> Ast.PComma(mcode cm)
    | Ast0.Pdots(dots) -> Ast.Pdots(mcode dots)
    | Ast0.Pcircles(dots) -> Ast.Pcircles(mcode dots)
    | Ast0.OptParam(param) -> Ast.OptParam(parameterTypeDef param)
    | Ast0.UniqueParam(param) -> Ast.UniqueParam(parameterTypeDef param))

let parameter_list = dots parameterTypeDef

(* --------------------------------------------------------------------- *)
(* Top-level code *)

let rec statement s =
  rewrap s
    (match Ast0.unwrap s with
      Ast0.Decl(decl) -> Ast.Atomic(rewrap s (Ast.Decl(declaration decl)))
    | Ast0.Seq(lbrace,body,rbrace) -> 
	let lbrace = mcode lbrace in
	let (decls,dots,body) = separate_decls body in
	let rbrace = mcode rbrace in
	Ast.Seq(tokenwrap lbrace (Ast.SeqStart(lbrace)),decls,dots,body,
		tokenwrap lbrace (Ast.SeqEnd(rbrace)))
    | Ast0.ExprStatement(exp,sem) ->
	Ast.Atomic(rewrap s(Ast.ExprStatement(expression exp,mcode sem)))
    | Ast0.IfThen(iff,lp,exp,rp,branch) ->
	Ast.IfThen(rewrap s
		     (Ast.IfHeader
			(mcode iff,mcode lp,expression exp,mcode rp)),
		   statement branch)
    | Ast0.IfThenElse(iff,lp,exp,rp,branch1,els,branch2) ->
	let els = mcode els in
	Ast.IfThenElse
	  (rewrap s (Ast.IfHeader(mcode iff,mcode lp,expression exp,mcode rp)),
	   statement branch1,tokenwrap els (Ast.Else(els)), statement branch2)
    | Ast0.While(wh,lp,exp,rp,body) ->
	Ast.While(rewrap s
		    (Ast.WhileHeader
		       (mcode wh,mcode lp,expression exp,mcode rp)),
		  statement body)
    | Ast0.Do(d,body,wh,lp,exp,rp,sem) ->
	let wh = mcode wh in
	Ast.Do(rewrap s (Ast.DoHeader(mcode d)), statement body,
	       tokenwrap wh
		 (Ast.WhileTail(wh,mcode lp,expression exp,mcode rp,
				mcode sem)))
    | Ast0.For(fr,lp,exp1,sem1,exp2,sem2,exp3,rp,body) ->
	let fr = mcode fr in
	let lp = mcode lp in
	let exp1 = get_option expression exp1 in
	let sem1 = mcode sem1 in
	let exp2 = get_option expression exp2 in
	let sem2= mcode sem2 in
	let exp3 = get_option expression exp3 in
	let rp = mcode rp in
	let body = statement body in
	Ast.For(rewrap s (Ast.ForHeader(fr,lp,exp1,sem1,exp2,sem2,exp3,rp)),
		body)
    | Ast0.Return(ret,sem) ->
	Ast.Atomic(rewrap s (Ast.Return(mcode ret,mcode sem)))
    | Ast0.ReturnExpr(ret,exp,sem) ->
	Ast.Atomic
	  (rewrap s (Ast.ReturnExpr(mcode ret,expression exp,mcode sem)))
    | Ast0.MetaStmt(name) ->
	Ast.Atomic(rewrap s (Ast.MetaStmt(mcode name)))
    | Ast0.MetaStmtList(name) ->
	Ast.Atomic(rewrap s (Ast.MetaStmtList(mcode name)))
    | Ast0.Exp(exp) ->
	Ast.Atomic(rewrap s (Ast.Exp(expression exp)))
    | Ast0.Disj(_,rule_elem_dots_list,_) ->
	Ast.Disj(List.map (function x -> dots statement x) rule_elem_dots_list)
    | Ast0.Nest(_,rule_elem_dots,_) ->
	Ast.Nest(dots statement rule_elem_dots,[])
    | Ast0.Dots(d,whencode) ->
	let d = mcode d in
	let whencode = get_option (dots statement) whencode in
	Ast.Dots(d,option_to_list whencode,[])
    | Ast0.Circles(d,whencode) ->
	let d = mcode d in
	let whencode = get_option (dots statement) whencode in
	Ast.Circles(d,option_to_list whencode,[])
    | Ast0.Stars(d,whencode) ->
	let d = mcode d in
	let whencode = get_option (dots statement) whencode in
	Ast.Stars(d,option_to_list whencode,[])
    | Ast0.FunDecl(stg,ty,name,lp,params,rp,lbrace,body,rbrace) ->
	let stg = get_option mcode stg in
	let ty = get_option typeC ty in
	let name = ident name in
	let lp = mcode lp in
	let params = parameter_list params in
	let rp = mcode rp in
	let lbrace = mcode lbrace in
	let (decls,dots,body) = separate_decls body in
	let rbrace = mcode rbrace in
	let allminus =
	  match Ast0.get_mcodekind s with
	    Ast0.MINUS(_) -> true
	  | _ -> false in
	Ast.FunDecl(rewrap s
		      (Ast.FunHeader(allminus,stg,ty,name,lp,params,rp)),
		    tokenwrap lbrace (Ast.SeqStart(lbrace)),
		    decls,dots,body,
		    tokenwrap rbrace (Ast.SeqEnd(rbrace)))
    | Ast0.OptStm(stm) -> Ast.OptStm(statement stm)
    | Ast0.UniqueStm(stm) -> Ast.UniqueStm(statement stm)
    | Ast0.MultiStm(stm) -> Ast.MultiStm(statement stm))

and separate_decls d =
  let rec collect_decls = function
      [] -> ([],false,[])
    | (x::xs) as l ->
	(match Ast0.unwrap x with
	  Ast0.Decl(_) ->
	    let (decls,dots,other) = collect_decls xs in
	    (x :: decls,dots,other)
	| Ast0.Dots(_,_) ->
	    let (decls,dots,other) = collect_decls xs in
	    (match decls with
	      [] -> ([],true,x::other)
	    | _ -> (x :: decls,dots,other))
	| _ -> ([],false,l)) in
  let process l d fn =
    let (decls,dots,other) = collect_decls l in
    (rewrap d (fn (List.map statement decls)), dots,
     rewrap d (fn (List.map statement other))) in
  match Ast0.unwrap d with
    Ast0.DOTS(x) -> process x d (function x -> Ast.DOTS x)
  | Ast0.CIRCLES(x) -> process x d (function x -> Ast.CIRCLES x)
  | Ast0.STARS(x) -> process x d (function x -> Ast.STARS x)

and option_to_list = function
    Some x -> [x]
  | None -> []
    
let statement_dots = dots statement
    
(* --------------------------------------------------------------------- *)
(* Function declaration *)
(* Haven't thought much about arity here... *)
    
let top_level t =
  rewrap t
    (match Ast0.unwrap t with
      Ast0.DECL(decl) -> Ast.DECL(declaration decl)
    | Ast0.INCLUDE(inc,s) -> Ast.INCLUDE(mcode inc,mcode s)
    | Ast0.FILEINFO(old_file,new_file) ->
	Ast.FILEINFO(mcode old_file,mcode new_file)
    | Ast0.FUNCTION(stmt) -> Ast.FUNCTION(statement stmt)
    | Ast0.CODE(rule_elem_dots) -> Ast.CODE(dots statement rule_elem_dots)
    | Ast0.ERRORWORDS(exps) -> Ast.ERRORWORDS(List.map expression exps)
    | Ast0.OTHER(_) -> failwith "eliminated by top_level")

(* --------------------------------------------------------------------- *)
(* Entry point for minus code *)

let ast0toast x =
  List.iter inline_mcodes.V0.combiner_top_level x;
  List.map top_level x
