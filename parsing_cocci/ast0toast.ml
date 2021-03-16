(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

(* Arities matter for the minus slice, but not for the plus slice. *)

(* + only allowed on code in a nest (in_nest = true).  ? only allowed on
rule_elems, and on subterms if the context is ? also. *)

module Ast0 = Ast0_cocci
module Ast = Ast_cocci
module V0 = Visitor_ast0
module VT0 = Visitor_ast0_types

let unitary = Ast.Unitary

let ctr = ref 0
let get_ctr _ =
  let c = !ctr in
  ctr := !ctr + 1;
  c

(* --------------------------------------------------------------------- *)
(* Move plus tokens from the MINUS and CONTEXT structured nodes to the
corresponding leftmost and rightmost mcodes *)

let inline_mcodes =
  let bind x y = () in
  let option_default = () in
  let mcode _ = () in
  let do_nothing_dir favor_start r k e =
    k e;
    let einfo = Ast0.get_info e in
    match (Ast0.get_mcodekind e) with
      Ast0.MINUS(replacements) ->
	(match !replacements with
	  (Ast.NOREPLACEMENT,_) -> ()
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
	    let (attachable_favored,mcode_favored,
		 attachable_unfavored,mcode_unfavored) =
	      if favor_start
	      then
		(einfo.Ast0.attachable_start,
		 einfo.Ast0.mcode_start,
		 einfo.Ast0.attachable_end,
		 einfo.Ast0.mcode_end)
	      else
		(einfo.Ast0.attachable_end,
		 einfo.Ast0.mcode_end,
		 einfo.Ast0.attachable_start,
		 einfo.Ast0.mcode_start) in
	    if not (minus_try(attachable_favored,mcode_favored) ||
    	            minus_try(attachable_unfavored,mcode_unfavored))
	    then
	      failwith "minus tree should not have bad code on both sides")
    | Ast0.CONTEXT(befaft)
    | Ast0.MIXED(befaft) ->
	let concat starter startinfo ender endinfo =
	  let lst =
	    match (starter,ender) with
	      ([],_) -> ender
	    | (_,[]) -> starter
	    | _ ->
		if startinfo.Ast0.tline_end = endinfo.Ast0.tline_start
		then (* put them in the same inner list *)
		  let last = List.hd (List.rev starter) in
		  let butlast = List.rev(List.tl(List.rev starter)) in
		  butlast @ (last@(List.hd ender)) :: (List.tl ender)
		else starter @ ender in
	  (lst,
	   {endinfo with Ast0.tline_start = startinfo.Ast0.tline_start}) in
	let attach_bef bef beforeinfo befit = function
	    (true,mcl) ->
	      List.iter
		(function
		    Ast0.MINUS(mreplacements) ->
		      (match !mreplacements with
			(Ast.NOREPLACEMENT,tokeninfo) ->
			  mreplacements :=
			    (Ast.REPLACEMENT(bef,befit),beforeinfo)
		      |	(Ast.REPLACEMENT(anythings,it),tokeninfo) ->
			  let (newbef,newinfo) =
			    concat bef beforeinfo anythings tokeninfo in
			  let it = Ast.lub_count befit it in
			  mreplacements :=
			    (Ast.REPLACEMENT(newbef,it),newinfo))
		  | Ast0.CONTEXT(mbefaft) ->
		      (match !mbefaft with
			(Ast.BEFORE(mbef,it),mbeforeinfo,a) ->
			  let (newbef,newinfo) =
			    concat bef beforeinfo mbef mbeforeinfo in
			  let it = Ast.lub_count befit it in
			  mbefaft := (Ast.BEFORE(newbef,it),newinfo,a)
		      | (Ast.AFTER(maft,it),_,a) ->
			  let it = Ast.lub_count befit it in
			  mbefaft :=
			    (Ast.BEFOREAFTER(bef,maft,it),beforeinfo,a)
		      | (Ast.BEFOREAFTER(mbef,maft,it),mbeforeinfo,a) ->
			  let (newbef,newinfo) =
			    concat bef beforeinfo mbef mbeforeinfo in
			  let it = Ast.lub_count befit it in
			  mbefaft :=
			    (Ast.BEFOREAFTER(newbef,maft,it),newinfo,a)
		      | (Ast.NOTHING,_,a) ->
			  mbefaft :=
			    (Ast.BEFORE(bef,befit),beforeinfo,a))
		  |	_ -> failwith "unexpected annotation")
		mcl
	  | _ ->
	      Printf.printf "before %s\n" (Dumper.dump bef);
	      failwith
		"context tree should not have bad code before" in
	let attach_aft aft afterinfo aftit = function
	    (true,mcl) ->
	      List.iter
		(function
		    Ast0.MINUS(mreplacements) ->
		      (match !mreplacements with
			(Ast.NOREPLACEMENT,tokeninfo) ->
			  mreplacements :=
			    (Ast.REPLACEMENT(aft,aftit),afterinfo)
		      |	(Ast.REPLACEMENT(anythings,it),tokeninfo) ->
			  let (newaft,newinfo) =
			    concat anythings tokeninfo aft afterinfo in
			  let it = Ast.lub_count aftit it in
			  mreplacements :=
			    (Ast.REPLACEMENT(newaft,it),newinfo))
		  | Ast0.CONTEXT(mbefaft) ->
		      (match !mbefaft with
			(Ast.BEFORE(mbef,it),b,_) ->
			  let it = Ast.lub_count aftit it in
			  mbefaft :=
			    (Ast.BEFOREAFTER(mbef,aft,it),b,afterinfo)
		      | (Ast.AFTER(maft,it),b,mafterinfo) ->
			  let (newaft,newinfo) =
			    concat maft mafterinfo aft afterinfo in
			  let it = Ast.lub_count aftit it in
			  mbefaft := (Ast.AFTER(newaft,it),b,newinfo)
		      | (Ast.BEFOREAFTER(mbef,maft,it),b,mafterinfo) ->
			  let (newaft,newinfo) =
			    concat maft mafterinfo aft afterinfo in
			  let it = Ast.lub_count aftit it in
			  mbefaft :=
			    (Ast.BEFOREAFTER(mbef,newaft,it),b,newinfo)
		      | (Ast.NOTHING,b,_) ->
			  mbefaft := (Ast.AFTER(aft,aftit),b,afterinfo))
		  |	_ -> failwith "unexpected annotation")
		mcl
	  | _ ->
	      failwith
		"context tree should not have bad code after" in
	(match !befaft with
	  (Ast.BEFORE(bef,it),beforeinfo,_) ->
	    attach_bef bef beforeinfo it
	      (einfo.Ast0.attachable_start,einfo.Ast0.mcode_start)
	| (Ast.AFTER(aft,it),_,afterinfo) ->
	    attach_aft aft afterinfo it
	      (einfo.Ast0.attachable_end,einfo.Ast0.mcode_end)
	| (Ast.BEFOREAFTER(bef,aft,it),beforeinfo,afterinfo) ->
	    attach_bef bef beforeinfo it
	      (einfo.Ast0.attachable_start,einfo.Ast0.mcode_start);
	    attach_aft aft afterinfo it
	      (einfo.Ast0.attachable_end,einfo.Ast0.mcode_end)
	| (Ast.NOTHING,_,_) -> ())
    | Ast0.PLUS _ -> () in
  let do_nothing r k e = do_nothing_dir true r k e in
  let do_nothing_end r k e = do_nothing_dir false r k e in
  V0.flat_combiner bind option_default
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    mcode mcode
    do_nothing do_nothing do_nothing do_nothing do_nothing do_nothing
    do_nothing do_nothing
    do_nothing do_nothing do_nothing do_nothing do_nothing do_nothing
    do_nothing do_nothing do_nothing_end do_nothing_end do_nothing do_nothing
    do_nothing do_nothing do_nothing do_nothing do_nothing do_nothing

(* --------------------------------------------------------------------- *)
(* For function declarations.  Can't use the mcode at the root, because that
might be mixed when the function contains ()s, where agglomeration of -s is
not possible. *)

let check_allminus =
  let donothing r k e = k e in
  let bind x y = x && y in
  let option_default = true in
  let mcode (_,_,_,mc,_,_) =
    match mc with
      Ast0.MINUS(r) -> let (plusses,_) = !r in plusses = Ast.NOREPLACEMENT
    | _ -> false in

  (* special case for disj and asExpr etc *)
  let ident r k e =
    match Ast0.unwrap e with
      Ast0.DisjId(starter,id_list,mids,ender)
    | Ast0.ConjId(starter,id_list,mids,ender) ->
	List.for_all r.VT0.combiner_rec_ident id_list
    | Ast0.AsIdent(id,asid) -> k id
    | _ -> k e in

  let expression r k e =
    match Ast0.unwrap e with
      Ast0.DisjExpr(starter,expr_list,mids,ender) (* ignore starter, etc *)
    | Ast0.ConjExpr(starter,expr_list,mids,ender) ->
	List.for_all r.VT0.combiner_rec_expression expr_list
    | Ast0.AsExpr(exp,asexp) -> k exp
    | Ast0.AsSExpr(exp,asstm) -> k exp
    | _ -> k e in

  let declaration r k e =
    match Ast0.unwrap e with
      Ast0.DisjDecl(starter,decls,mids,ender)
    | Ast0.ConjDecl(starter,decls,mids,ender) ->
	List.for_all r.VT0.combiner_rec_declaration decls
    | Ast0.AsDecl(decl,asdecl) -> k decl
    | _ -> k e in

  let field r k e =
    match Ast0.unwrap e with
      Ast0.DisjField(starter,decls,mids,ender)
    | Ast0.ConjField(starter,decls,mids,ender) ->
	List.for_all r.VT0.combiner_rec_field decls
    | _ -> k e in

  let typeC r k e =
    match Ast0.unwrap e with
      Ast0.DisjType(starter,type_list,mids,ender) ->
	List.for_all r.VT0.combiner_rec_typeC type_list
    | Ast0.ConjType(starter,type_list,mids,ender) ->
	List.for_all r.VT0.combiner_rec_typeC type_list
    | Ast0.AsType(ty,asty) -> k ty
    | _ -> k e in

  let initialiser r k e =
    match Ast0.unwrap e with
      Ast0.AsInit(init,asinit) -> k init
    | _ -> k e in

  let statement r k e =
    match Ast0.unwrap e with
      Ast0.Disj(starter,statement_dots_list,mids,ender)
    | Ast0.Conj(starter,statement_dots_list,mids,ender) ->
	List.for_all r.VT0.combiner_rec_statement_dots statement_dots_list
    | Ast0.AsStmt(stmt,asstmt) -> k stmt
    | _ -> k e in

  let case_line r k e =
    match Ast0.unwrap e with
      Ast0.DisjCase(starter,case_lines,mids,ender) ->
	List.for_all r.VT0.combiner_rec_case_line case_lines
    | _ -> k e in

  V0.flat_combiner bind option_default
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    mcode mcode mcode mcode
    donothing donothing donothing donothing donothing donothing donothing
    donothing
    donothing ident expression donothing donothing typeC initialiser donothing
    declaration field donothing statement donothing case_line donothing
    donothing donothing donothing

(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)

let get_option fn = function
    None -> None
  | Some x -> Some (fn x)

(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
(* Mcode *)

let convert_info info =
  let strings_to_s l =
    List.map
      (function (s,info) -> (s,info.Ast0.line_start,info.Ast0.column))
      l in
  { Ast.line = info.Ast0.pos_info.Ast0.line_start;
    Ast.column = info.Ast0.pos_info.Ast0.column;
    Ast.strbef = strings_to_s info.Ast0.strings_before;
    Ast.straft = strings_to_s info.Ast0.strings_after;
    Ast.whitespace = info.Ast0.whitespace;
  }

let convert_mcodekind adj = function
    Ast0.MINUS(replacements) ->
      let (replacements,_) = !replacements in
      Ast.MINUS(Ast.NoPos,[],Ast.ADJ adj,replacements)
  | Ast0.PLUS count -> Ast.PLUS count
  | Ast0.CONTEXT(befaft) ->
      let (befaft,_,_) = !befaft in
      Ast.CONTEXT(Ast.NoPos,befaft)
  | Ast0.MIXED(_) -> failwith "not possible for mcode"

let convert_fake_mcode (_,mc,adj) = convert_mcodekind adj mc

let convert_allminus_mcodekind allminus = function
    Ast0.CONTEXT(befaft) ->
      let (befaft,_,_) = !befaft in
      if allminus
      then
	(match befaft with
	  Ast.NOTHING ->
	    Ast.MINUS(Ast.NoPos,[],Ast.ALLMINUS,Ast.NOREPLACEMENT)
	| Ast.BEFORE(a,ct) | Ast.AFTER(a,ct) ->
	    Ast.MINUS(Ast.NoPos,[],Ast.ALLMINUS,Ast.REPLACEMENT(a,ct))
	| Ast.BEFOREAFTER(b,a,ct) ->
	    Ast.MINUS(Ast.NoPos,[],Ast.ALLMINUS,Ast.REPLACEMENT(b@a,ct)))
      else Ast.CONTEXT(Ast.NoPos,befaft)
  | _ -> failwith "convert_allminus_mcodekind: unexpected mcodekind"

let pos_mcode(term,_,info,mcodekind,pos,adj) =
  (* avoids a recursion problem *)
  (term,convert_info info,convert_mcodekind adj mcodekind,[])

(* Postponed transformer declaration for constraints *)
let constraints' = ref (fun _ -> failwith "unbound constraints")
let constraints c = !constraints' c

let mcode (term,_,info,mcodekind,pos,adj) =
  let pos =
    List.fold_left
      (function prev ->
	function
	    Ast0.MetaPosTag(Ast0.MetaPos(pos,cstr,per)) ->
	      let cstr' = constraints cstr in
	      (Ast.MetaPos(pos_mcode pos,cstr',per,unitary,false))::prev
	  | Ast0.MetaPosTag(Ast0.MetaCom(pos,cstr)) ->
	      let cstr' = constraints cstr in
	      (Ast.MetaCom(pos_mcode pos,cstr',unitary,false))::prev
	  | _ -> prev)
      [] !pos in
  (term,convert_info info,convert_mcodekind adj mcodekind,List.rev pos)

(* --------------------------------------------------------------------- *)
(* Dots *)
let wrap ast line isos =
  {(Ast.make_term ast) with Ast.node_line = line;
    Ast.iso_info = isos}

let rewrap ast0 isos ast =
  wrap ast ((Ast0.get_info ast0).Ast0.pos_info.Ast0.line_start) isos

let no_isos = []

(* no isos on tokens *)
let tokenwrap (_,info,_,_) s ast = wrap ast info.Ast.line no_isos
let iso_tokenwrap (_,info,_,_) s ast iso = wrap ast info.Ast.line iso

let dots fn d = rewrap d no_isos (List.map fn (Ast0.unwrap d))

(* --------------------------------------------------------------------- *)
(* Identifier *)

let rec do_isos l = List.map (function (nm,x) -> (nm,anything x)) l

and ident i =
  rewrap i (do_isos (Ast0.get_iso i))
    (match Ast0.unwrap i with
      Ast0.Id(name) -> Ast.Id(mcode name)
    | Ast0.DisjId(_,id_list,_,_) ->
	Ast.DisjId(List.map ident id_list)
    | Ast0.ConjId(_,id_list,_,_) ->
	Ast.ConjId(List.map ident id_list)
    | Ast0.MetaId(name,cstr,_,_) ->
	let cstr' = constraints cstr in
	Ast.MetaId(mcode name,cstr',unitary,false)
    | Ast0.MetaFunc(name,cstr,_) ->
	let cstr' = constraints cstr in
	Ast.MetaFunc(mcode name,cstr',unitary,false)
    | Ast0.MetaLocalFunc(name,cstr,_) ->
	let cstr' = constraints cstr in
	Ast.MetaLocalFunc(mcode name,cstr',unitary,false)
    | Ast0.AsIdent(id,asid) ->
	Ast.AsIdent(ident id,ident asid)
    | Ast0.OptIdent(id) -> Ast.OptIdent(ident id))

(* --------------------------------------------------------------------- *)
(* Expression *)

and expression e =
  let e1 =
  rewrap e (do_isos (Ast0.get_iso e))
    (match Ast0.unwrap e with
      Ast0.Ident(id) -> Ast.Ident(ident id)
    | Ast0.Constant(const) ->
	Ast.Constant(mcode const)
    | Ast0.StringConstant(lq,str,rq,isWchar) ->
	Ast.StringConstant(mcode lq, dots string_fragment str, mcode rq,isWchar)
    | Ast0.FunCall(fn,lp,args,rp) ->
	let fn = expression fn in
	let lp = mcode lp in
	let args = dots expression args in
	let rp = mcode rp in
	Ast.FunCall(fn,lp,args,rp)
    | Ast0.Assignment(left,op,right,simple) ->
	Ast.Assignment(expression left,assignOp op,expression right,simple)
    | Ast0.Sequence(left,op,right) ->
	Ast.Sequence(expression left,mcode op,expression right)
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
	Ast.Binary(expression left,binaryOp op,expression right)
    | Ast0.Nested(left,op,right) ->
	Ast.Nested(expression left,binaryOp op,expression right)
    | Ast0.Paren(lp,exp,rp) ->
	Ast.Paren(mcode lp,expression exp,mcode rp)
    | Ast0.ArrayAccess(exp1,lb,exp2,rb) ->
	Ast.ArrayAccess(expression exp1,mcode lb,expression exp2,mcode rb)
    | Ast0.RecordAccess(exp,pt,field) ->
	Ast.RecordAccess(expression exp,mcode pt,ident field)
    | Ast0.RecordPtAccess(exp,ar,field) ->
	Ast.RecordPtAccess(expression exp,mcode ar,ident field)
    | Ast0.Cast(lp,ty,attr,rp,exp) ->
	let allminus = check_allminus.VT0.combiner_rec_expression e in
	let attr = List.map attribute attr in
	Ast.Cast(mcode lp,typeC allminus ty,attr,mcode rp,expression exp)
    | Ast0.SizeOfExpr(szf,exp) ->
	Ast.SizeOfExpr(mcode szf,expression exp)
    | Ast0.SizeOfType(szf,lp,ty,rp) ->
	let allminus = check_allminus.VT0.combiner_rec_expression e in
	Ast.SizeOfType(mcode szf, mcode lp,typeC allminus ty,mcode rp)
    | Ast0.TypeExp(ty) ->
	let allminus = check_allminus.VT0.combiner_rec_expression e in
	Ast.TypeExp(typeC allminus ty)
    | Ast0.Constructor(lp,ty,rp,init) ->
	let allminus = check_allminus.VT0.combiner_rec_expression e in
	Ast.Constructor(mcode lp,typeC allminus ty,mcode rp,initialiser init)
    | Ast0.MetaErr(name,cstrts,_)  ->
	Ast.MetaErr(mcode name,constraints cstrts,unitary,false)
    | Ast0.MetaExpr(name,cstrts,ty,form,_,bitfield)  ->
        let ty' = Common.map_option (List.map (typeC false)) ty in
	let bitfield' = Common.map_option do_lenname bitfield in
        Ast.MetaExpr
	  (mcode name, constraints cstrts, unitary, ty', form, false, bitfield')
    | Ast0.MetaExprList(name,lenname,cstr,_) ->
	let cstr' = constraints cstr in
	Ast.MetaExprList (mcode name,do_lenname lenname,cstr',unitary,false)
    | Ast0.AsExpr(expr,asexpr) ->
	Ast.AsExpr(expression expr,expression asexpr)
    | Ast0.AsSExpr(expr,asstm) ->
	let stm =
	  match Ast.unwrap (statement asstm) with
	    Ast.Atomic(re) -> re
	  | _ -> failwith "stmt should be metavar, and thus atomic" in
	Ast.AsSExpr(expression expr,stm)
    | Ast0.EComma(cm)         -> Ast.EComma(mcode cm)
    | Ast0.DisjExpr(_,exps,_,_)     ->
	Ast.DisjExpr(List.map expression exps)
    | Ast0.ConjExpr(_,exps,_,_)     ->
	Ast.ConjExpr(List.map expression exps)
    | Ast0.NestExpr(starter,exp_dots,ender,whencode,multi) ->
	let starter = mcode starter in
	let whencode = get_option (fun (_,_,b) -> expression b) whencode in
	let ender = mcode ender in
	Ast.NestExpr(starter,dots expression exp_dots,ender,whencode,multi)
    | Ast0.Edots(dots,whencode) ->
	let dots = mcode dots in
	let whencode = get_option (fun (_,_,b) -> expression b) whencode in
	Ast.Edots(dots,whencode)
    | Ast0.OptExp(exp) -> Ast.OptExp(expression exp)) in
  if Ast0.get_test_exp e then Ast.set_test_exp e1 else e1

and assignOp op =
  rewrap op no_isos
    (match Ast0.unwrap op with
      Ast0.SimpleAssign op' -> Ast.SimpleAssign (mcode op')
    | Ast0.OpAssign op' -> Ast.OpAssign (mcode op')
    | Ast0.MetaAssign(mv, cstr, _) ->
	let cstr' = constraints cstr in
	Ast.MetaAssign(mcode mv, cstr', unitary, false))

and binaryOp op =
  rewrap op no_isos
    (match Ast0.unwrap op with
      Ast0.Arith op' -> Ast.Arith (mcode op')
    | Ast0.Logical op' -> Ast.Logical (mcode op')
    | Ast0.MetaBinary(mv, cstr, _) ->
	let cstr' = constraints cstr in
	Ast.MetaBinary(mcode mv, cstr', unitary, false))

and expression_dots ed = dots expression ed

and string_fragment e =
  rewrap e no_isos
    (match Ast0.unwrap e with
      Ast0.ConstantFragment(str) -> Ast.ConstantFragment(mcode str)
    | Ast0.FormatFragment(pct,fmt) ->
	Ast.FormatFragment(mcode pct, string_format fmt)
    | Ast0.Strdots dots -> Ast.Strdots (mcode dots)
    | Ast0.MetaFormatList(pct,name,cstr,lenname) ->
	Ast.MetaFormatList(mcode pct, mcode name, do_lenname lenname,
			   constraints cstr,unitary,false))

and string_format e =
  rewrap e no_isos
    (match Ast0.unwrap e with
      Ast0.ConstantFormat(str) -> Ast.ConstantFormat(mcode str)
    | Ast0.MetaFormat(name,cstr) ->
	let cstr' = constraints cstr in
	Ast.MetaFormat(mcode name,cstr',unitary,false))

and do_lenname = function
    Ast0.MetaListLen(nm,cstr) ->
      Ast.MetaListLen(mcode nm,constraints cstr,unitary,false)
  | Ast0.CstListLen n -> Ast.CstListLen n
  | Ast0.AnyListLen -> Ast.AnyListLen

(* --------------------------------------------------------------------- *)
(* Types *)

and rewrap_iso t t1 = rewrap t (do_isos (Ast0.get_iso t)) t1

and typeC allminus t =
  rewrap t (do_isos (Ast0.get_iso t))
    (match Ast0.unwrap t with
      Ast0.ConstVol(cv,ty) ->
	let rec collect_disjs t =
	  match Ast0.unwrap t with
	    Ast0.DisjType(_,types,_,_) ->
	      if Ast0.get_iso t = []
	      then List.concat (List.map collect_disjs types)
	      else failwith "unexpected iso on a disjtype"
	  | _ -> [t] in
	let res =
	  List.map
	    (function ty ->
	      Ast.Type
		(allminus, Some (mcode cv),
		 rewrap_iso ty (base_typeC allminus ty)))
	    (collect_disjs ty) in
	(* one could worry that isos are lost because we flatten the
	   disjunctions.  but there should not be isos on the disjunctions
	   themselves. *)
	(match res with
	  [ty] -> ty
	| types -> Ast.DisjType(List.map (rewrap t no_isos) types))
    | Ast0.BaseType(_) | Ast0.Signed(_,_) | Ast0.Pointer(_,_)
    | Ast0.ParenType(_,_,_) | Ast0.FunctionType(_,_,_,_)
    | Ast0.Array(_,_,_,_) | Ast0.Decimal(_,_,_,_,_,_)
    | Ast0.EnumName(_,_) | Ast0.StructUnionName(_,_)
    | Ast0.StructUnionDef(_,_,_,_) | Ast0.EnumDef(_,_,_,_)
    | Ast0.TypeOfExpr(_,_,_,_) | Ast0.TypeOfType(_,_,_,_)
    | Ast0.TypeName(_) | Ast0.AutoType(_) | Ast0.MetaType(_,_,_) ->
	Ast.Type(allminus,None,rewrap t no_isos (base_typeC allminus t))
    | Ast0.DisjType(_,types,_,_) ->
	Ast.DisjType(List.map (typeC allminus) types)
    | Ast0.ConjType(_,types,_,_) ->
	Ast.ConjType(List.map (typeC allminus) types)
    | Ast0.AsType(ty,asty) ->
	Ast.AsType(typeC allminus ty,typeC allminus asty)
    | Ast0.OptType(ty) -> Ast.OptType(typeC allminus ty))

and base_typeC allminus t =
  match Ast0.unwrap t with
    Ast0.BaseType(ty,strings) -> Ast.BaseType(ty,List.map mcode strings)
  | Ast0.Signed(sgn,ty) ->
      Ast.SignedT
	(mcode sgn,
	 get_option (function x -> rewrap_iso x (base_typeC allminus x)) ty)
  | Ast0.Pointer(ty,star) -> Ast.Pointer(typeC allminus ty,mcode star)
  | Ast0.ParenType(lp,ty,rp) ->
      Ast.ParenType(mcode lp,typeC allminus ty,mcode rp)
  | Ast0.FunctionType(ty,lp,params,rp) ->
      Ast.FunctionType
        (typeC allminus ty,mcode lp,parameter_list params,mcode rp)
  | Ast0.Array(ty,lb,size,rb) ->
      Ast.Array(typeC allminus ty,mcode lb,get_option expression size,
		mcode rb)
  | Ast0.Decimal(dec,lp,length,comma,precision_opt,rp) ->
      Ast.Decimal(mcode dec,mcode lp,expression length,
		  get_option mcode comma,get_option expression precision_opt,
		  mcode rp)
  | Ast0.EnumName(kind,name) ->
      Ast.EnumName(mcode kind,get_option ident name)
  | Ast0.EnumDef(ty,lb,ids,rb) ->
      Ast.EnumDef(typeC allminus ty,mcode lb,enum_decl_dots ids,mcode rb)
  | Ast0.StructUnionName(kind,name) ->
      Ast.StructUnionName(mcode kind,get_option ident name)
  | Ast0.StructUnionDef(ty,lb,decls,rb) ->
      Ast.StructUnionDef(typeC allminus ty,mcode lb,
			 field_dots decls, mcode rb)
  | Ast0.TypeOfExpr(tf,lp,exp,rp) ->
      Ast.TypeOfExpr(mcode tf,mcode lp,expression exp,mcode rp)
  | Ast0.TypeOfType(tf,lp,ty,rp) ->
      let allminus = check_allminus.VT0.combiner_rec_typeC t in
      Ast.TypeOfType(mcode tf, mcode lp,typeC allminus ty,mcode rp)
  | Ast0.TypeName(name) -> Ast.TypeName(mcode name)
  | Ast0.AutoType(auto) -> Ast.AutoType(mcode auto)
  | Ast0.MetaType(name,cstr,_) ->
      let cstr' = constraints cstr in
      Ast.MetaType(mcode name,cstr',unitary,false)
  | _ -> failwith "ast0toast: unexpected type"

(* --------------------------------------------------------------------- *)
(* Variable declaration *)
(* Even if the Cocci program specifies a list of declarations, they are
   split out into multiple declarations of a single variable each. *)

and declaration d =
  rewrap d (do_isos (Ast0.get_iso d))
    (match Ast0.unwrap d with
      Ast0.MetaDecl(name,cstr,_) ->
	Ast.MetaDecl(mcode name,constraints cstr,unitary,false)
    | Ast0.AsDecl(decl,asdecl) ->
	Ast.AsDecl(declaration decl,declaration asdecl)
    | Ast0.Init(stg,ty,id,attr,eq,ini,sem) ->
	let allminus = check_allminus.VT0.combiner_rec_declaration d in
	let stg = get_option mcode stg in
	let ty = typeC allminus ty in
	let id = ident id in
	let attr = List.map attribute attr in
	let eq = mcode eq in
	let ini = initialiser ini in
	let sem = mcode sem in
	Ast.Init(stg,ty,id,attr,eq,ini,sem)
    | Ast0.UnInit(stg,ty,id,attr,sem) ->
	let allminus = check_allminus.VT0.combiner_rec_declaration d in
	let attr = List.map attribute attr in
	Ast.UnInit(get_option mcode stg,typeC allminus ty,ident id,attr,
		   mcode sem)
    | Ast0.FunProto(fi,name,lp,params,va,rp,sem) ->
	  let fi = List.map fninfo fi in
	  let name = ident name in
	  let lp = mcode lp in
	  let params = parameter_list params in
          let va = match va with
            | None -> None
            | Some (comma,ellipsis) -> Some(mcode comma,mcode ellipsis) in
	  let rp = mcode rp in
	  let sem = mcode sem in
	  Ast.FunProto(fi,name,lp,params,va,rp,sem)
    | Ast0.MacroDecl(stg,name,lp,args,rp,attr,sem) ->
	(* this would seem to need allminus... *)
	let stg = get_option mcode stg in
	let name = ident name in
	let lp = mcode lp in
	let args = dots expression args in
	let rp = mcode rp in
	let attr = List.map attribute attr in
	let sem = mcode sem in
	Ast.MacroDecl(stg,name,lp,args,rp,attr,sem)
    | Ast0.MacroDeclInit(stg,name,lp,args,rp,eq,ini,sem) ->
	(* this would seem to need allminus... *)
	let stg = get_option mcode stg in
	let name = ident name in
	let lp = mcode lp in
	let args = dots expression args in
	let rp = mcode rp in
	let eq = mcode eq in
	let ini = initialiser ini in
	let sem = mcode sem in
	Ast.MacroDeclInit(stg,name,lp,args,rp,eq,ini,sem)
    | Ast0.TyDecl(ty,attr,sem) ->
	let allminus = check_allminus.VT0.combiner_rec_declaration d in
	let attr = List.map attribute attr in
	Ast.TyDecl(typeC allminus ty,attr,mcode sem)
    | Ast0.Typedef(stg,ty,id,sem) ->
	let allminus = check_allminus.VT0.combiner_rec_declaration d in
	let id = typeC allminus id in
	(match Ast.unwrap id with
	  Ast.Type(_,None,id) -> (* only MetaType or Id *)
	    Ast.Typedef(mcode stg,typeC allminus ty,id,mcode sem)
	| _ -> failwith "bad typedef")
    | Ast0.DisjDecl(_,decls,_,_) -> Ast.DisjDecl(List.map declaration decls)
    | Ast0.ConjDecl(_,decls,_,_) -> Ast.ConjDecl(List.map declaration decls)
    | Ast0.OptDecl(decl) -> Ast.OptDecl(declaration decl))

and annotated_decl bef d =
  rewrap d (do_isos (Ast0.get_iso d))
    (match Ast0.unwrap d with
      _ -> (* for decls where there is no bef information needed *)
	let bef =
	  match bef with
	    None -> (* fake, no change here *)
	      let bot = Ast0.default_token_info in
	      Ast0.CONTEXT (ref(Ast.NOTHING,bot,bot))
	  | Some bef -> bef in
	let allminus = check_allminus.VT0.combiner_rec_declaration d in
	Ast.DElem(convert_allminus_mcodekind allminus bef,allminus,
		  declaration d))

and declaration_dots l = dots (annotated_decl None) l

(* --------------------------------------------------------------------- *)
(* Field declaration *)

and field d =
  rewrap d (do_isos (Ast0.get_iso d))
    (match Ast0.unwrap d with
      Ast0.MetaField(name,cstr,_) ->
	Ast.MetaField(mcode name,constraints cstr,unitary,false)
    | Ast0.MetaFieldList(name,lenname,cstr,_) ->
	let cstr' = constraints cstr in
	Ast.MetaFieldList(mcode name,do_lenname lenname,cstr',unitary,false)
    | Ast0.Field(ty,id,bf,sem) ->
	let allminus = check_allminus.VT0.combiner_rec_field d in
	let bitfield (c, e) = (mcode c, expression e) in
	let bf = Common.map_option bitfield bf in
	Ast.Field(typeC allminus ty,Common.map_option ident id, bf, mcode sem)
    | Ast0.ConjField(_,_,_,_)
    | Ast0.DisjField(_,_,_,_)
    | Ast0.OptField(_)
    | Ast0.Fdots(_,_) -> failwith "should not be possible")

and annotated_field bef d =
  rewrap d (do_isos (Ast0.get_iso d))
    (match Ast0.unwrap d with
      Ast0.Fdots(dots,whencode) ->
	(* structure definitions only *)
	let dots = mcode dots in
	let whencode = get_option (fun (_,_,b) -> field b) whencode in
	Ast.Fdots(dots,whencode)
    | Ast0.ConjField(_,decls,_,_) -> Ast.ConjField(List.map (annotated_field bef) decls)
    | Ast0.DisjField(_,decls,_,_) -> Ast.DisjField(List.map (annotated_field bef) decls)
    | Ast0.OptField(decl) -> Ast.OptField(annotated_field bef decl)
    | _ -> (* for decls where there is no bef information needed *)
	let bef =
	  match bef with
	    None -> (* fake, no change here *)
	      let bot = Ast0.default_token_info in
	      Ast0.CONTEXT (ref(Ast.NOTHING,bot,bot))
	  | Some bef -> bef in
	let allminus = check_allminus.VT0.combiner_rec_field d in
	Ast.FElem(convert_allminus_mcodekind allminus bef,allminus,
		  field d))

and field_dots l = dots (annotated_field None) l

and enum_decl d =
  rewrap d (do_isos (Ast0.get_iso d))
    (match Ast0.unwrap d with
      Ast0.Enum(name,enum_val) ->
        (match enum_val with
          None -> Ast.Enum(ident name,None)
        | Some(eq,eval) ->
            Ast.Enum(ident name,Some(mcode eq,expression eval)))
    | Ast0.EnumComma(cm) ->
	Ast.EnumComma(mcode cm)
    | Ast0.EnumDots(dots,whencode) ->
	(* structure definitions only *)
	let dots = mcode dots in
	let whencode = get_option (fun (_,_,b) -> enum_decl b) whencode in
	Ast.EnumDots(dots,whencode))

and enum_decl_dots l = dots enum_decl l

(* --------------------------------------------------------------------- *)
(* Initialiser *)

and strip_idots initlist =
  let isminus mc =
    match Ast0.get_mcode_mcodekind mc with
      Ast0.MINUS _ -> true
    | _ -> false in
  let l = Ast0.unwrap initlist in
  let l =
    match List.rev l with
      [] | [_] -> l
    | x::y::xs ->
	(match (Ast0.unwrap x,Ast0.unwrap y) with
	  (Ast0.IComma _,Ast0.Idots _) ->
		(* drop comma that was added by add_comma *)
	    List.rev (y::xs)
	| _ -> l) in
  let (whencode,init,dotinfo) =
    let rec loop = function
	[] -> ([],[],[])
      | x::rest ->
	  (match Ast0.unwrap x with
	    Ast0.Idots(dots,Some whencode) ->
	      let (restwhen,restinit,dotinfo) = loop rest in
	      (whencode :: restwhen, restinit,
		(isminus dots)::dotinfo)
	  | Ast0.Idots(dots,None) ->
	      let (restwhen,restinit,dotinfo) = loop rest in
	      (restwhen, restinit, (isminus dots)::dotinfo)
	  | _ ->
	      let (restwhen,restinit,dotinfo) = loop rest in
	      (restwhen,x::restinit,dotinfo)) in
    loop l in
  let allminus =
    if List.for_all (function x -> not x) dotinfo
    then false (* false if no dots *)
    else
      if List.for_all (function x -> x) dotinfo
      then true
      else failwith "inconsistent annotations on initialiser list dots" in
  (whencode, init, allminus)
    
and initialiser i =
  rewrap i no_isos
    (match Ast0.unwrap i with
      Ast0.MetaInit(name,cstr,_) ->
	Ast.MetaInit(mcode name,constraints cstr,unitary,false)
    | Ast0.MetaInitList(name,lenname,cstr,_) ->
	let cstr' = constraints cstr in
	Ast.MetaInitList(mcode name,do_lenname lenname,cstr',unitary,false)
    | Ast0.AsInit(init,asinit) ->
	Ast.AsInit(initialiser init,initialiser asinit)
    | Ast0.InitExpr(exp) -> Ast.InitExpr(expression exp)
    | Ast0.InitList(lb,initlist,rb,true) ->
	Ast.ArInitList(mcode lb,dots initialiser initlist,mcode rb)
    | Ast0.InitList(lb,initlist,rb,false) ->
	let (whencode,initlist,allminus) = strip_idots initlist in
	Ast.StrInitList
	  (allminus,mcode lb,List.map initialiser initlist,mcode rb,
	   List.map (fun (_,_,b) -> initialiser b) whencode)
    | Ast0.InitGccExt(designators,eq,ini) ->
	Ast.InitGccExt(List.map designator designators,mcode eq,
		       initialiser ini)
    | Ast0.InitGccName(name,eq,ini) ->
	Ast.InitGccName(ident name,mcode eq,initialiser ini)
    | Ast0.IComma(comma) -> Ast.IComma(mcode comma)
    | Ast0.Idots(dots,whencode) ->
	let dots = mcode dots in
	let whencode = get_option (fun (_,_,b) -> initialiser b) whencode in
	Ast.Idots(dots,whencode)
    | Ast0.OptIni(ini) -> Ast.OptIni(initialiser ini))

and designator = function
    Ast0.DesignatorField(dot,id) -> Ast.DesignatorField(mcode dot,ident id)
  | Ast0.DesignatorIndex(lb,exp,rb) ->
      Ast.DesignatorIndex(mcode lb, expression exp, mcode rb)
  | Ast0.DesignatorRange(lb,min,dots,max,rb) ->
      Ast.DesignatorRange(mcode lb,expression min,mcode dots,expression max,
			  mcode rb)

(* --------------------------------------------------------------------- *)
(* Parameter *)

and parameterTypeDef p =
  rewrap p no_isos
    (match Ast0.unwrap p with
      Ast0.VoidParam(ty,attr) ->
        Ast.VoidParam(typeC false ty,List.map attribute attr)
    | Ast0.Param(ty,id,attr) ->
	let allminus = check_allminus.VT0.combiner_rec_parameter p in
	Ast.Param(typeC allminus ty,get_option ident id,List.map attribute attr)
    | Ast0.MetaParam(name,cstr,_) ->
	Ast.MetaParam(mcode name,constraints cstr,unitary,false)
    | Ast0.MetaParamList(name,lenname,cstr,_) ->
	let cstr' = constraints cstr in
	Ast.MetaParamList(mcode name,do_lenname lenname,cstr',unitary,false)
    | Ast0.AsParam(p,asexpr) ->
	Ast.AsParam(parameterTypeDef p,expression asexpr)
    | Ast0.PComma(cm) -> Ast.PComma(mcode cm)
    | Ast0.Pdots(dots) -> Ast.Pdots(mcode dots)
    | Ast0.OptParam(param) -> Ast.OptParam(parameterTypeDef param))

and parameter_list l = dots parameterTypeDef l

(* --------------------------------------------------------------------- *)
(* Top-level code *)

and statement s =
  let rec statement seqible s =
    let rewrap_stmt ast0 ast =
      let befaft =
	match Ast0.get_dots_bef_aft s with
	  Ast0.NoDots -> Ast.NoDots
	| Ast0.DroppingBetweenDots s ->
	    Ast.DroppingBetweenDots (statement seqible s,get_ctr())
	| Ast0.AddingBetweenDots s ->
	    Ast.AddingBetweenDots (statement seqible s,get_ctr()) in
      Ast.set_dots_bef_aft befaft (rewrap ast0 no_isos ast) in
    let rewrap_rule_elem ast0 ast =
      rewrap ast0 (do_isos (Ast0.get_iso ast0)) ast in
    rewrap_stmt s
      (match Ast0.unwrap s with
	Ast0.Decl((_,bef),decl) ->
	  Ast.Atomic(rewrap_rule_elem s
		       (Ast.Decl(annotated_decl (Some bef) decl)))
      | Ast0.Seq(lbrace,body,rbrace) ->
          let lbrace = mcode lbrace in
          let body = dots (statement seqible) body in
          let rbrace = mcode rbrace in
          Ast.Seq(iso_tokenwrap lbrace s (Ast.SeqStart(lbrace))
                    (do_isos (Ast0.get_iso s)),
                  body,
                  tokenwrap rbrace s (Ast.SeqEnd(rbrace)))
      | Ast0.ExprStatement(exp,sem) ->
	  Ast.Atomic(rewrap_rule_elem s
		       (Ast.ExprStatement
			  (get_option expression exp,mcode sem)))
      | Ast0.IfThen(iff,lp,exp,rp,branch,aft) ->
	  Ast.IfThen
	    (rewrap_rule_elem s
	       (Ast.IfHeader(mcode iff,mcode lp,expression exp,mcode rp)),
	     statement Ast.NotSequencible branch,
	     ([],[],[],convert_fake_mcode aft))
      | Ast0.IfThenElse(iff,lp,exp,rp,branch1,els,branch2,aft) ->
	  let els = mcode els in
	  Ast.IfThenElse
	    (rewrap_rule_elem s
	       (Ast.IfHeader(mcode iff,mcode lp,expression exp,mcode rp)),
	     statement Ast.NotSequencible branch1,
	     tokenwrap els s (Ast.Else(els)),
	     statement Ast.NotSequencible branch2,
	     ([],[],[],convert_fake_mcode aft))
      | Ast0.While(wh,lp,exp,rp,body,aft) ->
	  Ast.While(rewrap_rule_elem s
		      (Ast.WhileHeader
			 (mcode wh,mcode lp,expression exp,mcode rp)),
		    statement Ast.NotSequencible body,
		    ([],[],[],convert_fake_mcode aft))
      | Ast0.Do(d,body,wh,lp,exp,rp,sem) ->
	  let wh = mcode wh in
	  Ast.Do(rewrap_rule_elem s (Ast.DoHeader(mcode d)),
		 statement Ast.NotSequencible body,
		 tokenwrap wh s
		   (Ast.WhileTail(wh,mcode lp,expression exp,mcode rp,
				  mcode sem)))
      | Ast0.For(fr,lp,first,exp2,sem2,exp3,rp,body,aft) ->
	  let fr = mcode fr in
	  let lp = mcode lp in
	  let first = forinfo first in
	  let exp2 = get_option expression exp2 in
	  let sem2= mcode sem2 in
	  let exp3 = get_option expression exp3 in
	  let rp = mcode rp in
	  let body = statement Ast.NotSequencible body in
	  Ast.For(rewrap_rule_elem s
		    (Ast.ForHeader(fr,lp,first,exp2,sem2,exp3,rp)),
		  body,([],[],[],convert_fake_mcode aft))
      | Ast0.Iterator(nm,lp,args,rp,body,aft) ->
	  Ast.Iterator(rewrap_rule_elem s
		      (Ast.IteratorHeader
			 (ident nm,mcode lp,
			  dots expression args,
			  mcode rp)),
		    statement Ast.NotSequencible body,
		    ([],[],[],convert_fake_mcode aft))
      |	Ast0.Switch(switch,lp,exp,rp,lb,decls,cases,rb) ->
	  let switch = mcode switch in
	  let lp = mcode lp in
	  let exp = expression exp in
	  let rp = mcode rp in
	  let lb = mcode lb in
	  let decls = dots (statement seqible) decls in
	  let cases = List.map case_line (Ast0.unwrap cases) in
	  let rb = mcode rb in
	  Ast.Switch(rewrap_rule_elem s (Ast.SwitchHeader(switch,lp,exp,rp)),
		     tokenwrap lb s (Ast.SeqStart(lb)),
		     decls,cases,
		     tokenwrap rb s (Ast.SeqEnd(rb)))
      | Ast0.Break(br,sem) ->
	  Ast.Atomic(rewrap_rule_elem s (Ast.Break(mcode br,mcode sem)))
      | Ast0.Continue(cont,sem) ->
	  Ast.Atomic(rewrap_rule_elem s (Ast.Continue(mcode cont,mcode sem)))
      |	Ast0.Label(l,dd) ->
	  Ast.Atomic(rewrap_rule_elem s (Ast.Label(ident l,mcode dd)))
      |	Ast0.Goto(goto,l,sem) ->
	  Ast.Atomic
	    (rewrap_rule_elem s (Ast.Goto(mcode goto,ident l,mcode sem)))
      | Ast0.Return(ret,sem) ->
	  Ast.Atomic(rewrap_rule_elem s (Ast.Return(mcode ret,mcode sem)))
      | Ast0.ReturnExpr(ret,exp,sem) ->
	  Ast.Atomic
	    (rewrap_rule_elem s
	       (Ast.ReturnExpr(mcode ret,expression exp,mcode sem)))
      | Ast0.Exec(exec,lang,code,sem) ->
	  Ast.Atomic
	    (rewrap_rule_elem s
	       (Ast.Exec(mcode exec,mcode lang,dots exec_code code,mcode sem)))
      | Ast0.MetaStmt(name,cstr,_) ->
	  let cstr' = constraints cstr in
	  Ast.Atomic(rewrap_rule_elem s
		       (Ast.MetaStmt(mcode name,cstr',unitary,seqible,false)))
      | Ast0.MetaStmtList(name,lenname,cstr,_) ->
	  let cstr' = constraints cstr in
	  Ast.Atomic(rewrap_rule_elem s
		       (Ast.MetaStmtList(mcode name,do_lenname lenname,
					 cstr',unitary,false)))
      | Ast0.AsStmt(stmt,asstmt) ->
	  Ast.AsStmt(statement seqible stmt,statement seqible asstmt)
      | Ast0.TopExp(exp) ->
	  Ast.Atomic(rewrap_rule_elem s (Ast.TopExp(expression exp)))
      | Ast0.Exp(exp) ->
	  Ast.Atomic(rewrap_rule_elem s (Ast.Exp(expression exp)))
      | Ast0.TopInit(init) ->
	  Ast.Atomic(rewrap_rule_elem s (Ast.TopInit(initialiser init)))
      | Ast0.Ty(ty) ->
	  let allminus = check_allminus.VT0.combiner_rec_statement s in
	  Ast.Atomic(rewrap_rule_elem s (Ast.Ty(typeC allminus ty)))
      | Ast0.TopId(id) ->
	  Ast.Atomic(rewrap_rule_elem s (Ast.TopId(ident id)))
      | Ast0.Disj(_,rule_elem_dots_list,_,_) ->
	  Ast.Disj(List.map (function x -> statement_dots seqible x)
		     rule_elem_dots_list)
      | Ast0.Conj(_,rule_elem_dots_list,_,_) ->
	  Ast.Conj(List.map (function x -> statement_dots seqible x)
		     rule_elem_dots_list)
      | Ast0.Nest(starter,rule_elem_dots,ender,whn,multi) ->
	  Ast.Nest
	    (mcode starter,statement_dots Ast.Sequencible rule_elem_dots,
	     mcode ender,
	     List.map
	       (whencode (statement_dots Ast.Sequencible)
		 (statement Ast.NotSequencible))
	       whn,
	     multi,[],[])
      | Ast0.Dots(d,whn) ->
	  let d = mcode d in
	  let whn =
	    List.map
	      (whencode (statement_dots Ast.Sequencible)
		 (statement Ast.NotSequencible))
	      whn in
	  Ast.Dots(d,whn,[],[])
      | Ast0.FunDecl((_,bef),fi,name,lp,params,va,rp,lbrace,body,rbrace,
		     (_,aft)) ->
	  let fi = List.map fninfo fi in
	  let name = ident name in
	  let lp = mcode lp in
	  let params = parameter_list params in
          let newva = match va with
            | None -> None
            | Some (comma, ellipsis) -> Some (mcode comma, mcode ellipsis) in
	  let rp = mcode rp in
          let lbrace = mcode lbrace in
          let body = dots (statement seqible) body in
          let rbrace = mcode rbrace in
          let allminus = check_allminus.VT0.combiner_rec_statement s in
          Ast.FunDecl(rewrap_rule_elem s
                        (Ast.FunHeader
                           (convert_allminus_mcodekind allminus bef,
                            allminus,fi,name,lp,params,newva,rp)),
                      tokenwrap lbrace s (Ast.SeqStart(lbrace)),
                      body,
                      tokenwrap rbrace s (Ast.SeqEnd(rbrace)),
                      ([],[],[],convert_allminus_mcodekind allminus aft))
      |	Ast0.Include(inc,str) ->
	  Ast.Atomic(rewrap_rule_elem s (Ast.Include(mcode inc,mcode str)))
      |	Ast0.MetaInclude(inc,str) ->
	  Ast.Atomic
	    (rewrap_rule_elem s (Ast.MetaInclude(mcode inc,expression str)))
      |	Ast0.Undef(def,id) ->
	  Ast.Atomic(rewrap_rule_elem s (Ast.Undef(mcode def,ident id)))
      | Ast0.Define(def,id,params,body) ->
	  Ast.Define
	    (rewrap_rule_elem s
	       (Ast.DefineHeader
		  (mcode def,ident id, define_parameters params)),
	     statement_dots Ast.NotSequencible (*not sure*) body)
      |	Ast0.Pragma(prg,id,body) ->
	  Ast.Atomic(rewrap_rule_elem s
		       (Ast.Pragma(mcode prg,ident id,pragmainfo body)))
      | Ast0.OptStm(stm) -> Ast.OptStm(statement seqible stm))

  and whencode notfn alwaysfn = function
      Ast0.WhenNot (_,_,a) -> Ast.WhenNot (notfn a)
    | Ast0.WhenAlways (_,_,a) -> Ast.WhenAlways (alwaysfn a)
    | Ast0.WhenModifier(_,x) -> Ast.WhenModifier(x)
    | x ->
	let rewrap_rule_elem ast0 ast =
	  rewrap ast0 (do_isos (Ast0.get_iso ast0)) ast in
	match x with
	  Ast0.WhenNotTrue(_,_,e) ->
	    Ast.WhenNotTrue(rewrap_rule_elem e (Ast.Exp(expression e)))
	| Ast0.WhenNotFalse(_,_,e) ->
	    Ast.WhenNotFalse(rewrap_rule_elem e (Ast.Exp(expression e)))
	| _ -> failwith "not possible"

  and process_list seqible isos = function
      [] -> []
    | x::rest ->
	let first = statement seqible x in
	let first =
	  if !Flag.track_iso_usage
	  then Ast.set_isos first (isos@(Ast.get_isos first))
	  else first in
	(match Ast0.unwrap x with
	  Ast0.Dots(_,_) | Ast0.Nest(_) ->
	    first::(process_list (Ast.SequencibleAfterDots []) no_isos rest)
	| _ ->
	    first::(process_list Ast.Sequencible no_isos rest))

  and statement_dots seqible d =
    let isos = do_isos (Ast0.get_iso d) in
    rewrap d no_isos (process_list seqible isos (Ast0.unwrap d))

  (* the following is no longer used.
   the goal was to let one put a statement at the very beginning of a function
   pattern and have it skip over the declarations in the C code.
   that feature was removed a long time ago, however, in favor of
   ... when != S, which also causes whatever comes after it to match the
   first real statement.
   the separation of declarations from the rest of the body means that the
   quantifier of any variable shared between them comes out too high, posing
   problems when there is ... decl ... stmt, as the quantifier of any shared
   variable will be around the whole thing, making variables not free enough
   in the first ..., and thus not implementing the expected shortest path
   condition.  example: f() { ... int A; ... foo(A); }.
   the quantifier for A should start just before int A, not at the top of the
   function.
  and separate_decls seqible d =
    let rec collect_decls = function
	[] -> ([],[])
      | (x::xs) as l ->
	  (match Ast0.unwrap x with
	    Ast0.Decl(_) ->
	      let (decls,other) = collect_decls xs in
	      (x :: decls,other)
	  | Ast0.Dots(_,_) | Ast0.Nest(_,_,_,_,_) ->
	      let (decls,other) = collect_decls xs in
	      (match decls with
		[] -> ([],x::other)
	      | _ -> (x :: decls,other))
	  | Ast0.Disj(starter,stmt_dots_list,mids,ender)
	  | Ast0.Conj(starter,stmt_dots_list,mids,ender) ->
	      let disjs = List.map collect_dot_decls stmt_dots_list in
	      let all_decls = List.for_all (function (_,s) -> s=[]) disjs in
	      if all_decls
	      then
		let (decls,other) = collect_decls xs in
		(x :: decls,other)
	      else ([],l)
	  | _ -> ([],l))

    and collect_dot_decls d = collect_decls (Ast0.unwrap d) in

    let process l d =
      let (decls,other) = collect_decls l in
      (rewrap d no_isos (List.map (statement seqible) decls),
       rewrap d no_isos
	 (process_list seqible (do_isos (Ast0.get_iso d)) other)) in
     process x (Ast0.unwrap d) *) in

  statement Ast.Sequencible s

and pragmainfo pi =
  rewrap pi no_isos
    (match Ast0.unwrap pi with
      Ast0.PragmaString(s) -> Ast.PragmaString(mcode s)
    | Ast0.PragmaDots (dots) -> Ast.PragmaDots (mcode dots))

and define_parameters p =
  rewrap p no_isos
    (match Ast0.unwrap p with
      Ast0.NoParams -> Ast.NoParams
    | Ast0.DParams(lp,params,rp) ->
	Ast.DParams(mcode lp,define_param_dots params,mcode rp))

and define_param p =
  rewrap p no_isos
    (match Ast0.unwrap p with
      Ast0.DParam(id) -> Ast.DParam(ident id)
    | Ast0.MetaDParamList(name,lenname,cstr,_) ->
	let cstr' = constraints cstr in
	Ast.MetaDParamList(mcode name,do_lenname lenname,cstr',unitary,false)
    | Ast0.DPComma(comma) -> Ast.DPComma(mcode comma)
    | Ast0.DPdots(d) -> Ast.DPdots(mcode d)
    | Ast0.OptDParam(dp) -> Ast.OptDParam(define_param dp))

and define_param_dots l = dots define_param l

and forinfo fi =
  match Ast0.unwrap fi with
    Ast0.ForExp(exp1,sem1) ->
      let exp1 = get_option expression exp1 in
      let sem1 = mcode sem1 in
      Ast.ForExp(exp1,sem1)
  | Ast0.ForDecl ((_,bef),decl) -> Ast.ForDecl(annotated_decl (Some bef) decl)

and fninfo = function
    Ast0.FStorage(stg) -> Ast.FStorage(mcode stg)
  | Ast0.FType(ty) -> Ast.FType(typeC false ty)
  | Ast0.FInline(inline) -> Ast.FInline(mcode inline)
  | Ast0.FAttr(attr) -> Ast.FAttr(attribute attr)

and attribute a =
  rewrap a no_isos
    (match Ast0.unwrap a with
      Ast0.Attribute(attr) -> Ast.Attribute(attr_arg attr))

and attr_arg a =
  rewrap a no_isos
    (match Ast0.unwrap a with
      Ast0.MetaAttr(name,cstr,_) ->
	Ast.MetaAttr(mcode name,constraints cstr,unitary,false)
    | Ast0.AttrName(arg) -> Ast.AttrName(mcode arg))

and option_to_list = function
    Some x -> [x]
  | None -> []

and case_line c =
  rewrap c no_isos
    (match Ast0.unwrap c with
      Ast0.Default(def,colon,code) ->
	let def = mcode def in
	let colon = mcode colon in
	let code = dots statement code in
	Ast.CaseLine(rewrap c no_isos (Ast.Default(def,colon)),code)
    | Ast0.Case(case,exp,colon,code) ->
	let case = mcode case in
	let exp = expression exp in
	let colon = mcode colon in
	let code = dots statement code in
	Ast.CaseLine(rewrap c no_isos (Ast.Case(case,exp,colon)),code)
    | Ast0.DisjCase(_,case_lines,_,_) ->
	failwith "not supported"
	(*Ast.CaseLine(Ast.DisjRuleElem(List.map case_line case_lines))*)

    | Ast0.OptCase(case) -> Ast.OptCase(case_line case))

and exec_code c =
  rewrap c no_isos
    (match Ast0.unwrap c with
      Ast0.ExecEval(colon,id) -> Ast.ExecEval(mcode colon,expression id)
    | Ast0.ExecToken(tok) -> Ast.ExecToken(mcode tok)
    | Ast0.ExecDots(dots) -> Ast.ExecDots(mcode dots))

and statement_dots l = dots statement l

(* --------------------------------------------------------------------- *)

(* what is possible is only what is at the top level in an iso *)
and anything = function
    Ast0.DotsExprTag(d) -> Ast.ExprDotsTag(expression_dots d)
  | Ast0.DotsParamTag(d) -> Ast.ParamDotsTag(parameter_list d)
  | Ast0.DotsInitTag(d) -> failwith "not possible"
  | Ast0.DotsStmtTag(d) -> Ast.StmtDotsTag(statement_dots d)
  | Ast0.DotsDeclTag(d) -> Ast.AnnDeclDotsTag(declaration_dots d)
  | Ast0.DotsFieldTag(d) -> Ast.AnnFieldDotsTag(field_dots d)
  | Ast0.DotsEnumDeclTag(d) -> Ast.EnumDeclDotsTag(enum_decl_dots d)
  | Ast0.DotsCaseTag(d) -> failwith "not possible"
  | Ast0.DotsDefParamTag(d) -> Ast.DefParDotsTag(define_param_dots d)
  | Ast0.IdentTag(d) -> Ast.IdentTag(ident d)
  | Ast0.ExprTag(d) -> Ast.ExpressionTag(expression d)
  | Ast0.AssignOpTag d -> Ast.AssignOpTag(assignOp d)
  | Ast0.BinaryOpTag d -> Ast.BinaryOpTag(binaryOp d)
  | Ast0.ArgExprTag(d) | Ast0.TestExprTag(d) ->
     failwith "only in isos, not converted to ast"
  | Ast0.TypeCTag(d) -> Ast.FullTypeTag(typeC false d)
  | Ast0.ParamTag(d) -> Ast.ParamTag(parameterTypeDef d)
  | Ast0.InitTag(d) -> Ast.InitTag(initialiser d)
  | Ast0.DeclTag(d) -> Ast.DeclarationTag(declaration d)
  | Ast0.FieldTag(d) -> Ast.FieldTag(field d)
  | Ast0.EnumDeclTag(d) -> Ast.EnumDeclTag(enum_decl d)
  | Ast0.StmtTag(d) -> Ast.StatementTag(statement d)
  | Ast0.ForInfoTag(d) -> Ast.ForInfoTag(forinfo d)
  | Ast0.CaseLineTag(d) -> Ast.CaseLineTag(case_line d)
  | Ast0.StringFragmentTag(d) -> Ast.StringFragmentTag(string_fragment d)
  | Ast0.AttributeTag(d) -> Ast.AttributeTag(attribute d)
  | Ast0.AttrArgTag(d) -> Ast.AttrArgTag(attr_arg d)
  | Ast0.TopTag(d) -> Ast.Code(top_level d)
  | Ast0.IsoWhenTag(_) -> failwith "not possible"
  | Ast0.IsoWhenTTag(_) -> failwith "not possible"
  | Ast0.IsoWhenFTag(_) -> failwith "not possible"
  | Ast0.MetaPosTag _ -> failwith "not possible"
  | Ast0.HiddenVarTag _ -> failwith "not possible"
  | Ast0.WhenTag _ -> failwith "not possible"

(* --------------------------------------------------------------------- *)
(* Function declaration *)
(* top level isos are probably lost to tracking *)

and top_level t =
  rewrap t no_isos
    (match Ast0.unwrap t with
      Ast0.FILEINFO(old_file,new_file) ->
	Ast.FILEINFO(mcode old_file,mcode new_file)
    | Ast0.NONDECL(stmt) -> Ast.NONDECL(statement stmt)
    | Ast0.CODE(rule_elem_dots) -> Ast.CODE(statement_dots rule_elem_dots)
    | Ast0.ERRORWORDS(exps) -> Ast.ERRORWORDS(List.map expression exps)
    | Ast0.OTHER(_) | Ast0.TOPCODE(_) -> failwith "eliminated by top_level")

let constraints c =
  let cstr_expr = Some (fun e -> Ast.CstrExpr (expression e)) in
  Ast.cstr_map { Ast.empty_cstr_transformer with Ast.cstr_expr } c

let () = constraints' := constraints

(* --------------------------------------------------------------------- *)
(* Entry point for minus code *)

(* Inline_mcodes is very important - sends + code attached to the - code
down to the mcodes.  The functions above can only be used when there is no
attached + code, eg in + code itself. *)
let ast0toast_toplevel x =
  inline_mcodes.VT0.combiner_rec_top_level x;
  top_level x

let ast0toast name deps dropped exists x is_exp ruletype =
  List.iter inline_mcodes.VT0.combiner_rec_top_level x;
  Ast.CocciRule
    (name,(deps,dropped,exists),List.map top_level x,is_exp,ruletype)
