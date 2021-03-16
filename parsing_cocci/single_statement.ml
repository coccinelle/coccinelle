(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

(* detect statements that are between dots in the minus code, because they
may need a special treatment if they are if branches *)

module Ast0 = Ast0_cocci
module Ast = Ast_cocci
module V0 = Visitor_ast0
module VT0 = Visitor_ast0_types

(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
(* Helpers *)

let left_dots f l =
  match Ast0.unwrap l with
    [] -> false
  | x::xs -> f x

let right_dots f l =
  match List.rev (Ast0.unwrap l) with
    [] -> false
  | x::xs -> f x

let modif_before_mcode mc =
  match Ast0.get_mcode_mcodekind mc with
    Ast0.MINUS mc -> true (*conservative; don't want to hunt right for + code*)
  | Ast0.PLUS _ -> failwith "not possible"
  | Ast0.CONTEXT mc ->
      (match !mc with
	(Ast.BEFORE _,_,_) -> true
      |	(Ast.BEFOREAFTER _,_,_) -> true
      |	_ -> false)
  | Ast0.MIXED mc -> true (* don't think mcode can be mixed *)

let modif_after_mcodekind = function
    Ast0.MINUS mc -> true (*conservative; don't want to hunt right for + code*)
  | Ast0.PLUS _ -> failwith "not possible"
  | Ast0.CONTEXT mc ->
      (match !mc with
	(Ast.AFTER _,_,_) -> true
      |	(Ast.BEFOREAFTER _,_,_) -> true
      |	_ -> false)
  | Ast0.MIXED mc -> true (* don't think mcode can be mixed *)

let modif_after_mcode mc = modif_after_mcodekind (Ast0.get_mcode_mcodekind mc)

let any_statements =
  List.exists
    (List.exists
       (function
	   Ast.StatementTag(_) | Ast.StmtDotsTag(_)
	 | Ast.DeclarationTag(_) | Ast.AnnDeclDotsTag(_) -> true | _ -> false))

let modif_before x =
  match Ast0.get_mcodekind x with
    Ast0.PLUS _ -> failwith "not possible"
  | Ast0.MINUS mc ->
      (match !mc with
	(* do better for the common case of replacing a stmt by another one *)
	((Ast.REPLACEMENT([[Ast.StatementTag(s)]],c)) as old,ti) ->
	  (match Ast.unwrap s with
	    Ast.IfThen(_,_,_) -> true (* potentially dangerous *)
	  | _ -> mc := (old,ti); false)
      |	(_,_) -> true)
  | Ast0.CONTEXT mc | Ast0.MIXED mc ->
      (match !mc with
	(Ast.BEFORE _,_,_) -> true
      |	(Ast.BEFOREAFTER _,_,_) -> true
      |	_ -> false)

let modif_after x =
  match Ast0.get_mcodekind x with
    Ast0.PLUS _ -> failwith "not possible"
  | Ast0.MINUS mc ->
      (match !mc with
	(* do better for the common case of replacing a stmt by another one *)
	((Ast.REPLACEMENT([[Ast.StatementTag(s)]],count)) as old,ti) ->
	  (match Ast.unwrap s with
	    Ast.IfThen(_,_,_) -> true (* potentially dangerous *)
	  | _ -> mc := (old,ti); false)
      |	(Ast.REPLACEMENT(l,_),_) when any_statements l -> true
      |	(l,ti) -> mc := (l,ti); false)
  | Ast0.CONTEXT mc | Ast0.MIXED mc ->
      (match !mc with
	(Ast.AFTER _,_,_) -> true
      |	(Ast.BEFOREAFTER _,_,_) -> true
      |	_ -> false)

(* Identifier *)
let rec left_ident i =
  modif_before i ||
  match Ast0.unwrap i with
    Ast0.Id(name) -> modif_before_mcode name
  | Ast0.MetaId(name,_,_,_) -> modif_before_mcode name
  | Ast0.MetaFunc(name,_,_) -> modif_before_mcode name
  | Ast0.MetaLocalFunc(name,_,_) -> modif_before_mcode name
  | Ast0.DisjId(_,id_list,_,_) -> List.exists left_ident id_list
  | Ast0.ConjId(_,id_list,_,_) -> List.exists left_ident id_list
  | Ast0.OptIdent(id) -> left_ident id
  | Ast0.AsIdent _ -> failwith "not possible"

let rec right_ident i =
  modif_after i ||
  match Ast0.unwrap i with
    Ast0.Id(name) -> modif_after_mcode name
  | Ast0.MetaId(name,_,_,_) -> modif_after_mcode name
  | Ast0.MetaFunc(name,_,_) -> modif_after_mcode name
  | Ast0.MetaLocalFunc(name,_,_) -> modif_after_mcode name
  | Ast0.DisjId(_,id_list,_,_) -> List.exists right_ident id_list
  | Ast0.ConjId(_,id_list,_,_) -> List.exists right_ident id_list
  | Ast0.OptIdent(id) -> right_ident id
  | Ast0.AsIdent _ -> failwith "not possible"

(* --------------------------------------------------------------------- *)
(* Expression *)

let rec left_expression e =
  modif_before e ||
  match Ast0.unwrap e with
    Ast0.Ident(id) -> left_ident id
  | Ast0.Constant(const) -> modif_before_mcode const
  | Ast0.StringConstant(lq,str,rq,_sz) -> modif_before_mcode lq
  | Ast0.FunCall(fn,lp,args,rp) -> left_expression fn
  | Ast0.Assignment(left,op,right,_) -> left_expression left
  | Ast0.Sequence(left,op,right) -> left_expression left
  | Ast0.CondExpr(exp1,why,exp2,colon,exp3) -> left_expression exp1
  | Ast0.Postfix(exp,op) -> left_expression exp
  | Ast0.Infix(exp,op) -> modif_before_mcode op
  | Ast0.Unary(exp,op) -> modif_before_mcode op
  | Ast0.Binary(left,op,right) -> left_expression left
  | Ast0.Nested(left,op,right) -> left_expression left
  | Ast0.Paren(lp,exp,rp) -> modif_before_mcode lp
  | Ast0.ArrayAccess(exp1,lb,exp2,rb) -> left_expression exp1
  | Ast0.RecordAccess(exp,pt,field) -> left_expression exp
  | Ast0.RecordPtAccess(exp,ar,field) -> left_expression exp
  | Ast0.Cast(lp,ty,attr,rp,exp) -> modif_before_mcode lp
  | Ast0.SizeOfExpr(szf,exp) -> modif_before_mcode szf
  | Ast0.SizeOfType(szf,lp,ty,rp) -> modif_before_mcode szf
  | Ast0.TypeExp(ty) -> left_typeC ty
  | Ast0.Constructor(lp,ty,rp,init) -> modif_before_mcode lp
  | Ast0.MetaErr(name,_,_) -> modif_before_mcode name
  | Ast0.MetaExpr(name,_,ty,_,_,_bitfield) -> modif_before_mcode name
  | Ast0.MetaExprList(name,_,_,_) -> modif_before_mcode name
  | Ast0.EComma(cm) -> modif_before_mcode cm
  | Ast0.DisjExpr(_,exp_list,_,_) -> List.exists left_expression exp_list
  | Ast0.ConjExpr(_,exp_list,_,_) -> List.exists left_expression exp_list
  | Ast0.NestExpr(starter,expr_dots,ender,_,multi) ->
      left_dots left_expression expr_dots
  | Ast0.Edots(dots,_) -> false
  | Ast0.OptExp(exp) -> left_expression exp
  | Ast0.AsExpr _ | Ast0.AsSExpr _ -> failwith "not possible"

(* --------------------------------------------------------------------- *)
(* Types *)

and left_typeC t =
  modif_before t ||
  match Ast0.unwrap t with
    Ast0.ConstVol(cv,ty) -> modif_before_mcode cv
  | Ast0.BaseType(ty,strings) -> modif_before_mcode (List.hd strings)
  | Ast0.Signed(sgn,ty) -> modif_before_mcode sgn
  | Ast0.Pointer(ty,star) -> left_typeC ty
  | Ast0.ParenType(lp,ty,rp) -> modif_before_mcode lp
  | Ast0.FunctionType(ty,lp,params,rp) -> left_typeC ty
  | Ast0.Array(ty,lb,size,rb) -> left_typeC ty
  | Ast0.Decimal(dec,lp,length,comma,precision_opt,rp) ->
      modif_before_mcode dec
  | Ast0.EnumName(kind,name) -> modif_before_mcode kind
  | Ast0.EnumDef(ty,lb,ids,rb) -> left_typeC ty
  | Ast0.StructUnionName(kind,name) -> modif_before_mcode kind
  | Ast0.StructUnionDef(ty,lb,decls,rb) -> left_typeC ty
  | Ast0.TypeOfExpr(tf,_,_,_) -> modif_before_mcode tf
  | Ast0.TypeOfType(tf,_,_,_) -> modif_before_mcode tf
  | Ast0.TypeName(name) -> modif_before_mcode name
  | Ast0.AutoType(auto) -> modif_before_mcode auto
  | Ast0.MetaType(name,_,_) -> modif_before_mcode name
  | Ast0.DisjType(lp,types,mids,rp) -> List.exists left_typeC types
  | Ast0.ConjType(lp,types,mids,rp) -> List.exists left_typeC types
  | Ast0.OptType(ty) -> left_typeC ty
  | Ast0.AsType _ -> failwith "not possible"

(* --------------------------------------------------------------------- *)
(* Variable declaration *)
(* Even if the Cocci program specifies a list of declarations, they are
   split out into multiple declarations of a single variable each. *)

and left_declaration d =
  modif_before d ||
  match Ast0.unwrap d with
    Ast0.MetaDecl(name,_,_) -> modif_before_mcode name
  | Ast0.Init(Some stg,ty,id,attr,eq,ini,sem) -> modif_before_mcode stg
  | Ast0.Init(None,ty,id,attr,eq,ini,sem) -> left_typeC ty
  | Ast0.UnInit(Some stg,ty,id,attr,sem) -> modif_before_mcode stg
  | Ast0.UnInit(None,ty,id,attr,sem) -> left_typeC ty
  | Ast0.FunProto(fninfo,name,lp1,params,va,rp1,sem) ->
      (* should not be nested in anything anyway *)
      false
  | Ast0.MacroDecl(Some stg,name,lp,args,rp,attr,sem) -> modif_before_mcode stg
  | Ast0.MacroDecl(None,name,lp,args,rp,attr,sem) -> left_ident name
  | Ast0.MacroDeclInit(Some stg,name,lp,args,rp,eq,ini,sem) ->
      modif_before_mcode stg
  | Ast0.MacroDeclInit(None,name,lp,args,rp,eq,ini,sem) -> left_ident name
  | Ast0.TyDecl(ty,attr,sem) -> left_typeC ty
  | Ast0.Typedef(stg,ty,id,sem) -> modif_before_mcode stg
  | Ast0.DisjDecl(_,decls,_,_) -> List.exists left_declaration decls
  | Ast0.ConjDecl(_,decls,_,_) -> List.exists left_declaration decls
  | Ast0.OptDecl(decl) -> left_declaration decl
  | Ast0.AsDecl _ -> failwith "not possible"

and right_declaration d =
  modif_before d ||
  match Ast0.unwrap d with
    Ast0.MetaDecl(name,_,_) -> modif_before_mcode name
  | Ast0.Init(_,ty,id,eq,ini,attr,sem) -> modif_after_mcode sem
  | Ast0.UnInit(_,ty,id,attr,sem) -> modif_after_mcode sem
  | Ast0.FunProto(fninfo,name,lp1,params,va,rp1,sem) -> modif_after_mcode sem
  | Ast0.MacroDecl(_,name,lp,args,rp,attr,sem) -> modif_after_mcode sem
  | Ast0.MacroDeclInit(_,name,lp,args,rp,eq,ini,sem) -> modif_after_mcode sem
  | Ast0.TyDecl(ty,attr,sem) -> modif_after_mcode sem
  | Ast0.Typedef(stg,ty,id,sem) -> modif_after_mcode sem
  | Ast0.DisjDecl(_,decls,_,_) -> List.exists right_declaration decls
  | Ast0.ConjDecl(_,decls,_,_) -> List.exists right_declaration decls
  | Ast0.OptDecl(decl) -> right_declaration decl
  | Ast0.AsDecl _ -> failwith "not possible"

(* --------------------------------------------------------------------- *)
(* Field declaration *)

and left_field d =
  modif_before d ||
  match Ast0.unwrap d with
    Ast0.MetaField(name,_,_)
  | Ast0.MetaFieldList(name,_,_,_) ->
      modif_before_mcode name
  | Ast0.Field(ty,id,_bf,sem) -> left_typeC ty
  | Ast0.DisjField(_,decls,_,_) -> List.exists left_field decls
  | Ast0.ConjField(_,decls,_,_) -> List.exists left_field decls
  | Ast0.OptField(decl) -> left_field decl
  | Ast0.Fdots(dots,_) -> false

and right_field d =
  modif_before d ||
  match Ast0.unwrap d with
    Ast0.MetaField(name,_,_)
  | Ast0.MetaFieldList(name,_,_,_) ->
      modif_before_mcode name
  | Ast0.Field(ty,id,_bf,sem) -> modif_after_mcode sem
  | Ast0.DisjField(_,decls,_,_) -> List.exists right_field decls
  | Ast0.ConjField(_,decls,_,_) -> List.exists right_field decls
  | Ast0.OptField(decl) -> right_field decl
  | Ast0.Fdots(dots,_) -> false

(* --------------------------------------------------------------------- *)
(* Top-level code *)
(* These functions seem to be never used
and left_statement s =
  modif_before s or
  match Ast0.unwrap s with
    Ast0.FunDecl(_,fninfo,name,lp,params,rp,lbrace,body,rbrace) ->
      (* irrelevant *) false
  | Ast0.Decl(_,decl) -> left_declaration decl
  | Ast0.Seq(lbrace,body,rbrace) -> modif_before_mcode lbrace
  | Ast0.ExprStatement(Some exp,sem) -> left_expression exp
  | Ast0.ExprStatement(None,sem) -> modif_before_mcode sem
  | Ast0.IfThen(iff,lp,exp,rp,branch1,aft) -> modif_before_mcode iff
  | Ast0.IfThenElse(iff,lp,exp,rp,branch1,els,branch2,aft) ->
      modif_before_mcode iff
  | Ast0.While(whl,lp,exp,rp,body,aft) -> modif_before_mcode whl
  | Ast0.Do(d,body,whl,lp,exp,rp,sem) -> modif_before_mcode d
  | Ast0.For(fr,lp,first,e2,sem2,e3,rp,body,aft) ->
      modif_before_mcode fr
  | Ast0.Iterator(nm,lp,args,rp,body,aft) -> left_ident nm
  | Ast0.Switch(switch,lp,exp,rp,lb,decls,cases,rb) ->
      modif_before_mcode switch
  | Ast0.Break(br,sem) -> modif_before_mcode br
  | Ast0.Continue(cont,sem) -> modif_before_mcode cont
  | Ast0.Label(l,dd) -> left_ident l
  | Ast0.Goto(goto,l,sem) -> modif_before_mcode goto
  | Ast0.Return(ret,sem) -> modif_before_mcode ret
  | Ast0.ReturnExpr(ret,exp,sem) -> modif_before_mcode ret
  | Ast0.Exec(exec,lang,code,sem) -> modif_before_mcode exec
  | Ast0.MetaStmt(name,pure) -> modif_before_mcode name
  | Ast0.MetaStmtList(name,_,_) -> modif_before_mcode name
  | Ast0.Disj(_,statement_dots_list,_,_) ->
      List.exists (left_dots left_statement) statement_dots_list
  | Ast0.Nest(starter,stmt_dots,ender,whencode,multi) ->
      left_dots left_statement stmt_dots
  | Ast0.Exp(exp) -> false (* can only be replaced by an expression *)
  | Ast0.TopExp(exp) -> false (* as above *)
  | Ast0.Ty(ty) -> false (* can only be replaced by a type *)
  | Ast0.TopId(id) -> false (* can only be replaced by an ident *)
  | Ast0.TopInit(init) -> false (* can only be replaced by an init *)
  | Ast0.Dots(d,whn) -> false
  | Ast0.Include(inc,s) -> modif_before_mcode inc
  | Ast0.MetaInclude(inc,s) -> modif_before_mcode inc
  | Ast0.Undef(def,id) -> modif_before_mcode def
  | Ast0.Define(def,id,params,body) -> modif_before_mcode def
  | Ast0.Pragma(prg,id,body) -> modif_before_mcode prg
  | Ast0.OptStm(re) -> left_statement re
  | Ast0.AsStmt _ -> failwith "not possible"

and right_statement s =
  modif_after s or
  match Ast0.unwrap s with
    Ast0.FunDecl(_,fninfo,name,lp,params,rp,lbrace,body,rbrace) ->
      (* irrelevant *) false
  | Ast0.Decl(_,decl) -> right_declaration decl
  | Ast0.Seq(lbrace,body,rbrace) -> modif_after_mcode rbrace
  | Ast0.ExprStatement(exp,sem) -> modif_after_mcode sem
  | Ast0.IfThen(iff,lp,exp,rp,branch1,(_,aft,_)) -> modif_after_mcodekind aft
  | Ast0.IfThenElse(iff,lp,exp,rp,branch1,els,branch2,(_,aft,_)) ->
      modif_after_mcodekind aft
  | Ast0.While(whl,lp,exp,rp,body,(_,aft,_)) -> modif_after_mcodekind aft
  | Ast0.Do(d,body,whl,lp,exp,rp,sem) -> modif_after_mcode sem
  | Ast0.For(fr,lp,first,e2,sem2,e3,rp,body,(_,aft,_)) ->
      modif_after_mcodekind aft
  | Ast0.Iterator(nm,lp,args,rp,body,(_,aft,_)) ->
      modif_after_mcodekind aft
  | Ast0.Switch(switch,lp,exp,rp,lb,decls,cases,rb) -> modif_after_mcode rb
  | Ast0.Break(br,sem) -> modif_after_mcode sem
  | Ast0.Continue(cont,sem) -> modif_after_mcode sem
  | Ast0.Label(l,dd) -> modif_after_mcode dd
  | Ast0.Goto(goto,l,sem) -> modif_after_mcode sem
  | Ast0.Return(ret,sem) -> modif_after_mcode sem
  | Ast0.ReturnExpr(ret,exp,sem) -> modif_after_mcode sem
  | Ast0.Exec(exec,lang,code,sem) -> modif_after_mcode sem
  | Ast0.MetaStmt(name,pure) -> modif_after_mcode name
  | Ast0.MetaStmtList(name,_,_) -> modif_after_mcode name
  | Ast0.Disj(_,statement_dots_list,_,_) ->
      List.exists (right_dots right_statement) statement_dots_list
  | Ast0.Nest(starter,stmt_dots,ender,whencode,multi) ->
      right_dots right_statement stmt_dots
  | Ast0.Exp(exp) -> false (* can only be replaced by an expression *)
  | Ast0.TopExp(exp) -> false (* as above *)
  | Ast0.Ty(ty) -> false (* can only be replaced by a type *)
  | Ast0.TopId(id) -> false (* can only be replaced by a type *)
  | Ast0.TopInit(init) -> false (* can only be replaced by an init *)
  | Ast0.Dots(d,whn) -> false
  | Ast0.Include(inc,s) -> modif_after_mcode s
  | Ast0.MetaInclude(inc,s) -> false (* irrelevant, not a substatement *)
  | Ast0.Undef(def,id) -> right_ident id
  | Ast0.Define(def,id,params,body) -> right_dots right_statement body
  | Ast0.Pragma(prg,id,body) -> right_pragma body -- not defined, b/c not used
  | Ast0.OptStm(re) -> right_statement re
  | Ast0.AsStmt _ -> failwith "not possible"
*)

(* --------------------------------------------------------------------- *)


(* A very coarse approximation.  We would really only like to return true
if a new statement is added.  For this it would be best to correlate with the
plus slice. Or at least be sure that the new stuff is on the far left or
far right. *)

let rec adding_something s =
  match Ast0.get_mcodekind s with
    Ast0.MINUS(mc) ->
      (match !mc with
	(* do better for the common case of replacing a stmt by another one *)
	((Ast.REPLACEMENT([[Ast.StatementTag(s)]],c)) as old,ti) ->
	  (match Ast.unwrap s with
	    Ast.IfThen(_,_,_) -> true (* potentially dangerous *)
	  | _ -> mc := (old,ti); false)
      |	(_,_) -> true)
  | Ast0.CONTEXT(mc) ->
      let (text,tinfo1,tinfo2) = !mc in
      (match text with Ast.NOTHING -> false | _ -> true)
  | Ast0.MIXED(_) ->
      not(contains_only_minus.VT0.combiner_rec_statement s) (*&&
      (left_statement s) or (right_statement s)*)
  | _ -> failwith "unexpected plus code"

(* why do we need this; MINUS should mean the same thing *)
and contains_only_minus =
  let bind x y = x && y in
  let option_default = true in
  let mcodekind = function
      Ast0.MINUS(mc) ->
	(match !mc with
	  (Ast.NOREPLACEMENT,_) -> true
	| _ -> false)
    | Ast0.CONTEXT(mc) -> false
    | _ -> false in
  let mcode (_,_,_,mc,_,_) = mcodekind mc in

  let donothing r k e = mcodekind (Ast0.get_mcodekind e) && k e in

  let dots r k e =
    match Ast0.unwrap e with
      [] -> true
    | _ -> k e in

  let identifier r k e =
    mcodekind (Ast0.get_mcodekind e) &&
    match Ast0.unwrap e with
      Ast0.DisjId(starter,id_list,mids,ender) ->
	List.for_all r.VT0.combiner_rec_ident id_list
    | Ast0.ConjId(starter,id_list,mids,ender) ->
	List.exists r.VT0.combiner_rec_ident id_list
    | _ -> k e in

  let expression r k e =
    mcodekind (Ast0.get_mcodekind e) &&
    match Ast0.unwrap e with
      Ast0.DisjExpr(starter,expr_list,mids,ender) ->
	List.for_all r.VT0.combiner_rec_expression expr_list
    | Ast0.ConjExpr(starter,expr_list,mids,ender) ->
	List.exists r.VT0.combiner_rec_expression expr_list
    | _ -> k e in

  let declaration r k e =
    mcodekind (Ast0.get_mcodekind e) &&
    match Ast0.unwrap e with
      Ast0.DisjDecl(starter,decls,mids,ender) ->
	List.for_all r.VT0.combiner_rec_declaration decls
    | Ast0.ConjDecl(starter,decls,mids,ender) ->
	List.exists r.VT0.combiner_rec_declaration decls
    | _ -> k e in

  let field r k e =
    mcodekind (Ast0.get_mcodekind e) &&
    match Ast0.unwrap e with
      Ast0.DisjField(starter,decls,mids,ender) ->
	List.for_all r.VT0.combiner_rec_field decls
    | Ast0.ConjField(starter,decls,mids,ender) ->
	List.exists r.VT0.combiner_rec_field decls
    | _ -> k e in

  let typeC r k e =
    mcodekind (Ast0.get_mcodekind e) &&
    match Ast0.unwrap e with
      Ast0.DisjType(starter,types,mids,ender) ->
	List.for_all r.VT0.combiner_rec_typeC types
    | Ast0.ConjType(starter,types,mids,ender) ->
	List.exists r.VT0.combiner_rec_typeC types
    | _ -> k e in

  let statement r k e =
    mcodekind (Ast0.get_mcodekind e) &&
    match Ast0.unwrap e with
      Ast0.Disj(starter,statement_dots_list,mids,ender) ->
	List.for_all r.VT0.combiner_rec_statement_dots statement_dots_list
    | Ast0.Conj(starter,statement_dots_list,mids,ender) ->
	List.exists r.VT0.combiner_rec_statement_dots statement_dots_list
    | _ -> k e in

  let case_line r k e =
    mcodekind (Ast0.get_mcodekind e) &&
    match Ast0.unwrap e with
      Ast0.DisjCase(starter,case_list,mids,ender) ->
	List.for_all r.VT0.combiner_rec_case_line case_list
    | _ -> k e in

  V0.flat_combiner bind option_default
    mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
    mcode mcode mcode
    dots dots dots dots dots dots dots dots dots
    identifier expression donothing donothing typeC donothing donothing
    declaration field donothing statement donothing case_line donothing
    donothing donothing donothing


(* needs a special case when there is a Disj or an empty DOTS *)
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
    (Iso_pattern.rebuild_mcode None).VT0.rebuilder_rec_statement orig_s in
  let new_mcodekind =
    let add_times = Ast.ONE in
    match Ast0.get_mcodekind s with
      Ast0.MINUS(mc) ->
	let (text,tinfo) = !mc in
	let inner_text =
	  match text with
	    Ast.NOREPLACEMENT -> [[Ast.mkToken "{}"]]
	  | Ast.REPLACEMENT(anythings,Ast.ONE) ->
	      [Ast.mkToken "{"]::anythings@[[Ast.mkToken "}"]]
	  | Ast.REPLACEMENT(anythings,Ast.MANY) ->
	      failwith "++ not supported when braces must be added" in
	Ast0.MINUS(ref(Ast.REPLACEMENT(inner_text,Ast.ONE),tinfo))
    | Ast0.CONTEXT(mc) ->
	let (text,tinfo1,tinfo2) = !mc in
	let new_text =
	  (* this is going to be a mess if we allow it to be iterable...
	     there would be one level of braces for every added things.
	     need to come up with something better, or just add {} in the
	     source code. *)
	  match text with
	    Ast.BEFORE(bef,_) ->
	      Ast.BEFOREAFTER([Ast.mkToken "{"]::bef,[[Ast.mkToken "}"]],
			      add_times)
	  | Ast.AFTER(aft,_) ->
	      Ast.BEFOREAFTER([[Ast.mkToken "{"]],aft@[[Ast.mkToken "}"]],
			      add_times)
	  | Ast.BEFOREAFTER(bef,aft,_) ->
	      Ast.BEFOREAFTER([Ast.mkToken "{"]::bef,aft@[[Ast.mkToken "}"]],
			      add_times)
	  | Ast.NOTHING ->
	      Ast.BEFOREAFTER([[Ast.mkToken "{"]],[[Ast.mkToken "}"]],
			      add_times) in
	Ast0.CONTEXT(ref(new_text,tinfo1,tinfo2))
    | Ast0.MIXED(mc) ->
	let (text,tinfo1,tinfo2) = !mc in
	let new_text =
	  match text with
	    Ast.BEFORE(bef,_) ->
	      Ast.BEFOREAFTER([Ast.mkToken "{"]::bef,[[Ast.mkToken "}"]],
			      add_times)
	  | Ast.AFTER(aft,_) ->
	      Ast.BEFOREAFTER([[Ast.mkToken "{"]],aft@[[Ast.mkToken "}"]],
			      add_times)
	  | Ast.BEFOREAFTER(bef,aft,_) ->
	      Ast.BEFOREAFTER([Ast.mkToken "{"]::bef,aft@[[Ast.mkToken "}"]],
			      add_times)
	  | Ast.NOTHING ->
	      Ast.BEFOREAFTER([[Ast.mkToken "{"]],[[Ast.mkToken "}"]],
			      add_times) in
	Ast0.MIXED(ref(new_text,tinfo1,tinfo2))
    | _ -> failwith "unexpected plus code" in
  Ast0.set_mcodekind s new_mcodekind;
  Compute_lines.compute_statement_lines true s

(* ---------------------------------------------------------------------- *)

let is_dots x =
  match Ast0.unwrap x with
    Ast0.Dots(_,_) | Ast0.Nest(_,_,_,_,_) -> true
  | _ -> false

let all_minus s =
  match Ast0.get_mcodekind s with
    Ast0.MINUS(_) -> true
  | _ -> false

let unchanged_minus s =
  match Ast0.get_mcodekind s with
    Ast0.MINUS(mc) ->
      (match !mc with (Ast.NOREPLACEMENT,_) -> true | _ -> false)
  | _ -> false

let rec do_branch s =
  if unchanged_minus s
  then
    Ast0.set_dots_bef_aft s (Ast0.DroppingBetweenDots(add_braces s))
  else
    match Ast0.unwrap s with
      Ast0.Disj(starter,statement_dots_list,mids,ender) ->
	let stmts =
	  List.map
	    (function s ->
	      match Ast0.unwrap s with
		[s] -> Ast0.rewrap s [do_branch s]
	      |	_ -> s)
	    statement_dots_list in
	Ast0.rewrap s (Ast0.Disj(starter,stmts,mids,ender))
    | _ -> s

let rec statement dots_before dots_after s =
  let do_one s =
    if dots_before && dots_after
    then
      if unchanged_minus s
      then
	(let with_braces = add_braces s in
	Ast0.set_dots_bef_aft s (Ast0.DroppingBetweenDots(with_braces)))
      else if adding_something s
      then
	let with_braces = add_braces s in
	Ast0.set_dots_bef_aft s (Ast0.AddingBetweenDots(with_braces))
      else s
    else s in

  match Ast0.unwrap s with
    Ast0.FunDecl(x,fninfo,name,lp,params,va,rp,lbrace,body,rbrace,y) ->
      (* true for close brace, because that represents any way we can
	 exit the function, which is not necessarily followed by an explicit
	 close brace. *)
      Ast0.rewrap s
	(Ast0.FunDecl(x,fninfo,name,lp,params,va,rp,lbrace,
		      statement_dots false true body,
		      rbrace,y))
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
	      (iff,lp,exp,rp,
		statement false false branch1,els,
		statement false false branch2,x)))
  | Ast0.While(whl,lp,exp,rp,body,x) ->
      do_one
	(Ast0.rewrap s
	   (Ast0.While(whl,lp,exp,rp,statement false false body,x)))
  | Ast0.Do(d,body,whl,lp,exp,rp,sem) ->
      do_one
	(Ast0.rewrap s
	   (Ast0.Do(d,statement false false body,whl,lp,exp,rp,sem)))
  | Ast0.For(fr,lp,first,e2,sem2,e3,rp,body,x) ->
      do_one
	(Ast0.rewrap s
	   (Ast0.For(fr,lp,first,e2,sem2,e3,rp,
		     statement false false body,x)))
  | Ast0.Iterator(nm,lp,args,rp,body,x) ->
      do_one
	(Ast0.rewrap s
	   (Ast0.Iterator(nm,lp,args,rp,statement false false body,x)))
  | Ast0.Switch(switch,lp,exp,rp,lb,decls,cases,rb) ->
      do_one
	(Ast0.rewrap s
	   (Ast0.Switch(switch,lp,exp,rp,lb,decls,
			Ast0.rewrap cases
			  (List.map case_line (Ast0.unwrap cases)),
			rb)))
  | Ast0.Break(br,sem) -> do_one s
  | Ast0.Continue(cont,sem) -> do_one s
  | Ast0.Label(l,dd) -> do_one s
  | Ast0.Goto(goto,l,sem) -> do_one s
  | Ast0.Return(ret,sem) -> do_one s
  | Ast0.ReturnExpr(ret,exp,sem) -> do_one s
  | Ast0.Exec(exec,lang,code,sem) -> do_one s
  | Ast0.MetaStmt(name,_,_) -> do_one s
  | Ast0.MetaStmtList(name,_,_,_) -> do_one s
  | Ast0.Disj(starter,statement_dots_list,mids,ender) ->
      Ast0.rewrap s
	(Ast0.Disj(starter,
		   List.map (statement_dots dots_before dots_after)
		     statement_dots_list,
		   mids,ender))
  | Ast0.Conj(starter,statement_dots_list,mids,ender) ->
      (match statement_dots_list with
	s1::ss ->
	  Ast0.rewrap s
	    (Ast0.Conj(starter,
		       (statement_dots dots_before dots_after s1) ::
		       (List.map (statement_dots false false) ss),
		       mids,ender))
      |	_ -> s)
  | Ast0.Nest(starter,stmt_dots,ender,whencode,multi) ->
      (match Ast0.get_mcode_mcodekind starter with
	Ast0.MINUS _ -> (* everything removed, like -... *) s
      |	_ ->
	  Ast0.rewrap s
	    (Ast0.Nest
	       (starter,statement_dots true true stmt_dots,ender,
		whencode,multi)))
  | Ast0.Exp(exp) -> s
  | Ast0.TopExp(exp) -> s
  | Ast0.Ty(ty) -> s
  | Ast0.TopId(id) -> s
  | Ast0.TopInit(init) -> s
  | Ast0.Dots(d,whn) -> s
  | Ast0.Include(inc,string) -> s (* doesn't affect the need for braces *)
  | Ast0.MetaInclude(inc,name) -> s (* doesn't affect the need for braces *)
  | Ast0.Undef(def,id) -> s (* same as include *)
  | Ast0.Define(def,id,params,body) -> s (* same as include *)
  | Ast0.Pragma(prg,id,body) -> s (* same as include *)
  | Ast0.OptStm(re) ->
      Ast0.rewrap s
	(Ast0.OptStm(statement dots_before dots_after re))
  | Ast0.AsStmt _ -> failwith "not possible"

and case_line c =
  Ast0.rewrap c
    (match Ast0.unwrap c with
      Ast0.Default(def,colon,code) ->
	Ast0.Default(def,colon,statement_dots false false code)
    | Ast0.Case(case,exp,colon,code) ->
	Ast0.Case(case,exp,colon,statement_dots false false code)
    | Ast0.DisjCase(starter,case_lines,mids,ender) ->
	Ast0.DisjCase(starter,List.map case_line case_lines,mids,ender)
    | Ast0.OptCase(case) -> Ast0.OptCase(case_line c))

and do_statement_dots dots_before dots_after = function
    [] -> []
  | [x] -> [statement dots_before dots_after x]
  | dots::rest when is_dots dots ->
      dots::(do_statement_dots true dots_after rest)
  | x::(dots::_ as rest) when is_dots dots ->
      (statement dots_before true x)::
      do_statement_dots false dots_after rest
  | x::rest ->
      (statement dots_before false x)::
      do_statement_dots false dots_after rest

and statement_dots dots_before dots_after d =
  Ast0.rewrap d (do_statement_dots dots_before dots_after (Ast0.unwrap d))

let top_level t =
  Ast0.rewrap t
    (match Ast0.unwrap t with
      Ast0.NONDECL(stmt_dots) -> Ast0.NONDECL(statement true true stmt_dots)
    | Ast0.CODE(stmt_dots) -> Ast0.CODE(statement_dots true true stmt_dots)
    | t -> t)

let single_statement l =
  if !Flag_parsing_cocci.sgrep_mode then l else List.map top_level l
