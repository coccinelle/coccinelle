(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at https://coccinelle.gitlabpages.inria.fr/website
 *)

(* Find a directive or comment at the end of a statement.  Things with aft
given None, because they can accommodate their own directives or comments *)

module Ast0 = Ast0_cocci
module Ast = Ast_cocci
module V0 = Visitor_ast0
module VT0 = Visitor_ast0_types

let call_right processor data s cont =
  match processor data with
    None -> None
  | Some(pragmas,data) -> Some (pragmas,Ast0.rewrap s (cont data))

let left_mcode (a,b,info,mcodekind,d,e) =
  match (info.Ast0.strings_before,mcodekind) with
    ([],_) | (_,Ast0.PLUS _) -> None
  | (l,_) -> Some(l,(a,b,{info with Ast0.strings_before = []},mcodekind,d,e))

let right_mcode (a,b,info,mcodekind,d,e) =
  match (info.Ast0.strings_after,mcodekind) with
    ([],_) | (_,Ast0.PLUS _) -> None
  | (l,_) -> Some(l,(a,b,{info with Ast0.strings_after = []},mcodekind,d,e))

let update_before pragmas (info,x,adj) =
  ({info with Ast0.strings_before = pragmas @ info.Ast0.strings_before},
   Ast0.PLUS Ast.ONE,adj)(*not sure what the arg should be... one seems safe*)

let update_before2 pragmas (info,x) =
  ({info with Ast0.strings_before = pragmas @ info.Ast0.strings_before},
   Ast0.PLUS Ast.ONE)(*not sure what the arg should be... one seems safe*)

let update_after pragmas (info,x) =
  ({info with Ast0.strings_after = info.Ast0.strings_after @ pragmas},
   Ast0.PLUS Ast.ONE) (*not sure what the arg should be... one seems safe*)

let rec right_decl d =
  match Ast0.unwrap d with
    Ast0.MetaDecl(name,cstr,pure) ->
      call_right right_mcode name d
	(function name -> Ast0.MetaDecl(name,cstr,pure))
  | Ast0.AsDecl(decl,asdecl) -> failwith "not possible"
  | Ast0.Init(align,stg,ty,id,endattr,eq,ini,Some sem) ->
      call_right right_mcode sem d
	(function sem -> Ast0.Init(align,stg,ty,id,endattr,eq,ini,Some sem))
  | Ast0.Init(align,stg,ty,id,endattr,eq,ini,None) ->
      (* only in a while condition, so not reachable *)
      failwith "The entry point is via statements, and this can't be rightmost in a statement"
  | Ast0.UnInit(align,stg,ty,id,endattr,sem) ->
      call_right right_mcode sem d
	(function sem -> Ast0.UnInit(align,stg,ty,id,endattr,sem))
  | Ast0.FunProto(fninfo,id,lp1,params,va,rp1,sem) ->
      call_right right_mcode sem d
	(function ty -> Ast0.FunProto(fninfo,id,lp1,params,va,rp1,sem))
  | Ast0.MacroDecl(stg,preattr,name,lp,args,rp,attr,sem) ->
      call_right right_mcode sem d
	(function sem -> Ast0.MacroDecl(stg,preattr,name,lp,args,rp,attr,sem))
  | Ast0.MacroDeclInit(stg,preattr,name,lp,args,rp,attr,eq,ini,sem) ->
      call_right right_mcode sem d
	(function sem -> Ast0.MacroDeclInit(stg,preattr,name,lp,args,rp,attr,eq,ini,sem))
  | Ast0.TyDecl(ty,sem) ->
      call_right right_mcode sem d
	(function sem -> Ast0.TyDecl(ty,sem))
  | Ast0.Typedef(stg,ty,id,sem) ->
      call_right right_mcode sem d
	(function sem -> Ast0.Typedef(stg,ty,id,sem))
  | Ast0.DisjDecl(starter,decls,mids,ender)
  | Ast0.ConjDecl(starter,decls,mids,ender) -> None
  | Ast0.OptDecl(decl) ->
      call_right right_decl decl d (function decl -> Ast0.OptDecl(decl))

let rec right_statement s =
  match Ast0.unwrap s with
    Ast0.FunDecl(bef,fi,name,lp,params,va,rp,attrs,lbrace,body,rbrace,aft) -> None
  | Ast0.TemplateDefinition(_,_,_,_,_) -> None
  | Ast0.Decl(bef,decl) ->
      call_right right_decl decl s
	(function decl -> Ast0.Decl(bef,decl))
  | Ast0.Seq(lbrace,body,rbrace) ->
      call_right right_mcode rbrace s
	(function rbrace -> Ast0.Seq(lbrace,body,rbrace))
  | Ast0.ExprStatement(exp,sem) ->
      call_right right_mcode sem s
	(function sem -> Ast0.ExprStatement(exp,sem))
  | Ast0.IfThen(iff,lp,exp,rp,branch1,aft) -> None
  | Ast0.IfThenElse(iff,lp,exp,rp,branch1,els,branch2,aft) -> None
  | Ast0.While(whl,lp,exp,rp,body,aft) -> None
  | Ast0.Do(d,body,whl,lp,exp,rp,sem) ->
      call_right right_mcode sem s
	(function sem -> Ast0.Do(d,body,whl,lp,exp,rp,sem))
  | Ast0.For(fr,lp,first,rp,body,aft) -> None
  | Ast0.ScopedGuard(sg,lp,exp,rp,body,aft) -> None
  | Ast0.Iterator(nm,lp,args,rp,body,aft) -> None
  | Ast0.Switch(switch,lp,exp,rp,lb,decls,cases,rb) ->
      call_right right_mcode rb s
	(function rb -> Ast0.Switch(switch,lp,exp,rp,lb,decls,cases,rb))
  | Ast0.Break(br,sem) ->
      call_right right_mcode sem s
	(function sem -> Ast0.Break(br,sem))
  | Ast0.Continue(cont,sem) ->
      call_right right_mcode sem s
	(function sem -> Ast0.Continue(cont,sem))
  | Ast0.Label(l,dd) ->
      call_right right_mcode dd s
	(function dd -> Ast0.Label(l,dd))
  | Ast0.Goto(goto,l,sem) ->
      call_right right_mcode sem s
	(function sem -> Ast0.Goto(goto,l,sem))
  | Ast0.Return(ret,sem) ->
      call_right right_mcode sem s
	(function sem -> Ast0.Return(ret,sem))
  | Ast0.ReturnExpr(ret,exp,sem) ->
      call_right right_mcode sem s
	(function sem -> Ast0.ReturnExpr(ret,exp,sem))
  | Ast0.Exec(exec,lang,exp,sem) ->
      call_right right_mcode sem s
	(function sem -> Ast0.Exec(exec,lang,exp,sem))
  | Ast0.MetaStmt(name,cstr,pure) ->
      call_right right_mcode name s
	(function name -> Ast0.MetaStmt(name,cstr,pure))
  | Ast0.MetaStmtList(name,lenname,cstr,pure) ->
      call_right right_mcode name s
	(function name -> Ast0.MetaStmtList(name,lenname,cstr,pure))
  | Ast0.AsStmt(stm,asstm) -> failwith "not possible"
  | Ast0.Disj(starter,statement_dots_list,mids,ender)
  | Ast0.Conj(starter,statement_dots_list,mids,ender) -> None
  | Ast0.Nest(starter,stmt_dots,ender,whn,multi) -> None
  (* the following are None, because they can't be adjacent to an aft node *)
  | Ast0.Exp(exp) -> None
  | Ast0.TopExp(exp) -> None
  | Ast0.Ty(ty) -> None
  | Ast0.TopId(id) -> None
  | Ast0.TopInit(init) -> None
  | Ast0.Dots(d,whn) -> None
  | Ast0.CppTop(di) -> None
  | Ast0.Undef(def,id) ->
      (* nothing available for ident, and not sure code can appear
	 here anyway *)
      None
  | Ast0.Define(def,id,params,body) ->
      call_right right_statement_dots body s
	(function body -> Ast0.Define(def,id,params,body))
  | Ast0.OptStm(re) ->
      call_right right_statement re s (function re -> Ast0.OptStm(re))

and right_statement_dots sd =
  match Ast0.unwrap sd with
    [] -> failwith "empty statement dots"
  | s::r -> call_right right_statement s sd (function s -> List.rev(s::r))

let rec left_ty t =
  match Ast0.unwrap t with
    Ast0.ConstVol([],ty,cvafter) ->
      call_right left_ty ty t (function ty -> Ast0.ConstVol([],ty,cvafter))
  | Ast0.ConstVol(cv::cvbefore,ty,cvafter) ->
      (match cv with
	Ast0.CV cv ->
	  call_right left_mcode cv t
	    (function cv -> Ast0.ConstVol((Ast0.CV cv)::cvbefore,ty,cvafter))
      | Ast0.Attr attr ->
	  call_right left_attribute attr t
	    (function attr -> Ast0.ConstVol((Ast0.Attr attr)::cvbefore,ty,cvafter)))
  | Ast0.BaseType(ty,strings) ->
      (match strings with
	[] -> failwith "empty strings in type"
      |	s::r ->
	  call_right left_mcode s t (function s -> Ast0.BaseType(ty,s::r)))
  | Ast0.Signed(sign,ty) ->
      call_right left_mcode sign t (function sign -> Ast0.Signed(sign,ty))
  | Ast0.Pointer(ty,star) ->
      call_right left_ty ty t (function ty -> Ast0.Pointer(ty,star))
  | Ast0.ParenType(lp,ty,rp) ->
      call_right left_mcode lp t (function lp -> Ast0.ParenType(lp,ty,rp))
  | Ast0.FunctionType(ty,lp,params,rp) ->
      call_right left_ty ty t (function ty -> Ast0.FunctionType(ty,lp,params,rp))
  | Ast0.Array(ty,lb,size,rb) ->
      call_right left_ty ty t (function ty -> Ast0.Array(ty,lb,size,rb))
  | Ast0.Decimal(dec,lp,length,comma,precision_opt,rp) ->
      call_right left_mcode dec t
	(function dec -> Ast0.Decimal(dec,lp,length,comma,precision_opt,rp))
  | Ast0.EnumName(kind,key,name) ->
      call_right left_mcode kind t (function kind -> Ast0.EnumName(kind,key,name))
  | Ast0.EnumDef(ty,base,lb,ids,rb) ->
      call_right left_ty ty t
	(function ty -> Ast0.EnumDef(ty,base,lb,ids,rb))
  | Ast0.StructUnionName(kind,name) ->
      call_right left_mcode kind t
	(function kind -> Ast0.StructUnionName(kind,name))
  | Ast0.StructUnionDef(ty,lb,decls,rb) ->
      call_right left_ty ty t
	(function ty -> Ast0.StructUnionDef(ty,lb,decls,rb))
  | Ast0.TypeName(typename,name) ->
      call_right left_mcode typename t
	(function typename -> Ast0.TypeName(typename,name))
  | Ast0.TypeOfExpr(tf,lb,e,rb) ->
      call_right left_mcode tf t (function tf -> Ast0.TypeOfExpr(tf,lb,e,rb))
  | Ast0.TypeOfType(tf,lb,ty,rb) ->
      call_right left_mcode tf t (function tf -> Ast0.TypeOfType(tf,lb,ty,rb))
  | Ast0.NamedType(name) ->
      call_right left_mcode name t (function name -> Ast0.NamedType(name))
  | Ast0.QualifiedType(Some ty,coloncolon,name) ->
      call_right left_ty ty t
    (function ty -> Ast0.QualifiedType(Some ty,coloncolon,name))
  | Ast0.QualifiedType(None,coloncolon,name) ->
      call_right left_mcode coloncolon t
    (function coloncolon -> Ast0.QualifiedType(None,coloncolon,name))
  | Ast0.TemplateType(name,lp,args,rp) ->
      call_right left_ty name t (function name ->
          Ast0.TemplateType(name,lp,args,rp))
  | Ast0.AutoType(auto) ->
      call_right left_mcode auto t (function auto -> Ast0.AutoType(auto))
  | Ast0.MetaType(name,cstr,x) ->
      call_right left_mcode name t (function name -> Ast0.MetaType(name,cstr,x))
  | Ast0.AsType(ty,asty) -> failwith "not possible"
  | Ast0.DisjType(starter,types,mids,ender)
  | Ast0.ConjType(starter,types,mids,ender) -> None
  | Ast0.OptType(ty) ->
      call_right left_ty ty t (function ty -> Ast0.OptType(ty))

and left_ident i =
  match Ast0.unwrap i with
    Ast0.Id(name) ->
      call_right left_mcode name i (function name -> Ast0.Id(name))
  | Ast0.MetaId(name,a,b,c) ->
      call_right left_mcode name i (function name -> Ast0.MetaId(name,a,b,c))
  | Ast0.MetaFunc(name,a,b) ->
      call_right left_mcode name i (function name -> Ast0.MetaFunc(name,a,b))
  | Ast0.MetaLocalFunc(name,a,b) ->
      call_right left_mcode name i
	(function name -> Ast0.MetaLocalFunc(name,a,b))
  | Ast0.DisjId(starter,ids,mids,ender)
  | Ast0.ConjId(starter,ids,mids,ender) -> None
  | Ast0.OptIdent(id) ->
      call_right left_ident id i (function id -> Ast0.OptIdent(id))
  | Ast0.AsIdent(id,asid) -> failwith "not possible"

and left_attr_arg arg =
  match Ast0.unwrap arg with
    Ast0.MacroAttr(a) ->
      call_right left_mcode a arg (function a -> Ast0.MacroAttr(a))
  | Ast0.MetaAttr(name,a,b) ->
      call_right left_mcode name arg
        (function name -> Ast0.MetaAttr(name,a,b))
  | Ast0.MacroAttrArgs(attr,lp,args,rp) ->
      call_right left_mcode attr arg (function attr -> Ast0.MacroAttrArgs(attr,lp,args,rp))

and left_attribute attr =
  match Ast0.unwrap attr with
    Ast0.Attribute(a) ->
      call_right left_attr_arg a attr (function a -> Ast0.Attribute(a))
  | Ast0.GccAttribute(attr_,lp1,lp2,arg,rp1,rp2) ->
      call_right left_mcode attr_ attr
        (function attr_ -> Ast0.GccAttribute(attr_,lp1,lp2,arg,rp1,rp2))
  | Ast0.CxxAttribute(lb1,arg,rb1,rb2) ->
      call_right left_mcode lb1 attr
        (function lb1 -> Ast0.CxxAttribute(lb1,arg,rb1,rb2))
  | Ast0.CxxAttributeUsing(lb1,usng,atnm,dotdot,arg,rb1,rb2) ->
      call_right left_mcode lb1 attr
        (function lb1 -> Ast0.CxxAttributeUsing(lb1,usng,atnm,dotdot,arg,rb1,rb2))

let left_fundecl name fninfo =
  let fncall_right processor data cont =
    match processor data with
      None -> None
    | Some(pragmas,data) -> Some (pragmas,cont data,name) in
  match fninfo with
    [] ->
      (match left_ident name with
	None -> None
      |	Some(pragmas,name) -> Some(pragmas,fninfo,name))
  | (Ast0.FStorage sto)::x ->
      fncall_right left_mcode sto (function sto -> (Ast0.FStorage sto)::x)
  | (Ast0.FType ty)::x ->
      fncall_right left_ty ty (function ty -> (Ast0.FType ty)::x)
  | (Ast0.FInline inl)::x ->
      fncall_right left_mcode inl (function inl -> (Ast0.FInline inl)::x)

let rec left_decl decl =
  match Ast0.unwrap decl with
    Ast0.MetaDecl(name,cstr,pure) ->
      call_right right_mcode name decl
	(function name -> Ast0.MetaDecl(name,cstr,pure))
  | Ast0.AsDecl(decl,asdecl) -> failwith "not possible"
  | Ast0.Init(Some(Ast0.Align(align,lp,e,rp)),stg,ty,id,endattr,eq,ini,sem) ->
      call_right left_mcode align decl
	(function align ->
	  Ast0.Init(Some(Ast0.Align(align,lp,e,rp)),stg,ty,id,endattr,eq,ini,sem))
  | Ast0.Init(None,Some stg,ty,id,endattr,eq,ini,sem) ->
      call_right left_mcode stg decl
	(function stg -> Ast0.Init(None,Some stg,ty,id,endattr,eq,ini,sem))
  | Ast0.Init(None,None,ty,id,endattr,eq,ini,sem) ->
      call_right left_ty ty decl
	(function ty -> Ast0.Init(None,None,ty,id,endattr,eq,ini,sem))
  | Ast0.UnInit(Some(Ast0.Align(align,lp,e,rp)),stg,ty,id,endattr,sem) ->
      call_right left_mcode align decl
	(function align ->
	  Ast0.UnInit(Some(Ast0.Align(align,lp,e,rp)),stg,ty,id,endattr,sem))
  | Ast0.UnInit(None,Some stg,ty,id,endattr,sem) ->
      call_right left_mcode stg decl
	(function stg -> Ast0.UnInit(None,Some stg,ty,id,endattr,sem))
  | Ast0.UnInit(None,None,ty,id,endattr,sem) ->
      call_right left_ty ty decl
	(function ty -> Ast0.UnInit(None,None,ty,id,endattr,sem))
  | Ast0.FunProto(fi,name,lp1,params,va,rp1,sem) ->
      (match fi with
	[] ->
          call_right left_ident name decl
	    (function name -> Ast0.FunProto(fi,name,lp1,params,va,rp1,sem))
      | (Ast0.FStorage sto)::x ->
	  call_right left_mcode sto decl
	    (function sto ->
	      Ast0.FunProto((Ast0.FStorage sto)::x,name,lp1,params,va,rp1,sem))
      |	(Ast0.FType ty)::x ->
	  call_right left_ty ty decl
	    (function ty ->
	      Ast0.FunProto((Ast0.FType ty)::x,name,lp1,params,va,rp1,sem))
      | (Ast0.FInline inl)::x ->
	  call_right left_mcode inl decl
	    (function inl ->
	      Ast0.FunProto((Ast0.FInline inl)::x,name,lp1,params,va,rp1,sem)))
  | Ast0.MacroDecl(Some stg,preattr,name,lp,args,rp,attr,sem) ->
      call_right left_mcode stg decl
	(function stg -> Ast0.MacroDecl(Some stg,preattr,name,lp,args,rp,attr,sem))
  | Ast0.MacroDecl(None,[],name,lp,args,rp,attr,sem) ->
      call_right left_ident name decl
	(function name -> Ast0.MacroDecl(None,[],name,lp,args,rp,attr,sem))
  | Ast0.MacroDecl(None,preattr::l,name,lp,args,rp,attr,sem) ->
      call_right left_attribute preattr decl
	(function preattr -> Ast0.MacroDecl(None,preattr::l,name,lp,args,rp,attr,sem))
  | Ast0.MacroDeclInit(Some stg,preattr,name,lp,args,rp,attr,eq,ini,sem) ->
      call_right left_mcode stg decl
	(function stg ->
	  Ast0.MacroDeclInit(Some stg,preattr,name,lp,args,rp,attr,eq,ini,sem))
  | Ast0.MacroDeclInit(None,[],name,lp,args,rp,attr,eq,ini,sem) ->
      call_right left_ident name decl
	(function name -> Ast0.MacroDeclInit(None,[],name,lp,args,rp,attr,eq,ini,sem))
  | Ast0.MacroDeclInit(None,preattr::l,name,lp,args,rp,attr,eq,ini,sem) ->
      call_right left_attribute preattr decl
	(function preattr -> Ast0.MacroDeclInit(None,preattr::l,name,lp,args,rp,attr,eq,ini,sem))
  | Ast0.TyDecl(ty,sem) ->
      call_right left_ty ty decl (function ty -> Ast0.TyDecl(ty,sem))
  | Ast0.Typedef(stg,ty,id,sem) ->
      call_right left_mcode stg decl
	(function stg -> Ast0.Typedef(stg,ty,id,sem))
  | Ast0.DisjDecl(starter,decls,mids,ender)
  | Ast0.ConjDecl(starter,decls,mids,ender) -> None
  | Ast0.OptDecl(d) ->
      call_right left_decl d decl (function decl -> Ast0.OptDecl(decl))

let process =
  let statement r k s =
    let s = k s in
    Ast0.rewrap s
      (match Ast0.unwrap s with
	Ast0.FunDecl(bef,fi,name,lp,params,va,rp,attrs,lbrace,body,rbrace,aft) ->
	  let (rbrace,aft) =
	    match right_mcode rbrace with
	      None -> (rbrace,aft)
	    | Some (pragmas,rbrace) -> (rbrace,update_after pragmas aft) in
	  (match left_fundecl name fi with
	      None ->
		Ast0.FunDecl(bef,fi,name,lp,params,va,rp,attrs,lbrace,body,rbrace,aft)
	    | Some (pragmas,fi,name) ->
		Ast0.FunDecl
		  (update_before2 pragmas bef,
		   fi,name,lp,params,va,rp,attrs,lbrace,body,rbrace,aft))
      | Ast0.Decl(bef,decl) ->
	  (match left_decl decl with
	    None -> Ast0.unwrap s
	  | Some (pragmas,decl) ->
	      Ast0.Decl(update_before2 pragmas bef,decl))
      | Ast0.IfThen(iff,lp,exp,rp,branch1,aft) ->
	  (match right_statement branch1 with
	    None -> Ast0.unwrap s
	  | Some (pragmas,branch1) ->
	      Ast0.IfThen
		(iff,lp,exp,rp,branch1,update_before pragmas aft))
      | Ast0.IfThenElse(iff,lp,exp,rp,branch1,els,branch2,aft) ->
	  (match right_statement branch2 with
	    None -> Ast0.unwrap s
	  | Some (pragmas,branch2) ->
	      Ast0.IfThenElse
		(iff,lp,exp,rp,branch1,els,branch2,
		  update_before pragmas aft))
      | Ast0.While(whl,lp,exp,rp,body,aft) ->
	  (match right_statement body with
	    None -> Ast0.unwrap s
	  | Some (pragmas,body) ->
	      Ast0.While(whl,lp,exp,rp,body,update_before pragmas aft))
      | Ast0.ScopedGuard(sg,lp,exp,rp,body,aft) ->
	  (match right_statement body with
	    None -> Ast0.unwrap s
	  | Some (pragmas,body) ->
	      Ast0.ScopedGuard(sg,lp,exp,rp,body,update_before pragmas aft))
      | Ast0.For(fr,lp,first,rp,body,aft) ->
	  (match right_statement body with
	    None -> Ast0.unwrap s
	  | Some (pragmas,body) ->
	      Ast0.For(fr,lp,first,rp,body,update_before pragmas aft))
      | Ast0.Iterator(nm,lp,args,rp,body,aft) ->
	  (match right_statement body with
	    None -> Ast0.unwrap s
	  | Some (pragmas,body) ->
	      Ast0.Iterator(nm,lp,args,rp,body,update_before pragmas aft))
      | _ -> Ast0.unwrap s) in

  let res = V0.rebuilder_default ~stmt:statement () in

  List.map res.VT0.rebuilder_rec_top_level
