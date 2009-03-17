(* Find a directive or comment at the end of a statement.  Things with aft
given None, because they can accomodate their own directives or comments *)

module Ast0 = Ast0_cocci
module V0 = Visitor_ast0

let call_right processor data s cont =
  match processor data with
    None -> None
  | Some(pragmas,data) -> Some (pragmas,Ast0.rewrap s (cont data))

let left_mcode (a,b,info,mcodekind,d) =
  match (info.Ast0.strings_before,mcodekind) with
    ([],_) | (_,Ast0.PLUS) -> None
  | (l,_) -> Some(l,(a,b,{info with Ast0.strings_before = []},mcodekind,d))

let right_mcode (a,b,info,mcodekind,d) =
  match (info.Ast0.strings_after,mcodekind) with
    ([],_) | (_,Ast0.PLUS) -> None
  | (l,_) -> Some(l,(a,b,{info with Ast0.strings_after = []},mcodekind,d))

let update_before pragmas (info,x) =
  ({info with Ast0.strings_before = pragmas @ info.Ast0.strings_before},
   Ast0.PLUS)

let update_after pragmas (info,x) =
  ({info with Ast0.strings_after = info.Ast0.strings_after @ pragmas},
   Ast0.PLUS)

let rec right_decl d =
  match Ast0.unwrap d with
    Ast0.Init(Some stg,ty,id,eq,ini,sem) ->
      call_right right_mcode sem d
	(function sem -> Ast0.Init(Some stg,ty,id,eq,ini,sem))
  | Ast0.Init(None,ty,id,eq,ini,sem) ->
      call_right right_mcode sem d
	(function sem -> Ast0.Init(None,ty,id,eq,ini,sem))
  | Ast0.UnInit(Some stg,ty,id,sem) ->
      call_right right_mcode sem d
	(function sem -> Ast0.UnInit(Some stg,ty,id,sem))
  | Ast0.UnInit(None,ty,id,sem) ->
      call_right right_mcode sem d
	(function sem -> Ast0.UnInit(None,ty,id,sem))
  | Ast0.MacroDecl(name,lp,args,rp,sem) ->
      call_right right_mcode sem d
	(function sem -> Ast0.MacroDecl(name,lp,args,rp,sem))
  | Ast0.TyDecl(ty,sem) ->
      call_right right_mcode sem d
	(function sem -> Ast0.TyDecl(ty,sem))
  | Ast0.Typedef(stg,ty,id,sem) ->
      call_right right_mcode sem d
	(function sem -> Ast0.Typedef(stg,ty,id,sem))
  | Ast0.DisjDecl(starter,decls,mids,ender) -> None
  | Ast0.Ddots(dots,whencode) -> None
  | Ast0.OptDecl(decl) ->
      call_right right_decl decl d (function decl -> Ast0.OptDecl(decl))
  | Ast0.UniqueDecl(decl) ->
      call_right right_decl decl d (function decl -> Ast0.UniqueDecl(decl))

let rec right_statement s =
  match Ast0.unwrap s with
    Ast0.FunDecl(bef,fi,name,lp,params,rp,lbrace,body,rbrace) -> None
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
  | Ast0.For(fr,lp,e1,sem1,e2,sem2,e3,rp,body,aft) -> None
  | Ast0.Iterator(nm,lp,args,rp,body,aft) -> None
  | Ast0.Switch(switch,lp,exp,rp,lb,cases,rb) ->
      call_right right_mcode rb s
	(function rb -> Ast0.Switch(switch,lp,exp,rp,lb,cases,rb))
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
  | Ast0.MetaStmt(name,pure) ->
      call_right right_mcode name s
	(function name -> Ast0.MetaStmt(name,pure))
  | Ast0.MetaStmtList(name,pure) ->
      call_right right_mcode name s
	(function name -> Ast0.MetaStmtList(name,pure))
  | Ast0.Disj(starter,statement_dots_list,mids,ender) -> None
  | Ast0.Nest(starter,stmt_dots,ender,whn,multi) -> None
  (* the following are None, because they can't be adjacent to an aft node *)
  | Ast0.Exp(exp) -> None
  | Ast0.TopExp(exp) -> None
  | Ast0.Ty(ty) -> None
  | Ast0.TopInit(init) -> None
  | Ast0.Dots(d,whn) -> None
  | Ast0.Circles(d,whn) -> None
  | Ast0.Stars(d,whn) -> None
  | Ast0.Include(inc,name) ->
      call_right right_mcode name s
	(function name -> Ast0.Include(inc,name))
  | Ast0.Define(def,id,params,body) ->
      call_right right_statement_dots body s
	(function body -> Ast0.Define(def,id,params,body))
  | Ast0.OptStm(re) ->
      call_right right_statement re s (function re -> Ast0.OptStm(re))
  | Ast0.UniqueStm(re) ->
      call_right right_statement re s (function re -> Ast0.UniqueStm(re))

and right_statement_dots sd =
  match Ast0.unwrap sd with
    Ast0.DOTS([]) -> failwith "empty statement dots"
  | Ast0.DOTS(s::r) ->
      call_right right_statement s sd
	(function s -> Ast0.DOTS(List.rev(s::r)))
  | _ -> failwith "circles and stars not supported"

let rec left_ty t =
  match Ast0.unwrap t with
    Ast0.ConstVol(cv,ty) ->
      call_right left_mcode cv t (function cv -> Ast0.ConstVol(cv,ty))
  | Ast0.BaseType(ty,strings) ->
      (match strings with
	[] -> failwith "empty strings in type"
      |	s::r ->
	  call_right left_mcode s t (function s -> Ast0.BaseType(ty,s::r)))
  | Ast0.Signed(sign,ty) ->
      call_right left_mcode sign t (function sign -> Ast0.Signed(sign,ty))
  | Ast0.Pointer(ty,star) ->
      call_right left_ty ty t (function ty -> Ast0.Pointer(ty,star))
  | Ast0.FunctionPointer(ty,lp1,star,rp1,lp2,params,rp2) ->
      call_right left_ty ty t
	(function ty -> Ast0.FunctionPointer(ty,lp1,star,rp1,lp2,params,rp2))
  | Ast0.FunctionType(Some ty,lp1,params,rp1) ->
      call_right left_ty ty t
	(function ty -> Ast0.FunctionType(Some ty,lp1,params,rp1))
  | Ast0.FunctionType(None,lp1,params,rp1) ->
      call_right left_mcode lp1 t
	(function lp1 -> Ast0.FunctionType(None,lp1,params,rp1))
  | Ast0.Array(ty,lb,size,rb) ->
      call_right left_ty ty t (function ty -> Ast0.Array(ty,lb,size,rb))
  | Ast0.EnumName(kind,name) ->
      call_right left_mcode kind t (function kind -> Ast0.EnumName(kind,name))
  | Ast0.StructUnionName(kind,name) ->
      call_right left_mcode kind t
	(function kind -> Ast0.StructUnionName(kind,name))
  | Ast0.StructUnionDef(ty,lb,decls,rb) ->
      call_right left_ty ty t
	(function ty -> Ast0.StructUnionDef(ty,lb,decls,rb))
  | Ast0.TypeName(name) ->
      call_right left_mcode name t (function name -> Ast0.TypeName(name))
  | Ast0.MetaType(name,x) ->
      call_right left_mcode name t (function name -> Ast0.MetaType(name,x))
  | Ast0.DisjType(starter,types,mids,ender) -> None
  | Ast0.OptType(ty) ->
      call_right left_ty ty t (function ty -> Ast0.OptType(ty))
  | Ast0.UniqueType(ty) ->
      call_right left_ty ty t (function ty -> Ast0.UniqueType(ty))

let rec left_ident i =
  match Ast0.unwrap i with
    Ast0.Id(name) ->
      call_right left_mcode name i
	(function name -> Ast0.Id(name))
  | Ast0.MetaId(name,a,b) ->
      call_right left_mcode name i
	(function name -> Ast0.MetaId(name,a,b))
  | Ast0.MetaFunc(name,a,b) ->
      call_right left_mcode name i
	(function name -> Ast0.MetaFunc(name,a,b))
  | Ast0.MetaLocalFunc(name,a,b) ->
      call_right left_mcode name i
	(function name -> Ast0.MetaLocalFunc(name,a,b))
  | Ast0.OptIdent(id) ->
      call_right left_ident id i (function id -> Ast0.OptIdent(id))
  | Ast0.UniqueIdent(id) ->
      call_right left_ident id i (function id -> Ast0.UniqueIdent(id))

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
  | (Ast0.FAttr atr)::x ->
      fncall_right left_mcode atr (function atr -> (Ast0.FAttr atr)::x)

let rec left_decl decl =
  match Ast0.unwrap decl with
    Ast0.Init(Some stg,ty,id,eq,ini,sem) ->
      call_right left_mcode stg decl
	(function stg -> Ast0.Init(Some stg,ty,id,eq,ini,sem))
  | Ast0.Init(None,ty,id,eq,ini,sem) ->
      call_right left_ty ty decl
	(function ty -> Ast0.Init(None,ty,id,eq,ini,sem))
  | Ast0.UnInit(Some stg,ty,id,sem) ->
      call_right left_mcode stg decl
	(function stg -> Ast0.UnInit(Some stg,ty,id,sem))
  | Ast0.UnInit(None,ty,id,sem) ->
      call_right left_ty ty decl
	(function ty -> Ast0.UnInit(None,ty,id,sem))
  | Ast0.MacroDecl(name,lp,args,rp,sem) ->
      call_right left_ident name decl
	(function name -> Ast0.MacroDecl(name,lp,args,rp,sem))
  | Ast0.TyDecl(ty,sem) ->
      call_right left_ty ty decl (function ty -> Ast0.TyDecl(ty,sem))
  | Ast0.Typedef(stg,ty,id,sem) ->
      call_right left_mcode stg decl
	(function stg -> Ast0.Typedef(stg,ty,id,sem))
  | Ast0.DisjDecl(starter,decls,mids,ender) -> None
  | Ast0.Ddots(dots,whencode) -> None
  | Ast0.OptDecl(d) ->
      call_right left_decl d decl (function decl -> Ast0.OptDecl(decl))
  | Ast0.UniqueDecl(d) ->
      call_right left_decl d decl (function decl -> Ast0.UniqueDecl(decl))

let process =
  let donothing r k e = k e in
  let mcode x = x in

  let statement r k s =
    let s = k s in
    Ast0.rewrap s
      (match Ast0.unwrap s with
	Ast0.FunDecl(bef,fi,name,lp,params,rp,lbrace,body,rbrace) ->
	  (match left_fundecl name fi with
	    None -> Ast0.unwrap s
	  | Some (pragmas,fi,name) ->
	      Ast0.FunDecl
		(update_after pragmas bef,
		 fi,name,lp,params,rp,lbrace,body,rbrace))
      | Ast0.Decl(bef,decl) ->
	  (match left_decl decl with
	    None -> Ast0.unwrap s
	  | Some (pragmas,decl) ->
	      Ast0.Decl(update_after pragmas bef,decl))
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
      | Ast0.For(fr,lp,e1,sem1,e2,sem2,e3,rp,body,aft) ->
	  (match right_statement body with
	    None -> Ast0.unwrap s
	  | Some (pragmas,body) ->
	      Ast0.For
		(fr,lp,e1,sem1,e2,sem2,e3,rp,body,
		 update_before pragmas aft))
      | Ast0.Iterator(nm,lp,args,rp,body,aft) ->
	  (match right_statement body with
	    None -> Ast0.unwrap s
	  | Some (pragmas,body) ->
	      Ast0.Iterator(nm,lp,args,rp,body,update_before pragmas aft))
      | _ -> Ast0.unwrap s) in

  let res = V0.rebuilder
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      donothing donothing donothing donothing donothing donothing
      donothing donothing donothing donothing donothing donothing statement
      donothing donothing in

  List.map res.V0.rebuilder_top_level
