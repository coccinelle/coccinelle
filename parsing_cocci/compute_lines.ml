(*
* Copyright 2005-2008, Ecole des Mines de Nantes, University of Copenhagen
* Yoann Padioleau, Julia Lawall, Rene Rydhof Hansen, Henrik Stuart, Gilles Muller
* This file is part of Coccinelle.
* 
* Coccinelle is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, according to version 2 of the License.
* 
* Coccinelle is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
* 
* You should have received a copy of the GNU General Public License
* along with Coccinelle.  If not, see <http://www.gnu.org/licenses/>.
* 
* The authors reserve the right to distribute this or future versions of
* Coccinelle under other licenses.
*)


(* Computes starting and ending logical lines for statements and
expressions.  every node gets an index as well. *)

module Ast0 = Ast0_cocci
module Ast = Ast_cocci
    
(* --------------------------------------------------------------------- *)
(* Result *)

let mkres x e left right =
  let lstart = Ast0.get_info left in
  let lend = Ast0.get_info right in
  let info =
    { Ast0.line_start = lstart.Ast0.line_start;
      Ast0.line_end = lend.Ast0.line_end;
      Ast0.logical_start = lstart.Ast0.logical_start;
      Ast0.logical_end = lend.Ast0.logical_end;
      Ast0.attachable_start = lstart.Ast0.attachable_start;
      Ast0.attachable_end = lend.Ast0.attachable_end;
      Ast0.mcode_start = lstart.Ast0.mcode_start;
      Ast0.mcode_end = lend.Ast0.mcode_end;
      Ast0.column = lstart.Ast0.column;
      Ast0.offset = lstart.Ast0.offset;
      (* only for tokens, not inherited upwards *)
      Ast0.strings_before = []; Ast0.strings_after = []} in
  {x with Ast0.node = e; Ast0.info = info}

let mkmultires x e left right (astart,start_mcodes) (aend,end_mcodes) =
  let lstart = Ast0.get_info left in
  let lend = Ast0.get_info right in
  let info =
    { Ast0.line_start = lstart.Ast0.line_start;
      Ast0.line_end = lend.Ast0.line_end;
      Ast0.logical_start = lstart.Ast0.logical_start;
      Ast0.logical_end = lend.Ast0.logical_end;
      Ast0.attachable_start = astart;
      Ast0.attachable_end = aend;
      Ast0.mcode_start = start_mcodes;
      Ast0.mcode_end = end_mcodes;
      Ast0.column = lstart.Ast0.column;
      Ast0.offset = lstart.Ast0.offset;
      (* only for tokens, not inherited upwards *)
      Ast0.strings_before = []; Ast0.strings_after = [] } in
  {x with Ast0.node = e; Ast0.info = info}

(* --------------------------------------------------------------------- *)
    
let get_option fn = function
    None -> None
  | Some x -> Some (fn x)
	
(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
(* Mcode *)

let promote_mcode (_,_,info,mcodekind,_) =
  let new_info =
    {info with
      Ast0.mcode_start = [mcodekind]; Ast0.mcode_end = [mcodekind]} in
  {(Ast0.wrap ()) with Ast0.info = new_info; Ast0.mcodekind = ref mcodekind}

let promote_mcode_plus_one (_,_,info,mcodekind,_) =
  let new_info =
    {info with
      Ast0.line_start = info.Ast0.line_start + 1;
      Ast0.logical_start = info.Ast0.logical_start + 1;
      Ast0.line_end = info.Ast0.line_end + 1;
      Ast0.logical_end = info.Ast0.logical_end + 1;
      Ast0.mcode_start = [mcodekind]; Ast0.mcode_end = [mcodekind]} in
  {(Ast0.wrap ()) with Ast0.info = new_info; Ast0.mcodekind = ref mcodekind}

let promote_to_statement stm mcodekind =
  let info = Ast0.get_info stm in
  let new_info =
    {info with
      Ast0.logical_start = info.Ast0.logical_end;
      Ast0.line_start = info.Ast0.line_end;
      Ast0.mcode_start = [mcodekind]; Ast0.mcode_end = [mcodekind];
      Ast0.attachable_start = true; Ast0.attachable_end = true} in
  {(Ast0.wrap ()) with Ast0.info = new_info; Ast0.mcodekind = ref mcodekind}

let promote_to_statement_start stm mcodekind =
  let info = Ast0.get_info stm in
  let new_info =
    {info with
      Ast0.logical_end = info.Ast0.logical_start;
      Ast0.line_end = info.Ast0.line_start;
      Ast0.mcode_start = [mcodekind]; Ast0.mcode_end = [mcodekind];
      Ast0.attachable_start = true; Ast0.attachable_end = true} in
  {(Ast0.wrap ()) with Ast0.info = new_info; Ast0.mcodekind = ref mcodekind}

(* mcode is good by default *)
let bad_mcode (t,a,info,mcodekind,pos) =
  let new_info =
    {info with Ast0.attachable_start = false; Ast0.attachable_end = false} in
  (t,a,new_info,mcodekind,pos)

let get_all_start_info l =
  (List.for_all (function x -> (Ast0.get_info x).Ast0.attachable_start) l,
   List.concat (List.map (function x -> (Ast0.get_info x).Ast0.mcode_start) l))

let get_all_end_info l =
  (List.for_all (function x -> (Ast0.get_info x).Ast0.attachable_end) l,
   List.concat (List.map (function x -> (Ast0.get_info x).Ast0.mcode_end) l))

(* --------------------------------------------------------------------- *)
(* Dots *)

(* for the logline classification and the mcode field, on both sides, skip
over initial minus dots, as they don't contribute anything *)
let dot_list is_dots fn = function
    [] -> failwith "dots should not be empty"
  | l ->
      let get_node l fn =
	let first = List.hd l in
	let chosen =
	  match (is_dots first, l) with (true,_::x::_) -> x | _ -> first in
	(* get the logline decorator and the mcodekind of the chosen node *)
	fn (Ast0.get_info chosen) in
      let forward = List.map fn l in
      let backward = List.rev forward in
      let (first_attachable,first_mcode) =
	get_node forward
	  (function x -> (x.Ast0.attachable_start,x.Ast0.mcode_start)) in
      let (last_attachable,last_mcode) =
	get_node backward
	  (function x -> (x.Ast0.attachable_end,x.Ast0.mcode_end)) in
      let first = List.hd forward in
      let last = List.hd backward in
      let first_info =
	{ (Ast0.get_info first) with
	  Ast0.attachable_start = first_attachable;
	  Ast0.mcode_start = first_mcode } in
      let last_info =
	{ (Ast0.get_info last) with
	  Ast0.attachable_end = last_attachable;
	  Ast0.mcode_end = last_mcode } in
      let first = Ast0.set_info first first_info in
      let last = Ast0.set_info last last_info in
      (forward,first,last)
      
let dots is_dots prev fn d =
  match (prev,Ast0.unwrap d) with
    (Some prev,Ast0.DOTS([])) ->
      mkres d (Ast0.DOTS []) prev prev
  | (None,Ast0.DOTS([])) ->
      Ast0.set_info d
	{(Ast0.get_info d)
	with Ast0.attachable_start = false; Ast0.attachable_end = false}
  | (_,Ast0.DOTS(x)) ->
      let (l,lstart,lend) = dot_list is_dots fn x in
      mkres d (Ast0.DOTS l) lstart lend
  | (_,Ast0.CIRCLES(x)) ->
      let (l,lstart,lend) = dot_list is_dots fn x in
      mkres d (Ast0.CIRCLES l) lstart lend
  | (_,Ast0.STARS(x)) ->
      let (l,lstart,lend) = dot_list is_dots fn x in
      mkres d (Ast0.STARS l) lstart lend

(* --------------------------------------------------------------------- *)
(* Identifier *)
	
let rec ident i =
  match Ast0.unwrap i with
    Ast0.Id(name) as ui ->
      let name = promote_mcode name in mkres i ui name name
  | Ast0.MetaId(name,_,_)
  | Ast0.MetaFunc(name,_,_) | Ast0.MetaLocalFunc(name,_,_) as ui ->
      let name = promote_mcode name in mkres i ui name name
  | Ast0.OptIdent(id) ->
      let id = ident id in mkres i (Ast0.OptIdent(id)) id id
  | Ast0.UniqueIdent(id) ->
      let id = ident id in mkres i (Ast0.UniqueIdent(id)) id id
	
(* --------------------------------------------------------------------- *)
(* Expression *)

let is_exp_dots e =
  match Ast0.unwrap e with
    Ast0.Edots(_,_) | Ast0.Ecircles(_,_) | Ast0.Estars(_,_) -> true
  | _ -> false

let rec expression e =
  match Ast0.unwrap e with
    Ast0.Ident(id) ->
      let id = ident id in
      mkres e (Ast0.Ident(id)) id id
  | Ast0.Constant(const) as ue ->
      let ln = promote_mcode const in
      mkres e ue ln ln
  | Ast0.FunCall(fn,lp,args,rp) ->
      let fn = expression fn in
      let args = dots is_exp_dots (Some(promote_mcode lp)) expression args in
      mkres e (Ast0.FunCall(fn,lp,args,rp)) fn (promote_mcode rp)
  | Ast0.Assignment(left,op,right,simple) ->
      let left = expression left in
      let right = expression right in
      mkres e (Ast0.Assignment(left,op,right,simple)) left right
  | Ast0.CondExpr(exp1,why,exp2,colon,exp3) ->
      let exp1 = expression exp1 in
      let exp2 = get_option expression exp2 in
      let exp3 = expression exp3 in
      mkres e (Ast0.CondExpr(exp1,why,exp2,colon,exp3)) exp1 exp3
  | Ast0.Postfix(exp,op) ->
      let exp = expression exp in
      mkres e (Ast0.Postfix(exp,op)) exp (promote_mcode op)
  | Ast0.Infix(exp,op) ->
      let exp = expression exp in
      mkres e (Ast0.Infix(exp,op)) (promote_mcode op) exp
  | Ast0.Unary(exp,op) ->
      let exp = expression exp in
      mkres e (Ast0.Unary(exp,op)) (promote_mcode op) exp
  | Ast0.Binary(left,op,right) ->
      let left = expression left in
      let right = expression right in
      mkres e (Ast0.Binary(left,op,right)) left right
  | Ast0.Nested(left,op,right) ->
      let left = expression left in
      let right = expression right in
      mkres e (Ast0.Nested(left,op,right)) left right
  | Ast0.Paren(lp,exp,rp) ->
      mkres e (Ast0.Paren(lp,expression exp,rp))
	(promote_mcode lp) (promote_mcode rp)
  | Ast0.ArrayAccess(exp1,lb,exp2,rb) ->
      let exp1 = expression exp1 in
      let exp2 = expression exp2 in
      mkres e (Ast0.ArrayAccess(exp1,lb,exp2,rb)) exp1 (promote_mcode rb)
  | Ast0.RecordAccess(exp,pt,field) ->
      let exp = expression exp in
      let field = ident field in
      mkres e (Ast0.RecordAccess(exp,pt,field)) exp field
  | Ast0.RecordPtAccess(exp,ar,field) ->
      let exp = expression exp in
      let field = ident field in
      mkres e (Ast0.RecordPtAccess(exp,ar,field)) exp field
  | Ast0.Cast(lp,ty,rp,exp) ->
      let exp = expression exp in
      mkres e (Ast0.Cast(lp,typeC ty,rp,exp)) (promote_mcode lp) exp
  | Ast0.SizeOfExpr(szf,exp) ->
      let exp = expression exp in
      mkres e (Ast0.SizeOfExpr(szf,exp)) (promote_mcode szf) exp
  | Ast0.SizeOfType(szf,lp,ty,rp) ->
      mkres e (Ast0.SizeOfType(szf,lp,typeC ty,rp)) 
        (promote_mcode szf)  (promote_mcode rp)
  | Ast0.TypeExp(ty) ->
      let ty = typeC ty in mkres e (Ast0.TypeExp(ty)) ty ty
  | Ast0.MetaErr(name,_,_) | Ast0.MetaExpr(name,_,_,_,_)
  | Ast0.MetaExprList(name,_,_) as ue ->
      let ln = promote_mcode name in mkres e ue ln ln
  | Ast0.EComma(cm) ->
      let cm = bad_mcode cm in
      let ln = promote_mcode cm in
      mkres e (Ast0.EComma(cm)) ln ln
  | Ast0.DisjExpr(starter,exps,mids,ender) ->
      let starter = bad_mcode starter in
      let exps = List.map expression exps in
      let mids = List.map bad_mcode mids in
      let ender = bad_mcode ender in
      mkmultires e (Ast0.DisjExpr(starter,exps,mids,ender))
	(promote_mcode starter) (promote_mcode ender)
	(get_all_start_info exps) (get_all_end_info exps)
  | Ast0.NestExpr(starter,exp_dots,ender,whencode,multi) ->
      let exp_dots = dots is_exp_dots None expression exp_dots in
      let starter = bad_mcode starter in
      let ender = bad_mcode ender in
      mkres e (Ast0.NestExpr(starter,exp_dots,ender,whencode,multi))
	(promote_mcode starter) (promote_mcode ender)
  | Ast0.Edots(dots,whencode) ->
      let dots = bad_mcode dots in
      let ln = promote_mcode dots in
      mkres e (Ast0.Edots(dots,whencode)) ln ln
  | Ast0.Ecircles(dots,whencode) ->
      let dots = bad_mcode dots in
      let ln = promote_mcode dots in
      mkres e (Ast0.Ecircles(dots,whencode)) ln ln
  | Ast0.Estars(dots,whencode) ->
      let dots = bad_mcode dots in
      let ln = promote_mcode dots in
      mkres e (Ast0.Estars(dots,whencode)) ln ln
  | Ast0.OptExp(exp) ->
      let exp = expression exp in
      mkres e (Ast0.OptExp(exp)) exp exp
  | Ast0.UniqueExp(exp) ->
      let exp = expression exp in
      mkres e (Ast0.UniqueExp(exp)) exp exp

and expression_dots x = dots is_exp_dots None expression x
	
(* --------------------------------------------------------------------- *)
(* Types *)
	
and typeC t =
  match Ast0.unwrap t with
    Ast0.ConstVol(cv,ty) ->
      let ty = typeC ty in
      mkres t (Ast0.ConstVol(cv,ty)) (promote_mcode cv) ty
  | Ast0.BaseType(ty,None) as ut ->
      mkres t ut (promote_mcode ty) (promote_mcode ty)
  | Ast0.BaseType(ty,Some sgn) as ut ->
      mkres t ut (promote_mcode sgn) (promote_mcode ty)
  | Ast0.ImplicitInt(sgn) as ut ->
      mkres t ut (promote_mcode sgn) (promote_mcode sgn)
  | Ast0.Pointer(ty,star) ->
      let ty = typeC ty in
      mkres t (Ast0.Pointer(ty,star)) ty (promote_mcode star)
  | Ast0.FunctionPointer(ty,lp1,star,rp1,lp2,params,rp2) ->
      let ty = typeC ty in
      let params = parameter_list (Some(promote_mcode lp2)) params in
      mkres t (Ast0.FunctionPointer(ty,lp1,star,rp1,lp2,params,rp2))
	ty (promote_mcode rp2)
  | Ast0.FunctionType(Some ty,lp1,params,rp1) ->
      let ty = typeC ty in
      let params = parameter_list (Some(promote_mcode lp1)) params in
      let res = Ast0.FunctionType(Some ty,lp1,params,rp1) in
      mkres t res ty (promote_mcode rp1)
  | Ast0.FunctionType(None,lp1,params,rp1) ->
      let params = parameter_list (Some(promote_mcode lp1)) params in
      let res = Ast0.FunctionType(None,lp1,params,rp1) in
      mkres t res (promote_mcode lp1) (promote_mcode rp1)
  | Ast0.Array(ty,lb,size,rb) ->
      let ty = typeC ty in
      mkres t (Ast0.Array(ty,lb,get_option expression size,rb))
	ty (promote_mcode rb)
  | Ast0.StructUnionName(kind,Some name) ->
      let name = ident name in
      mkres t (Ast0.StructUnionName(kind,Some name)) (promote_mcode kind) name
  | Ast0.StructUnionName(kind,None) ->
      let mc = promote_mcode kind in
      mkres t (Ast0.StructUnionName(kind,None)) mc mc
  | Ast0.StructUnionDef(ty,lb,decls,rb) ->
      let ty = typeC ty in
      let decls =
	dots is_decl_dots (Some(promote_mcode lb)) declaration decls in
      mkres t (Ast0.StructUnionDef(ty,lb,decls,rb)) ty (promote_mcode rb)
  | Ast0.TypeName(name) as ut ->
      let ln = promote_mcode name in mkres t ut ln ln
  | Ast0.MetaType(name,_) as ut ->
      let ln = promote_mcode name in mkres t ut ln ln
  | Ast0.DisjType(starter,types,mids,ender) ->
      let starter = bad_mcode starter in
      let types = List.map typeC types in
      let mids = List.map bad_mcode mids in
      let ender = bad_mcode ender in
      mkmultires t (Ast0.DisjType(starter,types,mids,ender))
	(promote_mcode starter) (promote_mcode ender)
	(get_all_start_info types) (get_all_end_info types)
  | Ast0.OptType(ty) ->
      let ty = typeC ty in mkres t (Ast0.OptType(ty)) ty ty
  | Ast0.UniqueType(ty) ->
      let ty = typeC ty in mkres t (Ast0.UniqueType(ty)) ty ty
	
(* --------------------------------------------------------------------- *)
(* Variable declaration *)
(* Even if the Cocci program specifies a list of declarations, they are
   split out into multiple declarations of a single variable each. *)

and is_decl_dots s =
  match Ast0.unwrap s with
    Ast0.Ddots(_,_) -> true
  | _ -> false
	
and declaration d =
  match Ast0.unwrap d with
    Ast0.Init(stg,ty,id,eq,exp,sem) ->
      let ty = typeC ty in
      let id = ident id in
      let exp = initialiser exp in
      (match stg with
	None ->
	  mkres d (Ast0.Init(stg,ty,id,eq,exp,sem)) ty (promote_mcode sem)
      | Some x -> 
	  mkres d (Ast0.Init(stg,ty,id,eq,exp,sem))
	    (promote_mcode x) (promote_mcode sem))
  | Ast0.UnInit(stg,ty,id,sem) ->
      let ty = typeC ty in
      let id = ident id in
      (match stg with
	None ->
	  mkres d (Ast0.UnInit(stg,ty,id,sem)) ty (promote_mcode sem)
      | Some x ->
	  mkres d (Ast0.UnInit(stg,ty,id,sem))
	    (promote_mcode x) (promote_mcode sem))
  | Ast0.MacroDecl(name,lp,args,rp,sem) ->
      let name = ident name in
      let args = dots is_exp_dots (Some(promote_mcode lp)) expression args in
      mkres d (Ast0.MacroDecl(name,lp,args,rp,sem)) name (promote_mcode sem)
  | Ast0.TyDecl(ty,sem) ->
      let ty = typeC ty in
      mkres d (Ast0.TyDecl(ty,sem)) ty (promote_mcode sem)
  | Ast0.Typedef(stg,ty,id,sem) ->
      let ty = typeC ty in
      let id = typeC id in
      mkres d (Ast0.Typedef(stg,ty,id,sem))
	(promote_mcode stg) (promote_mcode sem)
  | Ast0.DisjDecl(starter,decls,mids,ender) ->
      let starter = bad_mcode starter in
      let decls = List.map declaration decls in
      let mids = List.map bad_mcode mids in
      let ender = bad_mcode ender in
      mkmultires d (Ast0.DisjDecl(starter,decls,mids,ender))
	(promote_mcode starter) (promote_mcode ender)
	(get_all_start_info decls) (get_all_end_info decls)
  | Ast0.Ddots(dots,whencode) ->
      let dots = bad_mcode dots in
      let ln = promote_mcode dots in
      mkres d (Ast0.Ddots(dots,whencode)) ln ln
  | Ast0.OptDecl(decl) ->
      let decl = declaration decl in
      mkres d (Ast0.OptDecl(declaration decl)) decl decl
  | Ast0.UniqueDecl(decl) ->
      let decl = declaration decl in
      mkres d (Ast0.UniqueDecl(declaration decl)) decl decl

(* --------------------------------------------------------------------- *)
(* Initializer *)

and is_init_dots i =
  match Ast0.unwrap i with
    Ast0.Idots(_,_) -> true
  | _ -> false
	
and initialiser i =
  match Ast0.unwrap i with
    Ast0.InitExpr(exp) ->
      let exp = expression exp in
      mkres i (Ast0.InitExpr(exp)) exp exp
  | Ast0.InitList(lb,initlist,rb) ->
      let initlist =
	dots is_init_dots (Some(promote_mcode lb)) initialiser initlist in
      mkres i (Ast0.InitList(lb,initlist,rb))
	(promote_mcode lb) (promote_mcode rb)
  | Ast0.InitGccDotName(dot,name,eq,ini) ->
      let name = ident name in
      let ini = initialiser ini in
      mkres i (Ast0.InitGccDotName(dot,name,eq,ini)) (promote_mcode dot) ini
  | Ast0.InitGccName(name,eq,ini) ->
      let name = ident name in
      let ini = initialiser ini in
      mkres i (Ast0.InitGccName(name,eq,ini)) name ini
  | Ast0.InitGccIndex(lb,exp,rb,eq,ini) ->
      let exp = expression exp in
      let ini = initialiser ini in
      mkres i (Ast0.InitGccIndex(lb,exp,rb,eq,ini)) (promote_mcode lb) ini
  | Ast0.InitGccRange(lb,exp1,dots,exp2,rb,eq,ini) ->
      let exp1 = expression exp1 in
      let exp2 = expression exp2 in
      let ini = initialiser ini in
      mkres i (Ast0.InitGccRange(lb,exp1,dots,exp2,rb,eq,ini))
	(promote_mcode lb) ini
  | Ast0.IComma(cm) as up ->
      let ln = promote_mcode cm in mkres i up ln ln
  | Ast0.Idots(dots,whencode) ->
      let dots = bad_mcode dots in
      let ln = promote_mcode dots in
      mkres i (Ast0.Idots(dots,whencode)) ln ln
  | Ast0.OptIni(ini) ->
      let ini = initialiser ini in
      mkres i (Ast0.OptIni(ini)) ini ini
  | Ast0.UniqueIni(ini) ->
      let ini = initialiser ini in
      mkres i (Ast0.UniqueIni(ini)) ini ini

and initialiser_list prev = dots is_init_dots prev initialiser

(* for export *)
and initialiser_dots x = dots is_init_dots None initialiser x

(* --------------------------------------------------------------------- *)
(* Parameter *)

and is_param_dots p =
  match Ast0.unwrap p with
    Ast0.Pdots(_) | Ast0.Pcircles(_) -> true
  | _ -> false
	
and parameterTypeDef p =
  match Ast0.unwrap p with
    Ast0.VoidParam(ty) ->
      let ty = typeC ty in mkres p (Ast0.VoidParam(ty)) ty ty
  | Ast0.Param(ty,Some id) ->
      let id = ident id in
      let ty = typeC ty in mkres p (Ast0.Param(ty,Some id)) ty id
  | Ast0.Param(ty,None) ->
      let ty = typeC ty in mkres p (Ast0.Param(ty,None)) ty ty
  | Ast0.MetaParam(name,_) as up ->
      let ln = promote_mcode name in mkres p up ln ln
  | Ast0.MetaParamList(name,_,_) as up ->
      let ln = promote_mcode name in mkres p up ln ln
  | Ast0.PComma(cm) ->
      let cm = bad_mcode cm in
      let ln = promote_mcode cm in
      mkres p (Ast0.PComma(cm)) ln ln
  | Ast0.Pdots(dots) ->
      let dots = bad_mcode dots in
      let ln = promote_mcode dots in
      mkres p (Ast0.Pdots(dots)) ln ln
  | Ast0.Pcircles(dots) ->
      let dots = bad_mcode dots in
      let ln = promote_mcode dots in
      mkres p (Ast0.Pcircles(dots)) ln ln
  | Ast0.OptParam(param) ->
      let res = parameterTypeDef param in
      mkres p (Ast0.OptParam(res)) res res
  | Ast0.UniqueParam(param) ->
      let res = parameterTypeDef param in
      mkres p (Ast0.UniqueParam(res)) res res

and parameter_list prev = dots is_param_dots prev parameterTypeDef

(* for export *)
let parameter_dots x = dots is_param_dots None parameterTypeDef x

(* --------------------------------------------------------------------- *)
(* Top-level code *)

let is_stm_dots s =
  match Ast0.unwrap s with
    Ast0.Dots(_,_) | Ast0.Circles(_,_) | Ast0.Stars(_,_) -> true
  | _ -> false
    
let rec statement s =
  let res =
    match Ast0.unwrap s with
      Ast0.Decl((_,bef),decl) ->
	let decl = declaration decl in
	let left = promote_to_statement_start decl bef in
	mkres s (Ast0.Decl((Ast0.get_info left,bef),decl)) decl decl
    | Ast0.Seq(lbrace,body,rbrace) -> 
	let body =
	  dots is_stm_dots (Some(promote_mcode lbrace)) statement body in
	mkres s (Ast0.Seq(lbrace,body,rbrace))
	  (promote_mcode lbrace) (promote_mcode rbrace)
    | Ast0.ExprStatement(exp,sem) ->
	let exp = expression exp in
	mkres s (Ast0.ExprStatement(exp,sem)) exp (promote_mcode sem)
    | Ast0.IfThen(iff,lp,exp,rp,branch,(_,aft)) ->
	let exp = expression exp in
	let branch = statement branch in
	let right = promote_to_statement branch aft in
	mkres s (Ast0.IfThen(iff,lp,exp,rp,branch,(Ast0.get_info right,aft)))
	  (promote_mcode iff) right
    | Ast0.IfThenElse(iff,lp,exp,rp,branch1,els,branch2,(_,aft)) ->
	let exp = expression exp in
	let branch1 = statement branch1 in
	let branch2 = statement branch2 in
	let right = promote_to_statement branch2 aft in
	mkres s
	  (Ast0.IfThenElse(iff,lp,exp,rp,branch1,els,branch2,
	    (Ast0.get_info right,aft)))
	  (promote_mcode iff) right
    | Ast0.While(wh,lp,exp,rp,body,(_,aft)) ->
	let exp = expression exp in
	let body = statement body in
	let right = promote_to_statement body aft in
	mkres s (Ast0.While(wh,lp,exp,rp,body,(Ast0.get_info right,aft)))
	  (promote_mcode wh) right
    | Ast0.Do(d,body,wh,lp,exp,rp,sem) ->
	let body = statement body in
	let exp = expression exp in
	mkres s (Ast0.Do(d,body,wh,lp,exp,rp,sem))
	  (promote_mcode d) (promote_mcode sem)
    | Ast0.For(fr,lp,exp1,sem1,exp2,sem2,exp3,rp,body,(_,aft)) ->
	let exp1 = get_option expression exp1 in
	let exp2 = get_option expression exp2 in
	let exp3 = get_option expression exp3 in
	let body = statement body in
	let right = promote_to_statement body aft in
	mkres s (Ast0.For(fr,lp,exp1,sem1,exp2,sem2,exp3,rp,body,
			  (Ast0.get_info right,aft)))
	  (promote_mcode fr) right
    | Ast0.Iterator(nm,lp,args,rp,body,(_,aft)) ->
	let nm = ident nm in
	let args = dots is_exp_dots (Some(promote_mcode lp)) expression args in
	let body = statement body in
	let right = promote_to_statement body aft in
	mkres s (Ast0.Iterator(nm,lp,args,rp,body,(Ast0.get_info right,aft)))
	  nm right
    | Ast0.Switch(switch,lp,exp,rp,lb,cases,rb) ->
	let exp = expression exp in
	let cases =
	  dots (function _ -> false) (Some(promote_mcode lb)) case_line cases in
	mkres s
	  (Ast0.Switch(switch,lp,exp,rp,lb,cases,rb))
	  (promote_mcode switch) (promote_mcode rb)
    | Ast0.Break(br,sem) as us ->
	mkres s us (promote_mcode br) (promote_mcode sem)
    | Ast0.Continue(cont,sem) as us ->
	mkres s us (promote_mcode cont) (promote_mcode sem)
    | Ast0.Label(l,dd) ->
	let l = ident l in
	mkres s (Ast0.Label(l,dd)) l (promote_mcode dd)
    | Ast0.Goto(goto,id,sem) ->
	let id = ident id in
	mkres s (Ast0.Goto(goto,id,sem)) 
	  (promote_mcode goto) (promote_mcode sem)
    | Ast0.Return(ret,sem) as us ->
	mkres s us (promote_mcode ret) (promote_mcode sem)
    | Ast0.ReturnExpr(ret,exp,sem) ->
	let exp = expression exp in
	mkres s (Ast0.ReturnExpr(ret,exp,sem)) 
	  (promote_mcode ret) (promote_mcode sem)
    | Ast0.MetaStmt(name,_)
    | Ast0.MetaStmtList(name,_) as us ->
	let ln = promote_mcode name in mkres s us ln ln
    | Ast0.Exp(exp) ->
	let exp = expression exp in
	mkres s (Ast0.Exp(exp)) exp exp
    | Ast0.TopExp(exp) ->
	let exp = expression exp in
	mkres s (Ast0.TopExp(exp)) exp exp
    | Ast0.Ty(ty) ->
	let ty = typeC ty in
	mkres s (Ast0.Ty(ty)) ty ty
    | Ast0.TopInit(init) ->
	let init = initialiser init in
	mkres s (Ast0.TopInit(init)) init init
    | Ast0.Disj(starter,rule_elem_dots_list,mids,ender) ->
	let starter = bad_mcode starter in
	let mids = List.map bad_mcode mids in
	let ender = bad_mcode ender in
	let rec loop prevs = function
	    [] -> []
	  | stm::stms ->
	      (dots is_stm_dots (Some(promote_mcode_plus_one(List.hd prevs)))
		 statement stm)::
	      (loop (List.tl prevs) stms) in
	let elems = loop (starter::mids) rule_elem_dots_list in
	mkmultires s (Ast0.Disj(starter,elems,mids,ender))
	  (promote_mcode starter) (promote_mcode ender)
	  (get_all_start_info elems) (get_all_end_info elems)
    | Ast0.Nest(starter,rule_elem_dots,ender,whencode,multi) ->
	let starter = bad_mcode starter in
	let ender = bad_mcode ender in
	let rule_elem_dots = dots is_stm_dots None statement rule_elem_dots in
	mkres s (Ast0.Nest(starter,rule_elem_dots,ender,whencode,multi))
	  (promote_mcode starter) (promote_mcode ender)
    | Ast0.Dots(dots,whencode) ->
	let dots = bad_mcode dots in
	let ln = promote_mcode dots in
	mkres s (Ast0.Dots(dots,whencode)) ln ln
    | Ast0.Circles(dots,whencode) ->
	let dots = bad_mcode dots in
	let ln = promote_mcode dots in
	mkres s (Ast0.Circles(dots,whencode)) ln ln
    | Ast0.Stars(dots,whencode) ->
	let dots = bad_mcode dots in
	let ln = promote_mcode dots in
	mkres s (Ast0.Stars(dots,whencode)) ln ln
    | Ast0.FunDecl((_,bef),fninfo,name,lp,params,rp,lbrace,body,rbrace) ->
	let fninfo =
	  List.map
	    (function Ast0.FType(ty) -> Ast0.FType(typeC ty) | x -> x)
	    fninfo in
	let name = ident name in
	let params = parameter_list (Some(promote_mcode lp)) params in
	let body =
	  dots is_stm_dots (Some(promote_mcode lbrace)) statement body in
	let left =
	(* cases on what is leftmost *)
	  match fninfo with
	    [] -> promote_to_statement_start name bef
	  | Ast0.FStorage(stg)::_ ->
	      promote_to_statement_start (promote_mcode stg) bef
	  | Ast0.FType(ty)::_ ->
	      promote_to_statement_start ty bef
	  | Ast0.FInline(inline)::_ ->
	      promote_to_statement_start (promote_mcode inline) bef
	  | Ast0.FAttr(attr)::_ ->
	      promote_to_statement_start (promote_mcode attr) bef in
      (* pretend it is one line before the start of the function, so that it
	 will catch things defined at top level.  We assume that these will not
	 be defined on the same line as the function.  This is a HACK.
	 A better approach would be to attach top_level things to this node,
	 and other things to the node after, but that would complicate
	 insert_plus, which doesn't distinguish between different mcodekinds *)
	let res =
	  Ast0.FunDecl((Ast0.get_info left,bef),fninfo,name,lp,params,rp,lbrace,
		       body,rbrace) in
      (* have to do this test again, because of typing problems - can't save
	 the result, only use it *)
	(match fninfo with
	  [] -> mkres s res name (promote_mcode rbrace)
	| Ast0.FStorage(stg)::_ ->
	    mkres s res (promote_mcode stg) (promote_mcode rbrace)
	| Ast0.FType(ty)::_ -> mkres s res ty (promote_mcode rbrace)
	| Ast0.FInline(inline)::_ ->
	    mkres s res (promote_mcode inline) (promote_mcode rbrace)
	| Ast0.FAttr(attr)::_ ->
	    mkres s res (promote_mcode attr) (promote_mcode rbrace))
	  
    | Ast0.Include(inc,stm) ->
	mkres s (Ast0.Include(inc,stm)) (promote_mcode inc) (promote_mcode stm)
    | Ast0.Define(def,id,params,body) ->
	let id = ident id in
	let body = dots is_stm_dots None statement body in
	mkres s (Ast0.Define(def,id,params,body)) (promote_mcode def) body
    | Ast0.OptStm(stm) ->
	let stm = statement stm in mkres s (Ast0.OptStm(stm)) stm stm
    | Ast0.UniqueStm(stm) ->
	let stm = statement stm in mkres s (Ast0.UniqueStm(stm)) stm stm in
  Ast0.set_dots_bef_aft res
    (match Ast0.get_dots_bef_aft res with
      Ast0.NoDots -> Ast0.NoDots
    | Ast0.AddingBetweenDots s ->
	Ast0.AddingBetweenDots(statement s)
    | Ast0.DroppingBetweenDots s ->
	Ast0.DroppingBetweenDots(statement s))

and case_line c =
  match Ast0.unwrap c with
    Ast0.Default(def,colon,code) ->
      let code = dots is_stm_dots (Some(promote_mcode colon)) statement code in
      mkres c (Ast0.Default(def,colon,code)) (promote_mcode def) code
  | Ast0.Case(case,exp,colon,code) ->
      let exp = expression exp in
      let code = dots is_stm_dots (Some(promote_mcode colon)) statement code in
      mkres c (Ast0.Case(case,exp,colon,code)) (promote_mcode case) code
  | Ast0.OptCase(case) ->
      let case = case_line case in mkres c (Ast0.OptCase(case)) case case

and statement_dots x = dots is_stm_dots None statement x
	
(* --------------------------------------------------------------------- *)
(* Function declaration *)
	
let top_level t =
  match Ast0.unwrap t with
    Ast0.FILEINFO(old_file,new_file) -> t
  | Ast0.DECL(stmt) ->
      let stmt = statement stmt in mkres t (Ast0.DECL(stmt)) stmt stmt
  | Ast0.CODE(rule_elem_dots) ->
      let rule_elem_dots = dots is_stm_dots None statement rule_elem_dots in
      mkres t (Ast0.CODE(rule_elem_dots)) rule_elem_dots rule_elem_dots
  | Ast0.ERRORWORDS(exps) -> t
  | Ast0.OTHER(_) -> failwith "eliminated by top_level"
	
(* --------------------------------------------------------------------- *)
(* Entry points *)
	
let compute_lines = List.map top_level
    
