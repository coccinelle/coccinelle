(*
 * Copyright 2012-2014, INRIA
 * Julia Lawall, Gilles Muller
 * Copyright 2010-2011, INRIA, University of Copenhagen
 * Julia Lawall, Rene Rydhof Hansen, Gilles Muller, Nicolas Palix
 * Copyright 2005-2009, Ecole des Mines de Nantes, University of Copenhagen
 * Yoann Padioleau, Julia Lawall, Rene Rydhof Hansen, Henrik Stuart, Gilles Muller, Nicolas Palix
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


# 0 "./unify_ast.ml"
(* --------------------------------------------------------------------- *)
(* Given two patterns, A and B, determine whether B can match any matched
subterms of A.  For simplicity, this doesn't maintain an environment; it
just assume metavariables match.  Thus the result is either NO or MAYBE. *)

module Ast = Ast_cocci
module V = Visitor_ast

(* --------------------------------------------------------------------- *)

type res = NO | MAYBE

let return b = if b then MAYBE else NO

let unify_mcode (x,_,_,_) (y,_,_,_) = x = y

let ret_unify_mcode a b = return (unify_mcode a b)

let unify_option f t1 t2 =
  match (t1,t2) with
    (Some t1, Some t2) -> f t1 t2
  | (None, None) -> return true
  | _ -> return false

let unify_true_option f t1 t2 =
  match (t1,t2) with
    (Some t1, Some t2) -> f t1 t2
  | (None, None) -> return true
  | _ -> return true

let bool_unify_option f t1 t2 =
  match (t1,t2) with
    (Some t1, Some t2) -> f t1 t2
  | (None, None) -> true
  | _ -> false

let conjunct_bindings b1 b2 =
  match b1 with NO -> b1 | MAYBE -> b2

let disjunct_bindings b1 b2 =
  match b1 with MAYBE -> b1 | NO -> b2

let disjunct_all_bindings = List.fold_left disjunct_bindings NO

(* --------------------------------------------------------------------- *)

(* compute the common prefix.  if in at least one case, this ends with the
end of the pattern or a ..., then return true. *)

let unify_lists fn dfn la lb =
  let rec loop = function
      ([],_) | (_,[]) -> return true
    | (cura::resta,curb::restb) ->
	(match fn cura curb with
	  MAYBE -> loop (resta,restb)
	| NO -> if dfn cura or dfn curb then MAYBE else NO) in
  loop (la,lb)

let unify_dots fn dfn d1 d2 =
  match (Ast.unwrap d1,Ast.unwrap d2) with
    (Ast.DOTS(l1),Ast.DOTS(l2))
  | (Ast.CIRCLES(l1),Ast.CIRCLES(l2))
  | (Ast.STARS(l1),Ast.STARS(l2)) -> unify_lists fn dfn l1 l2
  | _ -> return false

let edots e =
  match Ast.unwrap e with
    Ast.Edots(_,_) | Ast.Ecircles(_,_) | Ast.Estars(_,_) -> true
  | _ -> false

let ddots e =
  match Ast.unwrap e with
    Ast.Ddots(_,_) -> true
  | _ -> false

let pdots p =
  match Ast.unwrap p with
    Ast.Pdots(_) | Ast.Pcircles(_) -> true
  | _ -> false

let dpdots e =
  match Ast.unwrap e with
    Ast.DPdots(_) | Ast.DPcircles(_) -> true
  | _ -> false

let sdots s =
  match Ast.unwrap s with
    Ast.Dots(_,_,_,_) | Ast.Circles(_,_,_,_) | Ast.Stars(_,_,_,_) -> true
  | _ -> false

let idots e =
  match Ast.unwrap e with
    Ast.Idots(_,_) -> true
  | _ -> false

let strdots e =
  match Ast.unwrap e with
    Ast.Strdots(_) -> true
  | _ -> false

let ecdots e =
  match Ast.unwrap e with
    Ast.ExecDots(_) -> true
  | _ -> false

(* --------------------------------------------------------------------- *)
(* Identifier *)

let rec unify_ident i1 i2 =
  match (Ast.unwrap i1,Ast.unwrap i2) with
    (Ast.Id(i1),Ast.Id(i2)) -> return (unify_mcode i1 i2)

  | (Ast.MetaId(_,_,_,_),_)
  | (Ast.MetaFunc(_,_,_,_),_)
  | (Ast.MetaLocalFunc(_,_,_,_),_)
  | (_,Ast.MetaId(_,_,_,_))
  | (_,Ast.MetaFunc(_,_,_,_))
  | (_,Ast.MetaLocalFunc(_,_,_,_)) -> return true

  | (Ast.AsIdent(id1,asid1),_) ->
      disjunct_all_bindings
	(List.map (function x -> unify_ident x i2) [id1;asid1])
  | (_,Ast.AsIdent(id2,asid2)) ->
      disjunct_all_bindings
	(List.map (function x -> unify_ident x i1) [id2;asid2])

  | (Ast.DisjId(i1),_) ->
      disjunct_all_bindings (List.map (function x -> unify_ident x i2) i1)
  | (_,Ast.DisjId(i2)) ->
      disjunct_all_bindings (List.map (function x -> unify_ident i1 x) i2)

  | (Ast.OptIdent(_),_)
  | (Ast.UniqueIdent(_),_)
  | (_,Ast.OptIdent(_))
  | (_,Ast.UniqueIdent(_)) -> failwith "unsupported ident"

(* --------------------------------------------------------------------- *)
(* Expression *)

and unify_expression e1 e2 =
  match (Ast.unwrap e1,Ast.unwrap e2) with
    (Ast.Ident(i1),Ast.Ident(i2)) -> unify_ident i1 i2
  | (Ast.Constant(c1),Ast.Constant(c2))-> return (unify_mcode c1 c2)
  | (Ast.StringConstant(lq1,str1,rq1),Ast.StringConstant(lq2,str2,rq2)) ->
      unify_dots unify_string_fragment strdots str1 str2
  | (Ast.FunCall(f1,lp1,args1,rp1),Ast.FunCall(f2,lp2,args2,rp2)) ->
      conjunct_bindings
	(unify_expression f1 f2)
	(unify_dots unify_expression edots args1 args2)
  | (Ast.Assignment(l1,op1,r1,_),Ast.Assignment(l2,op2,r2,_)) ->
      if unify_mcode op1 op2
      then conjunct_bindings (unify_expression l1 l2) (unify_expression r1 r2)
      else return false
  | (Ast.Sequence(l1,_,r1),Ast.Sequence(l2,_,r2)) ->
      conjunct_bindings (unify_expression l1 l2) (unify_expression r1 r2)
  | (Ast.CondExpr(tst1,q1,thn1,c1,els1),Ast.CondExpr(tst2,q2,thn2,c2,els2)) ->
      conjunct_bindings (unify_expression tst1 tst2)
	(conjunct_bindings (unify_option unify_expression thn1 thn2)
	   (unify_expression els1 els2))
  | (Ast.Postfix(e1,op1),Ast.Postfix(e2,op2)) ->
      if unify_mcode op1 op2 then unify_expression e1 e2 else return false
  | (Ast.Infix(e1,op1),Ast.Infix(e2,op2)) ->
      if unify_mcode op1 op2 then unify_expression e1 e2 else return false
  | (Ast.Unary(e1,op1),Ast.Unary(e2,op2)) ->
      if unify_mcode op1 op2 then unify_expression e1 e2 else return false
  | (Ast.Binary(l1,op1,r1),Ast.Binary(l2,op2,r2)) ->
      if unify_mcode op1 op2
      then conjunct_bindings (unify_expression l1 l2) (unify_expression r1 r2)
      else return false
  | (Ast.ArrayAccess(ar1,lb1,e1,rb1),Ast.ArrayAccess(ar2,lb2,e2,rb2)) ->
      conjunct_bindings (unify_expression ar1 ar2) (unify_expression e1 e2)
  | (Ast.RecordAccess(e1,d1,fld1),Ast.RecordAccess(e2,d2,fld2)) ->
      conjunct_bindings (unify_expression e1 e2) (unify_ident fld1 fld2)
  | (Ast.RecordPtAccess(e1,pt1,fld1),Ast.RecordPtAccess(e2,pt2,fld2)) ->
      conjunct_bindings (unify_expression e1 e2) (unify_ident fld1 fld2)
  | (Ast.Cast(lp1,ty1,rp1,e1),Ast.Cast(lp2,ty2,rp2,e2)) ->
      conjunct_bindings (unify_fullType ty1 ty2) (unify_expression e1 e2)
  | (Ast.SizeOfExpr(szf1,e1),Ast.SizeOfExpr(szf2,e2)) ->
      unify_expression e1 e2
  | (Ast.SizeOfType(szf1,lp1,ty1,rp1),Ast.SizeOfType(szf2,lp2,ty2,rp2)) ->
      unify_fullType ty1 ty2
  | (Ast.TypeExp(ty1),Ast.TypeExp(ty2)) -> unify_fullType ty1 ty2
  | (Ast.Constructor(lp1,ty1,rp1,i1),Ast.Constructor(lp2,ty2,rp2,i2)) ->
      conjunct_bindings (unify_fullType ty1 ty2) (unify_initialiser i1 i2)
  | (Ast.Paren(lp1,e1,rp1),Ast.Paren(lp2,e2,rp2)) ->
      unify_expression e1 e2

  | (Ast.MetaErr(_,_,_,_),_)
  | (Ast.MetaExpr(_,_,_,_,_,_),_)
  | (Ast.MetaExprList(_,_,_,_),_)
  | (_,Ast.MetaErr(_,_,_,_))
  | (_,Ast.MetaExpr(_,_,_,_,_,_))
  | (_,Ast.MetaExprList(_,_,_,_)) -> return true

  | (Ast.AsExpr(exp1,asexp1),_) ->
      disjunct_all_bindings
	(List.map (function x -> unify_expression x e2) [exp1;asexp1])
  | (_,Ast.AsExpr(exp2,asexp2)) ->
      disjunct_all_bindings
	(List.map (function x -> unify_expression x e1) [exp2;asexp2])

  | (Ast.EComma(cm1),Ast.EComma(cm2)) -> return true

  | (Ast.DisjExpr(e1),_) ->
      disjunct_all_bindings (List.map (function x -> unify_expression x e2) e1)
  | (_,Ast.DisjExpr(e2)) ->
      disjunct_all_bindings (List.map (function x -> unify_expression e1 x) e2)
  | (Ast.NestExpr(_,e1,_,_,_),Ast.NestExpr(_,e2,_,_,_)) ->
      unify_dots unify_expression edots e1 e2

  (* dots can match against anything.  return true to be safe. *)
  | (Ast.Edots(_,_),_) | (_,Ast.Edots(_,_))
  | (Ast.Ecircles(_,_),_) | (_,Ast.Ecircles(_,_))
  | (Ast.Estars(_,_),_) | (_,Ast.Estars(_,_)) -> return true

  | (Ast.OptExp(_),_)
  | (Ast.UniqueExp(_),_)
  | (_,Ast.OptExp(_))
  | (_,Ast.UniqueExp(_)) -> failwith "unsupported expression"
  | _ -> return false

(* --------------------------------------------------------------------- *)
(* Strings *)

and unify_string_fragment e1 e2 =
  match (Ast.unwrap e1,Ast.unwrap e2) with
    (Ast.ConstantFragment(str1),Ast.ConstantFragment(str2)) ->
      return (unify_mcode str1 str2)
  | (Ast.FormatFragment(pct1,fmt1),Ast.FormatFragment(pct2,fmt2)) ->
      unify_string_format fmt1 fmt2
  | (Ast.Strdots(dots1),Ast.Strdots(dots2)) -> return true
  | (Ast.MetaFormatList(pct,name,len,_,_),_)
  | (_,Ast.MetaFormatList(pct,name,len,_,_)) -> return true
  | _ -> return false

and unify_string_format e1 e2 =
  match (Ast.unwrap e1,Ast.unwrap e2) with
    (Ast.ConstantFormat(str1), Ast.ConstantFormat(str2)) ->
      return (unify_mcode str1 str2)
  | (Ast.MetaFormat(name,_,_,_),_)
  | (_,Ast.MetaFormat(name,_,_,_)) -> return true

(* --------------------------------------------------------------------- *)
(* Types *)

and unify_fullType ft1 ft2 =
  match (Ast.unwrap ft1,Ast.unwrap ft2) with
    (Ast.Type(_,cv1,ty1),Ast.Type(_,cv2,ty2)) ->
      if bool_unify_option unify_mcode cv1 cv2
      then unify_typeC ty1 ty2
      else return false
  | (Ast.AsType(ty1,asty1),_) ->
      disjunct_all_bindings
	(List.map (function x -> unify_fullType x ft2) [ty1;asty1])
  | (_,Ast.AsType(ty2,asty2)) ->
      disjunct_all_bindings
	(List.map (function x -> unify_fullType x ft1) [ty2;asty2])
  | (Ast.DisjType(ft1),_) ->
      disjunct_all_bindings (List.map (function x -> unify_fullType x ft2) ft1)
  | (_,Ast.DisjType(ft2)) ->
      disjunct_all_bindings (List.map (function x -> unify_fullType ft1 x) ft2)

  | (Ast.OptType(_),_)
  | (Ast.UniqueType(_),_)
  | (_,Ast.OptType(_))
  | (_,Ast.UniqueType(_)) -> failwith "unsupported type"

and unify_typeC t1 t2 =
  match (Ast.unwrap t1,Ast.unwrap t2) with
    (Ast.BaseType(ty1,stringsa),Ast.BaseType(ty2,stringsb)) ->
      if ty1 = ty2
      then
	unify_lists ret_unify_mcode (function _ -> false (* not dots*))
	  stringsa stringsb
      else return false
  | (Ast.SignedT(sgn1,ty1),Ast.SignedT(sgn2,ty2)) ->
      if unify_mcode sgn1 sgn2
      then unify_option unify_typeC ty1 ty2
      else return false
  | (Ast.Pointer(ty1,s1),Ast.Pointer(ty2,s2)) -> unify_fullType ty1 ty2
  | (Ast.FunctionPointer(tya,lp1a,stara,rp1a,lp2a,paramsa,rp2a),
     Ast.FunctionPointer(tyb,lp1b,starb,rp1b,lp2b,paramsb,rp2b)) ->
       if List.for_all2 unify_mcode
	   [lp1a;stara;rp1a;lp2a;rp2a] [lp1b;starb;rp1b;lp2b;rp2b]
       then
	 conjunct_bindings (unify_fullType tya tyb)
	   (unify_dots unify_parameterTypeDef pdots paramsa paramsb)
       else return false
  | (Ast.FunctionType(_,tya,lp1a,paramsa,rp1a),
     Ast.FunctionType(_,tyb,lp1b,paramsb,rp1b)) ->
       if List.for_all2 unify_mcode [lp1a;rp1a] [lp1b;rp1b]
       then
	 conjunct_bindings (unify_option unify_fullType tya tyb)
	   (unify_dots unify_parameterTypeDef pdots paramsa paramsb)
       else return false
  | (Ast.FunctionType _ , _) -> failwith "not supported"
  | (Ast.Array(ty1,lb1,e1,rb1),Ast.Array(ty2,lb2,e2,rb2)) ->
      conjunct_bindings
	(unify_fullType ty1 ty2) (unify_option unify_expression e1 e2)
  | (Ast.Decimal(dec1,lp1,len1,comma1,prec_opt1,rp1),
     Ast.Decimal(dec2,lp2,len2,comma2,prec_opt2,rp2)) ->
       conjunct_bindings
	(unify_expression len1 len2)
	 (unify_option unify_expression prec_opt1 prec_opt2)
  | (Ast.EnumName(s1,Some ts1),Ast.EnumName(s2,Some ts2)) ->
      if unify_mcode s1 s2 then unify_ident ts1 ts2 else return false
  | (Ast.EnumName(s1,None),Ast.EnumName(s2,None)) ->
      return true
  | (Ast.EnumDef(ty1,lb1,ids1,rb1),Ast.EnumDef(ty2,lb2,ids2,rb2)) ->
       conjunct_bindings (unify_fullType ty1 ty2)
	 (unify_dots unify_expression edots ids1 ids2)
  | (Ast.StructUnionName(s1,Some ts1),Ast.StructUnionName(s2,Some ts2)) ->
      if unify_mcode s1 s2 then unify_ident ts1 ts2 else return false
  | (Ast.StructUnionName(s1,None),Ast.StructUnionName(s2,None)) ->
      return (unify_mcode s1 s2)
  | (Ast.StructUnionDef(ty1,lb1,decls1,rb1),
     Ast.StructUnionDef(ty2,lb2,decls2,rb2)) ->
       conjunct_bindings (unify_fullType ty1 ty2)
	 (unify_dots unify_annotated_decl ddots decls1 decls2)
  | (Ast.TypeName(t1),Ast.TypeName(t2)) -> return (unify_mcode t1 t2)

  | (Ast.MetaType(_,_,_),_)
  | (_,Ast.MetaType(_,_,_)) -> return true
  | _ -> return false

(* --------------------------------------------------------------------- *)
(* Variable declaration *)
(* Even if the Cocci program specifies a list of declarations, they are
   split out into multiple declarations of a single variable each. *)

and unify_declaration d1 d2 =
  match (Ast.unwrap d1,Ast.unwrap d2) with
    (Ast.MetaDecl(_,_,_),_) | (_,Ast.MetaDecl(_,_,_)) -> return true
  | (Ast.MetaField(_,_,_),_) | (_,Ast.MetaField(_,_,_)) -> return true
  | (Ast.MetaFieldList(_,_,_,_),_) | (_,Ast.MetaFieldList(_,_,_,_)) ->
      return true
  | (Ast.Init(stg1,ft1,id1,eq1,i1,s1),Ast.Init(stg2,ft2,id2,eq2,i2,s2)) ->
      if bool_unify_option unify_mcode stg1 stg2
      then
	conjunct_bindings (unify_fullType ft1 ft2)
	  (conjunct_bindings (unify_ident id1 id2) (unify_initialiser i1 i2))
      else return false
  | (Ast.UnInit(stg1,ft1,id1,s1),Ast.UnInit(stg2,ft2,id2,s2)) ->
      if bool_unify_option unify_mcode stg1 stg2
      then conjunct_bindings (unify_fullType ft1 ft2) (unify_ident id1 id2)
      else return false
  | (Ast.MacroDecl(n1,lp1,args1,rp1,sem1),
     Ast.MacroDecl(n2,lp2,args2,rp2,sem2)) ->
       conjunct_bindings (unify_ident n1 n2)
	 (unify_dots unify_expression edots args1 args2)
  | (Ast.MacroDeclInit(n1,lp1,args1,rp1,eq1,ini1,sem1),
     Ast.MacroDeclInit(n2,lp2,args2,rp2,eq2,ini2,sem2)) ->
       conjunct_bindings (unify_ident n1 n2)
	 (conjunct_bindings (unify_dots unify_expression edots args1 args2)
	    (unify_initialiser ini1 ini2))
  | (Ast.TyDecl(ft1,s1),Ast.TyDecl(ft2,s2)) -> unify_fullType ft1 ft2
  | (Ast.Typedef(stg1,ft1,id1,s1),Ast.Typedef(stg2,ft2,id2,s2)) ->
      conjunct_bindings (unify_fullType ft1 ft2) (unify_typeC id1 id2)
  | (Ast.DisjDecl(d1),_) ->
      disjunct_all_bindings
	(List.map (function x -> unify_declaration x d2) d1)
  | (_,Ast.DisjDecl(d2)) ->
      disjunct_all_bindings
	(List.map (function x -> unify_declaration d1 x) d2)

  | (Ast.OptDecl(_),_)
  | (Ast.UniqueDecl(_),_)
  | (_,Ast.OptDecl(_))
  | (_,Ast.UniqueDecl(_)) -> failwith "unsupported decl"
  | _ -> return false

and unify_annotated_decl d1 d2 =
  match (Ast.unwrap d1,Ast.unwrap d2) with
    (Ast.DElem(_,_,d1),Ast.DElem(_,_,d2)) -> unify_declaration d1 d2
  (* dots can match against anything.  return true to be safe. *)
  | (Ast.Ddots(_,_),_) | (_,Ast.Ddots(_,_)) -> return true

(* --------------------------------------------------------------------- *)
(* Initializer *)

and unify_initialiser i1 i2 =
  match (Ast.unwrap i1,Ast.unwrap i2) with
    (Ast.MetaInit(_,_,_),_) | (_,Ast.MetaInit(_,_,_)) -> return true
  | (Ast.MetaInitList(_,_,_,_),_) | (_,Ast.MetaInitList(_,_,_,_)) ->
      return true
  | (Ast.InitExpr(expa),Ast.InitExpr(expb)) ->
      unify_expression expa expb
  | (Ast.ArInitList(_,initlista,_),
     Ast.ArInitList(_,initlistb,_)) ->
      (* ignore whencode - returns true safely *)
      unify_dots unify_initialiser idots initlista initlistb
  | (Ast.StrInitList(_,_,initlista,_,whena),
     Ast.StrInitList(_,_,initlistb,_,whenb)) ->
      (* ignore whencode - returns true safely *)
      unify_lists unify_initialiser (function _ -> false) initlista initlistb
  | (Ast.InitGccExt(designatorsa,_,inia),
     Ast.InitGccExt(designatorsb,_,inib)) ->
       conjunct_bindings
	 (unify_lists unify_designator (function _ -> false)
	    designatorsa designatorsb)
	 (unify_initialiser inia inib)
  | (Ast.InitGccName(namea,_,inia),Ast.InitGccName(nameb,_,inib)) ->
      conjunct_bindings (unify_ident namea nameb) (unify_initialiser inia inib)

  | (Ast.OptIni(_),_)
  | (Ast.UniqueIni(_),_)
  | (_,Ast.OptIni(_))
  | (_,Ast.UniqueIni(_)) -> failwith "unsupported decl"
  | _ -> return false

and unify_designator d1 d2 =
  match (d1,d2) with
    (Ast.DesignatorField(_,idb),Ast.DesignatorField(_,ida)) ->
      unify_ident ida idb
  | (Ast.DesignatorIndex(_,expa,_),Ast.DesignatorIndex(_,expb,_)) ->
      unify_expression expa expb
  | (Ast.DesignatorRange(_,mina,_,maxa,_),
     Ast.DesignatorRange(_,minb,_,maxb,_)) ->
       conjunct_bindings (unify_expression mina minb)
	 (unify_expression maxa maxb)
  | _ -> return false

(* --------------------------------------------------------------------- *)
(* Parameter *)

and unify_parameterTypeDef p1 p2 =
  match (Ast.unwrap p1,Ast.unwrap p2) with
    (Ast.VoidParam(ft1),Ast.VoidParam(ft2)) -> unify_fullType ft1 ft2
  | (Ast.Param(ft1,i1),Ast.Param(ft2,i2)) ->
      conjunct_bindings (unify_fullType ft1 ft2)
	(unify_option unify_ident i1 i2)

  | (Ast.MetaParam(_,_,_),_)
  | (Ast.MetaParamList(_,_,_,_),_)
  | (_,Ast.MetaParam(_,_,_))
  | (_,Ast.MetaParamList(_,_,_,_)) -> return true

  | (Ast.PComma(_),Ast.PComma(_)) -> return true

  (* dots can match against anything.  return true to be safe. *)
  | (Ast.Pdots(_),_) | (_,Ast.Pdots(_))
  | (Ast.Pcircles(_),_) | (_,Ast.Pcircles(_)) -> return true

  (* not sure what to do with the asexp.... *)
  | (Ast.AsParam(param1,asexp1),_) -> unify_parameterTypeDef param1 p2
  | (_,Ast.AsParam(param2,asexp2)) -> unify_parameterTypeDef p1 param2

  | (Ast.OptParam(_),_)
  | (Ast.UniqueParam(_),_)
  | (_,Ast.OptParam(_))
  | (_,Ast.UniqueParam(_)) -> failwith "unsupported parameter"
  | _ -> return false

(* --------------------------------------------------------------------- *)
(* Define parameter *)

and unify_define_parameters p1 p2 =
 match (Ast.unwrap p1,Ast.unwrap p2) with
    (Ast.NoParams,Ast.NoParams) -> return true
  | (Ast.DParams(lp1,params1,rp1),Ast.DParams(lp2,params2,rp2)) ->
      unify_dots unify_define_param dpdots params1 params2
  | _ -> return false

and unify_define_param p1 p2 =
  match (Ast.unwrap p1,Ast.unwrap p2) with
    (Ast.DParam(i1),Ast.DParam(i2)) ->
	(unify_ident i1 i2)
  | (Ast.DPComma(_),Ast.DPComma(_)) -> return true

  (* dots can match against anything.  return true to be safe. *)
  | (Ast.DPdots(_),_) | (_,Ast.DPdots(_))
  | (Ast.DPcircles(_),_) | (_,Ast.DPcircles(_)) -> return true

  | (Ast.OptDParam(_),_)
  | (Ast.UniqueDParam(_),_)
  | (_,Ast.OptDParam(_))
  | (_,Ast.UniqueDParam(_)) -> failwith "unsupported parameter"
  | _ -> return false

(* --------------------------------------------------------------------- *)
(* Top-level code *)

and unify_rule_elem re1 re2 =
  match (Ast.unwrap re1,Ast.unwrap re2) with
    (Ast.FunHeader(_,_,fi1,nm1,lp1,params1,rp1),
     Ast.FunHeader(_,_,fi2,nm2,lp2,params2,rp2)) ->
       conjunct_bindings (unify_fninfo fi1 fi2)
	 (conjunct_bindings (unify_ident nm1 nm2)
	    (unify_dots unify_parameterTypeDef pdots params1 params2))
  | (Ast.Decl d1,Ast.Decl d2) -> unify_annotated_decl d1 d2

  | (Ast.SeqStart(lb1),Ast.SeqStart(lb2)) -> return true
  | (Ast.SeqEnd(rb1),Ast.SeqEnd(rb2)) -> return true

  | (Ast.ExprStatement(e1,s1),Ast.ExprStatement(e2,s2)) ->
      unify_option unify_expression e1 e2
  | (Ast.IfHeader(if1,lp1,e1,rp1),Ast.IfHeader(if2,lp2,e2,rp2)) ->
      unify_expression e1 e2
  | (Ast.Else(e1),Ast.Else(e2)) -> return true
  | (Ast.WhileHeader(wh1,lp1,e1,rp1),Ast.WhileHeader(wh2,lp2,e2,rp2)) ->
      unify_expression e1 e2
  | (Ast.DoHeader(d1),Ast.DoHeader(d2)) -> return true
  | (Ast.WhileTail(wh1,lp1,e1,rp1,s1),Ast.WhileTail(wh2,lp2,e2,rp2,s2)) ->
      unify_expression e1 e2
  | (Ast.ForHeader(fr1,lp1,first1,e21,s21,e31,rp1),
     Ast.ForHeader(fr2,lp2,first2,e22,s22,e32,rp2)) ->
       let first =
	 match (first1,first2) with
	   (Ast.ForExp(e11,s11),Ast.ForExp(e12,s1)) ->
	     unify_option unify_expression e11 e12
	 | (Ast.ForDecl d1,Ast.ForDecl d2) -> unify_annotated_decl d1 d2
	 | _ -> return false in
       conjunct_bindings first
	 (conjunct_bindings
	    (unify_option unify_expression e21 e22)
	    (unify_option unify_expression e31 e32))
  | (Ast.IteratorHeader(nm1,lp1,args1,rp1),
     Ast.IteratorHeader(nm2,lp2,args2,rp2)) ->
      conjunct_bindings (unify_ident nm1 nm2)
	 (unify_dots unify_expression edots args1 args2)
  | (Ast.Undef(_,n1),Ast.Undef(_,n2)) -> unify_ident n1 n2
  | (Ast.DefineHeader(_,n1,p1),Ast.DefineHeader(_,n2,p2)) ->
      conjunct_bindings (unify_ident n1 n2)
	(unify_define_parameters p1 p2)
  | (Ast.Pragma(_,i1,n1),Ast.Pragma(_,i2,n2)) ->
      conjunct_bindings (unify_ident i1 i2) (unify_pragmainfo n1 n2)
  | (Ast.Break(r1,s1),Ast.Break(r2,s2)) -> return true
  | (Ast.Continue(r1,s1),Ast.Continue(r2,s2)) -> return true
  | (Ast.Label(l1,dd1),Ast.Label(l2,dd2)) -> unify_ident l1 l2
  | (Ast.Goto(g1,l1,dd1),Ast.Goto(g2,l2,dd2)) -> unify_ident l1 l2
  | (Ast.Return(r1,s1),Ast.Return(r2,s2)) -> return true
  | (Ast.ReturnExpr(r1,e1,s1),Ast.ReturnExpr(r2,e2,s2)) ->
      unify_expression e1 e2
  | (Ast.Exec(exec1,lang1,code1,sem1),Ast.Exec(exec2,lang2,code2,sem2)) ->
      if unify_mcode lang1 lang2
      then unify_dots unify_exec_code ecdots code1 code2
      else return false

  | (Ast.DisjRuleElem(res1),_) ->
      disjunct_all_bindings
	(List.map (function x -> unify_rule_elem x re2) res1)
  | (_,Ast.DisjRuleElem(res2)) ->
      disjunct_all_bindings
	(List.map (function x -> unify_rule_elem re1 x) res2)

  | (Ast.MetaRuleElem(_,_,_),_)
  | (Ast.MetaStmt(_,_,_,_),_)
  | (Ast.MetaStmtList(_,_,_),_)
  | (_,Ast.MetaRuleElem(_,_,_))
  | (_,Ast.MetaStmt(_,_,_,_))
  | (_,Ast.MetaStmtList(_,_,_)) -> return true

    (* can match a rule_elem in different parts *)
  | (Ast.Exp(e1),Ast.Exp(e2)) -> return true
  | (Ast.Exp(e1),_) -> subexp (unify_expression e1) re2
  | (_,Ast.Exp(e2)) -> subexp (unify_expression e2) re1

  | (Ast.TopExp(e1),Ast.TopExp(e2)) -> unify_expression e1 e2
  | (Ast.TopInit(i1),Ast.TopInit(i2)) -> unify_initialiser i1 i2

    (* can match a rule_elem in different parts *)
  | (Ast.Ty(t1),Ast.Ty(t2)) -> return true
  | (Ast.Ty(t1),_) -> subtype (unify_fullType t1) re2
  | (_,Ast.Ty(t2)) -> subtype (unify_fullType t2) re1
  | _ -> return false

and unify_pragmainfo pi1 pi2 =
  match (Ast.unwrap pi1,Ast.unwrap pi2) with
      (Ast.PragmaTuple(lp1,args1,rp1),Ast.PragmaTuple(lp2,args2,rp2)) ->
	unify_dots unify_expression edots args1 args2
    | (Ast.PragmaIdList(ids1),Ast.PragmaIdList(ids2)) ->
	unify_dots unify_ident (function _ -> false) ids1 ids2
    | (Ast.PragmaDots(_),_) | (_,Ast.PragmaDots(_)) -> return true
    | _ -> return false

and unify_fninfo patterninfo cinfo =
  let patterninfo = List.sort compare patterninfo in
  let cinfo = List.sort compare cinfo in
  let rec loop = function
      (Ast.FStorage(sta)::resta,Ast.FStorage(stb)::restb) ->
      if unify_mcode sta stb then loop (resta,restb) else return false
    | (Ast.FType(tya)::resta,Ast.FType(tyb)::restb) ->
	conjunct_bindings (unify_fullType tya tyb) (loop (resta,restb))
    | (Ast.FInline(ia)::resta,Ast.FInline(ib)::restb) ->
	if unify_mcode ia ib then loop (resta,restb) else return false
    | (Ast.FAttr(ia)::resta,Ast.FAttr(ib)::restb) ->
	if unify_mcode ia ib then loop (resta,restb) else return false
    | (x::resta,((y::_) as restb)) ->
	(match compare x y with
	  -1 -> return false
	| 1 -> loop (resta,restb)
	| _ -> failwith "not possible")
    | _ -> return false in
  loop (patterninfo,cinfo)

and unify_exec_code ec1 ec2 =
  match (Ast.unwrap ec1,Ast.unwrap ec2) with
    (Ast.ExecEval(colon1,id1),Ast.ExecEval(colon2,id2)) ->
      unify_expression id1 id2
  | (Ast.ExecToken(tok1),Ast.ExecToken(tok2)) ->
      return (unify_mcode tok1 tok2)
  | (Ast.ExecDots(_),_) | (_,Ast.ExecDots(_)) ->  return true
  | _ -> return false

and subexp f =
  let bind = conjunct_bindings in
  let option_default = return false in
  let mcode r e = option_default in
  let expr r k e = conjunct_bindings (f e) (k e) in
  let donothing r k e = k e in
  let recursor = V.combiner bind option_default
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      donothing donothing donothing donothing donothing donothing expr 
      donothing donothing donothing donothing donothing donothing
      donothing donothing donothing donothing donothing donothing donothing in
  recursor.V.combiner_rule_elem

and subtype f =
  let bind = conjunct_bindings in
  let option_default = return false in
  let mcode r e = option_default in
  let fullType r k e = conjunct_bindings (f e) (k e) in
  let donothing r k e = k e in
  let recursor = V.combiner bind option_default
      mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode mcode
      donothing donothing donothing donothing donothing donothing
      donothing donothing donothing fullType donothing donothing donothing
      donothing donothing donothing donothing donothing donothing donothing in
  recursor.V.combiner_rule_elem

let rec unify_statement s1 s2 =
  match (Ast.unwrap s1,Ast.unwrap s2) with
    (Ast.Seq(lb1,s1,rb1),Ast.Seq(lb2,s2,rb2)) ->
      conjunct_bindings (unify_rule_elem lb1 lb2)
	(conjunct_bindings
	   (unify_dots unify_statement sdots s1 s2)
	   (unify_rule_elem rb1 rb2))
  | (Ast.IfThen(h1,thn1,_),Ast.IfThen(h2,thn2,_)) ->
      conjunct_bindings (unify_rule_elem h1 h2) (unify_statement thn1 thn2)
  | (Ast.IfThenElse(h1,thn1,e1,els1,_),Ast.IfThenElse(h2,thn2,e2,els2,_)) ->
      conjunct_bindings (unify_rule_elem h1 h2)
	(conjunct_bindings (unify_statement thn1 thn2)
	   (conjunct_bindings (unify_rule_elem e1 e2)
	      (unify_statement els1 els2)))
  | (Ast.While(h1,s1,_),Ast.While(h2,s2,_)) ->
      conjunct_bindings (unify_rule_elem h1 h2) (unify_statement s1 s2)
  | (Ast.Do(h1,s1,t1),Ast.Do(h2,s2,t2)) ->
      conjunct_bindings (unify_rule_elem h1 h2)
	(conjunct_bindings (unify_statement s1 s2) (unify_rule_elem t1 t2))
  | (Ast.For(h1,s1,_),Ast.For(h2,s2,_)) ->
      conjunct_bindings (unify_rule_elem h1 h2) (unify_statement s1 s2)
  | (Ast.Atomic(re1),Ast.Atomic(re2)) -> unify_rule_elem re1 re2
  | (Ast.Disj(s1),_) ->
      let s2 = Ast.rewrap s2 (Ast.DOTS[s2]) in
      disjunct_all_bindings
	(List.map
	   (function x -> unify_dots unify_statement sdots x s2)
	   s1)
  | (_,Ast.Disj(s2)) ->
      let s1 = Ast.rewrap s1 (Ast.DOTS[s1]) in
      disjunct_all_bindings
	(List.map
	   (function x -> unify_dots unify_statement sdots s1 x)
	   s2)
  | (Ast.Nest(_,s1,_,_,_,_,_),Ast.Nest(_,s2,_,_,_,_,_)) ->
      unify_dots unify_statement sdots s1 s2
  | (Ast.FunDecl(h1,lb1,s1,rb1,_),Ast.FunDecl(h2,lb2,s2,rb2,_)) ->
      conjunct_bindings (unify_rule_elem h1 h2)
	(conjunct_bindings (unify_rule_elem lb1 lb2)
	   (conjunct_bindings (unify_dots unify_statement sdots s1 s2)
	      (unify_rule_elem rb1 rb2)))
  | (Ast.Define(h1,s1),Ast.Define(h2,s2)) ->
      conjunct_bindings (unify_rule_elem h1 h2)
	(unify_dots unify_statement sdots s1 s2)
  (* dots can match against anything.  return true to be safe. *)
  | (Ast.Dots(_,_,_,_),_) | (_,Ast.Dots(_,_,_,_))
  | (Ast.Circles(_,_,_,_),_) | (_,Ast.Circles(_,_,_,_))
  | (Ast.Stars(_,_,_,_),_) | (_,Ast.Stars(_,_,_,_)) -> return true
  | (Ast.OptStm(_),_)
  | (Ast.UniqueStm(_),_)
  | (_,Ast.OptStm(_))
  | (_,Ast.UniqueStm(_)) -> failwith "unsupported statement"
  | _ -> return false

let unify_statement_dots = unify_dots unify_statement sdots
