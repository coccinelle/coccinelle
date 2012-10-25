(*
 * Copyright 2012, INRIA
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


# 0 "./pretty_print_cocci.ml"
open Format
module Ast = Ast_cocci

let print_plus_flag = ref true
let print_minus_flag = ref true
let print_newlines_disj = ref true

let start_block str =
  force_newline(); print_string "  "; open_box 0

let end_block str =
  close_box(); force_newline ()

let print_string_box s = print_string s; open_box 0


let print_option = Common.do_option
let print_between = Common.print_between

(* --------------------------------------------------------------------- *)
(* Modified code *)

(* avoid polyvariance problems *)
let anything : (Ast.anything -> unit) ref = ref (function _ -> ())

let rec print_anything str = function
    [] -> ()
  | stream ->
      start_block();
      print_between force_newline
	(function x ->
	  print_string str; open_box 0; print_anything_list x; close_box())
	stream;
      end_block()

and print_anything_list = function
    [] -> ()
  | [x] -> !anything x
  | bef::((aft::_) as rest) ->
      !anything bef;
      let space =
	(match bef with
	  Ast.Rule_elemTag(_) | Ast.AssignOpTag(_) | Ast.BinaryOpTag(_)
	| Ast.ArithOpTag(_) | Ast.LogicalOpTag(_)
	| Ast.Token("if",_) | Ast.Token("while",_) -> true | _ -> false) or
	(match aft with
	  Ast.Rule_elemTag(_) | Ast.AssignOpTag(_) | Ast.BinaryOpTag(_)
	| Ast.ArithOpTag(_) | Ast.LogicalOpTag(_) | Ast.Token("{",_) -> true
	| _ -> false) in
      if space then print_string " ";
      print_anything_list rest

let print_around printer term = function
    Ast.NOTHING -> printer term
  | Ast.BEFORE(bef,_) -> print_anything "<<< " bef; printer term
  | Ast.AFTER(aft,_) -> printer term; print_anything ">>> " aft
  | Ast.BEFOREAFTER(bef,aft,_) ->
      print_anything "<<< " bef; printer term; print_anything ">>> " aft

let print_string_befaft fn x info =
  let print = function
      Ast.Noindent s | Ast.Indent s | Ast.Space s -> print_string s in
  List.iter (function (s,_,_) -> print s; force_newline()) info.Ast.strbef;
  fn x;
  List.iter (function (s,_,_) -> force_newline(); print s) info.Ast.straft

let print_meta (r,x) = print_string r; print_string ":"; print_string x

let print_pos l =
  List.iter
    (function
	Ast.MetaPos(name,_,_,_,_) ->
	  let name = Ast.unwrap_mcode name in
	  print_string "@"; print_meta name)
    l

let mcode fn = function
    (x, _, Ast.MINUS(_,_,adj,plus_stream), pos) ->
      if !print_minus_flag
      then print_string (if !Flag.sgrep_mode2 then "*" else "-");
      fn x; print_pos pos;
      if !print_plus_flag
      then
	(match plus_stream with
	  Ast.NOREPLACEMENT -> ()
	| Ast.REPLACEMENT(plus_stream,_) -> print_anything ">>> " plus_stream)
  | (x, _, Ast.CONTEXT(_,plus_streams), pos) ->
      if !print_plus_flag
      then
	let fn x = fn x; print_pos pos in
	print_around fn x plus_streams
      else (fn x; print_pos pos)
  | (x, info, Ast.PLUS _, pos) ->
      let fn x = fn x; print_pos pos in
      print_string_befaft fn x info

let print_mcodekind = function
    Ast.MINUS(_,_,_,plus_stream) ->
      print_string "MINUS";
      (match plus_stream with
	Ast.NOREPLACEMENT -> ()
      | Ast.REPLACEMENT(plus_stream,_) -> print_anything ">>> " plus_stream)
  | Ast.CONTEXT(_,plus_streams) ->
      print_around (function _ -> print_string "CONTEXT") () plus_streams
  | Ast.PLUS _ -> print_string "PLUS"

(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
(* Dots *)

let dots between fn d =
  match Ast.unwrap d with
    Ast.DOTS(l) -> print_between between fn l
  | Ast.CIRCLES(l) -> print_between between fn l
  | Ast.STARS(l) -> print_between between fn l

let nest_dots starter ender fn f d =
  mcode print_string starter;
  f(); start_block();
  (match Ast.unwrap d with
    Ast.DOTS(l)    -> print_between force_newline fn l
  | Ast.CIRCLES(l) -> print_between force_newline fn l
  | Ast.STARS(l)   -> print_between force_newline fn l);
  end_block();
  mcode print_string ender

(* --------------------------------------------------------------------- *)
(* Disjunctions *)

let print_disj_list fn l =
  if !print_newlines_disj
  then (force_newline(); print_string "("; force_newline())
  else print_string "(";
  print_between
    (function _ ->
      if !print_newlines_disj
      then (force_newline(); print_string "|"; force_newline())
      else print_string " | ")
    fn l;
  if !print_newlines_disj
  then (force_newline(); print_string ")"; force_newline())
  else print_string ")"

(* --------------------------------------------------------------------- *)

let print_type keep info = function
    None -> ()
	(* print_string "/* ";
           print_string "keep:"; print_unitary keep;
           print_string " inherited:"; print_bool inherited;
           print_string " */"*)
  | Some ty -> ()
      (*;
      print_string "/* ";
      print_between (function _ -> print_string ", ") Type_cocci.typeC ty;(*
      print_string "keep:"; print_unitary keep;
      print_string " inherited:"; print_bool inherited;*)
      print_string " */"*)

(* --------------------------------------------------------------------- *)
(* Contraint on Identifier and Function *)
(* FIXME: Not called at the moment *)

let rec idconstraint = function
    Ast.IdNoConstraint  -> print_string "/* No constraint */"
  | Ast.IdNegIdSet (str,meta)     ->
      List.iter (function s -> print_string (" "^s)) str;
      List.iter (function (r,n) -> print_string " "; print_meta(r,n)) meta
  | Ast.IdRegExpConstraint re -> regconstraint re

and regconstraint = function
    Ast.IdRegExp (re,_) ->
      print_string "~= \""; print_string re; print_string "\""
  | Ast.IdNotRegExp (re,_) ->
      print_string "~!= \""; print_string re; print_string "\""

(* --------------------------------------------------------------------- *)
(* Identifier *)

let rec ident i =
  match Ast.unwrap i with
    Ast.Id(name) -> mcode print_string name
  | Ast.MetaId(name,_,keep,inherited) -> mcode print_meta name
  | Ast.MetaFunc(name,_,_,_) -> mcode print_meta name
  | Ast.MetaLocalFunc(name,_,_,_) -> mcode print_meta name
  | Ast.AsIdent(id,asid) -> ident id; print_string "@"; ident asid
  | Ast.DisjId(id_list) -> print_disj_list ident id_list
  | Ast.OptIdent(id) -> print_string "?"; ident id
  | Ast.UniqueIdent(id) -> print_string "!"; ident id

and print_unitary = function
    Type_cocci.Unitary -> print_string "unitary"
  | Type_cocci.Nonunitary -> print_string "nonunitary"
  | Type_cocci.Saved -> print_string "saved"

(* --------------------------------------------------------------------- *)
(* Expression *)

let rec expression e =
  match Ast.unwrap e with
    Ast.Ident(id) -> ident id
  | Ast.Constant(const) -> mcode constant const
  | Ast.FunCall(fn,lp,args,rp) ->
      expression fn; mcode print_string_box lp;
      dots (function _ -> ()) expression args;
      close_box(); mcode print_string rp
  | Ast.Assignment(left,op,right,simple) ->
      expression left; print_string " "; mcode assignOp op;
      print_string " "; expression right
  | Ast.Sequence(left,op,right) ->
      expression left; mcode print_string op;
      print_string " "; expression right
  | Ast.CondExpr(exp1,why,exp2,colon,exp3) ->
      expression exp1; print_string " "; mcode print_string why;
      print_option (function e -> print_string " "; expression e) exp2;
      print_string " "; mcode print_string colon; expression exp3
  | Ast.Postfix(exp,op) -> expression exp; mcode fixOp op
  | Ast.Infix(exp,op) -> mcode fixOp op; expression exp
  | Ast.Unary(exp,op) -> mcode unaryOp op; expression exp
  | Ast.Binary(left,op,right) ->
      expression left; print_string " "; mcode binaryOp op; print_string " ";
      expression right
  | Ast.Nested(left,op,right) ->
      expression left; print_string " "; mcode binaryOp op; print_string " ";
      expression right
  | Ast.Paren(lp,exp,rp) ->
      mcode print_string_box lp; expression exp; close_box();
      mcode print_string rp
  | Ast.ArrayAccess(exp1,lb,exp2,rb) ->
      expression exp1; mcode print_string_box lb; expression exp2; close_box();
      mcode print_string rb
  | Ast.RecordAccess(exp,pt,field) ->
      expression exp; mcode print_string pt; ident field
  | Ast.RecordPtAccess(exp,ar,field) ->
      expression exp; mcode print_string ar; ident field
  | Ast.Cast(lp,ty,rp,exp) ->
      mcode print_string_box lp; fullType ty; close_box();
      mcode print_string rp; expression exp
  | Ast.SizeOfExpr(sizeof,exp) ->
      mcode print_string sizeof; expression exp
  | Ast.SizeOfType(sizeof,lp,ty,rp) ->
      mcode print_string sizeof;
      mcode print_string_box lp; fullType ty; close_box();
      mcode print_string rp
  | Ast.TypeExp(ty) -> fullType ty
  | Ast.Constructor(lp,ty,rp,init) ->
      mcode print_string_box lp; fullType ty; close_box();
      mcode print_string rp; initialiser init

  | Ast.MetaErr(name,_,_,_) -> mcode print_meta name
  | Ast.MetaExpr(name,_,keep,ty,form,inherited) ->
      mcode print_meta name; print_type keep inherited ty
  | Ast.MetaExprList(name,_,_,_) -> mcode print_meta name
  | Ast.AsExpr(exp,asexp) -> expression exp; print_string "@"; expression asexp
  | Ast.EComma(cm) -> mcode print_string cm; print_space()
  | Ast.DisjExpr(exp_list) -> print_disj_list expression exp_list
  | Ast.NestExpr(starter,expr_dots,ender,Some whencode,multi) ->
      nest_dots starter ender expression
	(function _ -> print_string "   when != "; expression whencode)
	expr_dots
  | Ast.NestExpr(starter,expr_dots,ender,None,multi) ->
      nest_dots starter ender expression (function _ -> ()) expr_dots
  | Ast.Edots(dots,Some whencode)
  | Ast.Ecircles(dots,Some whencode)
  | Ast.Estars(dots,Some whencode) ->
      mcode print_string dots; print_string "   when != "; expression whencode
  | Ast.Edots(dots,None)
  | Ast.Ecircles(dots,None)
  | Ast.Estars(dots,None) -> mcode print_string dots
  | Ast.OptExp(exp) -> print_string "?"; expression exp
  | Ast.UniqueExp(exp) -> print_string "!"; expression exp

and  unaryOp = function
    Ast.GetRef -> print_string "&"
  | Ast.GetRefLabel -> print_string "&&"
  | Ast.DeRef -> print_string "*"
  | Ast.UnPlus -> print_string "+"
  | Ast.UnMinus -> print_string "-"
  | Ast.Tilde -> print_string "~"
  | Ast.Not -> print_string "!"

and  assignOp = function
    Ast.SimpleAssign -> print_string "="
  | Ast.OpAssign(aop) -> arithOp aop; print_string "="

and  fixOp = function
    Ast.Dec -> print_string "--"
  | Ast.Inc -> print_string "++"

and  binaryOp = function
    Ast.Arith(aop) -> arithOp aop
  | Ast.Logical(lop) -> logicalOp lop

and  arithOp = function
    Ast.Plus -> print_string "+"
  | Ast.Minus -> print_string "-"
  | Ast.Mul -> print_string "*"
  | Ast.Div -> print_string "/"
  | Ast.Min -> print_string "<?"
  | Ast.Max -> print_string ">?"
  | Ast.Mod -> print_string "%"
  | Ast.DecLeft -> print_string "<<"
  | Ast.DecRight -> print_string ">>"
  | Ast.And -> print_string "&"
  | Ast.Or -> print_string "|"
  | Ast.Xor -> print_string "^"

and  logicalOp = function
    Ast.Inf -> print_string "<"
  | Ast.Sup -> print_string ">"
  | Ast.InfEq -> print_string "<="
  | Ast.SupEq -> print_string ">="
  | Ast.Eq -> print_string "=="
  | Ast.NotEq -> print_string "!="
  | Ast.AndLog -> print_string "&&"
  | Ast.OrLog -> print_string "||"

and constant = function
    Ast.String(s) -> print_string "\""; print_string s; print_string "\""
  | Ast.Char(s) -> print_string "'"; print_string s; print_string "'"
  | Ast.Int(s) -> print_string s
  | Ast.Float(s) -> print_string s

(* --------------------------------------------------------------------- *)
(* Declarations *)

and storage = function
    Ast.Static -> print_string "static "
  | Ast.Auto -> print_string "auto "
  | Ast.Register -> print_string "register "
  | Ast.Extern -> print_string "extern "

(* --------------------------------------------------------------------- *)
(* Types *)

and fullType ft =
  match Ast.unwrap ft with
    Ast.Type(_,cv,ty) ->
      (match Ast.unwrap ty with
	Ast.Pointer(_,_) ->
	  typeC ty;
	  print_option (function x -> print_string " "; mcode const_vol x) cv
      |	_ ->
	  print_option (function x -> mcode const_vol x; print_string " ") cv;
	  typeC ty)
  | Ast.AsType(ty,asty) -> fullType ty; print_string "@"; fullType asty
  | Ast.DisjType(decls) -> print_disj_list fullType decls
  | Ast.OptType(ty) -> print_string "?"; fullType ty
  | Ast.UniqueType(ty) -> print_string "!"; fullType ty

and print_function_pointer (ty,lp1,star,rp1,lp2,params,rp2) fn =
  fullType ty; mcode print_string lp1; mcode print_string star; fn();
  mcode print_string rp1; mcode print_string lp1;
  parameter_list params; mcode print_string rp2

and print_function_type (ty,lp1,params,rp1) fn =
  print_option fullType ty; fn(); mcode print_string lp1;
  parameter_list params; mcode print_string rp1

and print_fninfo = function
    Ast.FStorage(stg) -> mcode storage stg
  | Ast.FType(ty) -> fullType ty
  | Ast.FInline(inline) -> mcode print_string inline; print_string " "
  | Ast.FAttr(attr) -> mcode print_string attr; print_string " "

and typeC ty =
  match Ast.unwrap ty with
    Ast.BaseType(ty,strings) ->
      List.iter (function s -> mcode print_string s; print_string " ") strings
  | Ast.SignedT(sgn,ty) -> mcode sign sgn; print_option typeC ty
  | Ast.Pointer(ty,star) -> fullType ty; mcode print_string star
  | Ast.FunctionPointer(ty,lp1,star,rp1,lp2,params,rp2) ->
      print_function_pointer (ty,lp1,star,rp1,lp2,params,rp2)
	(function _ -> ())
  | Ast.FunctionType (_,ty,lp1,params,rp1) ->
      print_function_type (ty,lp1,params,rp1) (function _ -> ())
  | Ast.Array(ty,lb,size,rb) ->
      fullType ty; mcode print_string lb; print_option expression size;
      mcode print_string rb
  | Ast.EnumName(kind,name) ->
      mcode print_string kind;
      print_option (function x -> ident x; print_string " ") name
  | Ast.EnumDef(ty,lb,ids,rb) ->
      fullType ty; mcode print_string lb;
      dots force_newline expression ids;
      mcode print_string rb
  | Ast.StructUnionName(kind,name) ->
      mcode structUnion kind;
      print_option (function x -> ident x; print_string " ") name
  | Ast.StructUnionDef(ty,lb,decls,rb) ->
      fullType ty; mcode print_string lb;
      dots force_newline declaration decls;
      mcode print_string rb
  | Ast.TypeName(name) -> mcode print_string name; print_string " "
  | Ast.MetaType(name,_,_) ->
      mcode print_meta name; print_string " "

and baseType = function
    Ast.VoidType -> print_string "void "
  | Ast.CharType -> print_string "char "
  | Ast.ShortType -> print_string "short "
  | Ast.ShortIntType -> print_string "short int "
  | Ast.IntType -> print_string "int "
  | Ast.DoubleType -> print_string "double "
  | Ast.LongDoubleType -> print_string "long double "
  | Ast.FloatType -> print_string "float "
  | Ast.LongType -> print_string "long "
  | Ast.LongIntType -> print_string "long int "
  | Ast.LongLongType -> print_string "long long "
  | Ast.LongLongIntType -> print_string "long long int "
  | Ast.SizeType -> print_string "size_t "
  | Ast.SSizeType -> print_string "ssize_t "
  | Ast.PtrDiffType -> print_string "ptrdiff_t "

and structUnion = function
    Ast.Struct -> print_string "struct "
  | Ast.Union -> print_string "union "

and sign = function
    Ast.Signed -> print_string "signed "
  | Ast.Unsigned -> print_string "unsigned "

and const_vol = function
    Ast.Const -> print_string "const"
  | Ast.Volatile -> print_string "volatile"

(* --------------------------------------------------------------------- *)
(* Variable declaration *)
(* Even if the Cocci program specifies a list of declarations, they are
   split out into multiple declarations of a single variable each. *)

and print_named_type ty id =
  match Ast.unwrap ty with
    Ast.Type(_,None,ty1) ->
      (match Ast.unwrap ty1 with
	Ast.FunctionPointer(ty,lp1,star,rp1,lp2,params,rp2) ->
	  print_function_pointer (ty,lp1,star,rp1,lp2,params,rp2)
	    (function _ -> print_string " "; ident id)
      | Ast.FunctionType(_,ty,lp1,params,rp1) ->
	  print_function_type (ty,lp1,params,rp1)
	    (function _ -> print_string " "; ident id)
      | Ast.Array(ty,lb,size,rb) ->
	  let rec loop ty k =
	    match Ast.unwrap ty with
	      Ast.Array(ty,lb,size,rb) ->
		(match Ast.unwrap ty with
		  Ast.Type(_,cv,ty) ->
		    print_option
		      (function x -> mcode const_vol x; print_string " ")
		      cv;
		    loop ty
		      (function _ ->
			k ();
			mcode print_string lb;
			print_option expression size;
			mcode print_string rb)
		| _ -> failwith "complex array types not supported")
	    | _ -> typeC ty; ident id; k () in
	  loop ty1 (function _ -> ())
      | _ -> fullType ty; ident id)
  | _ -> fullType ty; ident id

and declaration d =
  match Ast.unwrap d with
    Ast.MetaDecl(name,_,_) | Ast.MetaField(name,_,_)
  | Ast.MetaFieldList(name,_,_,_) ->
      mcode print_meta name
  | Ast.AsDecl(decl,asdecl) -> declaration decl; print_string "@";
      declaration asdecl
  | Ast.Init(stg,ty,id,eq,ini,sem) ->
      print_option (mcode storage) stg; print_named_type ty id;
      print_string " "; mcode print_string eq;
      print_string " "; initialiser ini; mcode print_string sem
  | Ast.UnInit(stg,ty,id,sem) ->
      print_option (mcode storage) stg; print_named_type ty id;
      mcode print_string sem
  | Ast.MacroDecl(name,lp,args,rp,sem) ->
      ident name; mcode print_string_box lp;
      dots (function _ -> ()) expression args;
      close_box(); mcode print_string rp; mcode print_string sem
  | Ast.MacroDeclInit(name,lp,args,rp,eq,ini,sem) ->
      ident name; mcode print_string_box lp;
      dots (function _ -> ()) expression args;
      close_box(); mcode print_string rp;
      print_string " "; mcode print_string eq;
      print_string " "; initialiser ini; mcode print_string sem
  | Ast.TyDecl(ty,sem) -> fullType ty; mcode print_string sem
  | Ast.Typedef(stg,ty,id,sem) ->
      mcode print_string stg; print_string " "; fullType ty; typeC id;
      mcode print_string sem
  | Ast.DisjDecl(decls) -> print_disj_list declaration decls
  | Ast.Ddots(dots,Some whencode) ->
      mcode print_string dots; print_string "   when != "; declaration whencode
  | Ast.Ddots(dots,None) -> mcode print_string dots
  | Ast.OptDecl(decl) -> print_string "?"; declaration decl
  | Ast.UniqueDecl(decl) -> print_string "!"; declaration decl

(* --------------------------------------------------------------------- *)
(* Initialiser *)

and initialiser i =
  match Ast.unwrap i with
    Ast.MetaInit(name,_,_) ->
      mcode print_meta name; print_string " "
  | Ast.MetaInitList(name,_,_,_) ->
      mcode print_meta name; print_string " "
  | Ast.AsInit(ini,asini) -> initialiser ini; print_string "@";
      initialiser asini
  | Ast.InitExpr(exp) -> expression exp
  | Ast.ArInitList(lb,initlist,rb) ->
      mcode print_string lb; open_box 0;
      dots force_newline initialiser initlist; close_box();
      mcode print_string rb
  | Ast.StrInitList(allminus,lb,initlist,rb,whencode) ->
      mcode print_string lb; open_box 0;
      if not (whencode = [])
      then
	(print_string "   WHEN != ";
	 print_between (function _ -> print_string " v ")
	   initialiser whencode;
	 force_newline());
      List.iter initialiser initlist; close_box();
      mcode print_string rb
  | Ast.InitGccExt(designators,eq,ini) ->
      List.iter designator designators; print_string " ";
      mcode print_string eq; print_string " "; initialiser ini
  | Ast.InitGccName(name,eq,ini) ->
      ident name; mcode print_string eq; initialiser ini
  | Ast.IComma(comma) -> mcode print_string comma; force_newline()
  | Ast.Idots(dots,Some whencode) ->
      mcode print_string dots; print_string "   when != "; initialiser whencode
  | Ast.Idots(dots,None) -> mcode print_string dots
  | Ast.OptIni(ini) -> print_string "?"; initialiser ini
  | Ast.UniqueIni(ini) -> print_string "!"; initialiser ini

and designator = function
    Ast.DesignatorField(dot,id) -> mcode print_string dot; ident id
  | Ast.DesignatorIndex(lb,exp,rb) ->
      mcode print_string lb; expression exp; mcode print_string rb
  | Ast.DesignatorRange(lb,min,dots,max,rb) ->
      mcode print_string lb; expression min; mcode print_string dots;
      expression max; mcode print_string rb

(* --------------------------------------------------------------------- *)
(* Parameter *)

and parameterTypeDef p =
  match Ast.unwrap p with
    Ast.VoidParam(ty) -> fullType ty
  | Ast.Param(ty,Some id) -> print_named_type ty id
  | Ast.Param(ty,None) -> fullType ty
  | Ast.MetaParam(name,_,_) -> mcode print_meta name
  | Ast.MetaParamList(name,_,_,_) -> mcode print_meta name
  | Ast.PComma(cm) -> mcode print_string cm; print_space()
  | Ast.Pdots(dots) -> mcode print_string dots
  | Ast.Pcircles(dots) -> mcode print_string dots
  | Ast.OptParam(param) -> print_string "?"; parameterTypeDef param
  | Ast.UniqueParam(param) -> print_string "!"; parameterTypeDef param

and parameter_list l = dots (function _ -> ()) parameterTypeDef l

(* --------------------------------------------------------------------- *)
(* Top-level code *)

let rec rule_elem arity re =
  match Ast.unwrap re with
    Ast.FunHeader(bef,allminus,fninfo,name,lp,params,rp) ->
      mcode (function _ -> ()) ((),Ast.no_info,bef,[]);
      print_string arity; List.iter print_fninfo fninfo;
      ident name; mcode print_string_box lp;
      parameter_list params; close_box(); mcode print_string rp;
      print_string " "
  | Ast.Decl(bef,allminus,decl) ->
      mcode (function _ -> ()) ((),Ast.no_info,bef,[]);
      print_string arity;
      declaration decl
  | Ast.SeqStart(brace) ->
      print_string arity; mcode print_string brace;
      if !print_newlines_disj then start_block()
  | Ast.SeqEnd(brace) ->
      if !print_newlines_disj then end_block();
      print_string arity; mcode print_string brace
  | Ast.ExprStatement(exp,sem) ->
      print_string arity; print_option expression exp; mcode print_string sem
  | Ast.IfHeader(iff,lp,exp,rp) ->
      print_string arity;
      mcode print_string iff; print_string " "; mcode print_string_box lp;
      expression exp; close_box(); mcode print_string rp; print_string " "
  | Ast.Else(els) ->
      print_string arity; mcode print_string els; print_string " "
  | Ast.WhileHeader(whl,lp,exp,rp) ->
      print_string arity;
      mcode print_string whl; print_string " "; mcode print_string_box lp;
      expression exp; close_box(); mcode print_string rp; print_string " "
  | Ast.DoHeader(d) ->
      print_string arity; mcode print_string d; print_string " "
  | Ast.WhileTail(whl,lp,exp,rp,sem) ->
      print_string arity;
      mcode print_string whl; print_string " "; mcode print_string_box lp;
      expression exp; close_box(); mcode print_string rp;
      mcode print_string sem
  | Ast.ForHeader(fr,lp,first,e2,sem2,e3,rp) ->
      print_string arity;
      mcode print_string fr; mcode print_string_box lp; forinfo first;
      print_option expression e2; mcode print_string sem2;
      print_option expression e3; close_box();
      mcode print_string rp; print_string " "
  | Ast.IteratorHeader(nm,lp,args,rp) ->
      print_string arity;
      ident nm; print_string " "; mcode print_string_box lp;
      dots (function _ -> ()) expression args; close_box();
      mcode print_string rp; print_string " "
  | Ast.SwitchHeader(switch,lp,exp,rp) ->
      print_string arity;
      mcode print_string switch; print_string " "; mcode print_string_box lp;
      expression exp; close_box(); mcode print_string rp; print_string " "
  | Ast.Break(br,sem) ->
      print_string arity; mcode print_string br; mcode print_string sem
  | Ast.Continue(cont,sem) ->
      print_string arity; mcode print_string cont; mcode print_string sem
  | Ast.Label(l,dd) -> ident l; mcode print_string dd
  | Ast.Goto(goto,l,sem) ->
      mcode print_string goto; ident l; mcode print_string sem
  | Ast.Return(ret,sem) ->
      print_string arity; mcode print_string ret; mcode print_string sem
  | Ast.ReturnExpr(ret,exp,sem) ->
      print_string arity; mcode print_string ret; print_string " ";
      expression exp; mcode print_string sem
  | Ast.MetaRuleElem(name,_,_) ->
      print_string arity; mcode print_meta name
  | Ast.MetaStmt(name,_,_,_) ->
      print_string arity; mcode print_meta name
  | Ast.MetaStmtList(name,_,_) ->
      print_string arity;  mcode print_meta name
  | Ast.Exp(exp) -> print_string arity; expression exp
  | Ast.TopExp(exp) -> print_string arity; expression exp
  | Ast.Ty(ty) -> print_string arity; fullType ty
  | Ast.TopInit(init) -> initialiser init
  | Ast.Include(inc,s) ->
      mcode print_string inc; print_string " "; mcode inc_file s
  | Ast.Undef(def,id) ->
      mcode print_string def; print_string " "; ident id
  | Ast.DefineHeader(def,id,params) ->
      mcode print_string def; print_string " "; ident id;
      print_define_parameters params
  | Ast.Default(def,colon) ->
      mcode print_string def; mcode print_string colon; print_string " "
  | Ast.Case(case,exp,colon) ->
      mcode print_string case; print_string " "; expression exp;
      mcode print_string colon; print_string " "
  | Ast.DisjRuleElem(res) ->
      print_string arity;
      force_newline(); print_string "("; force_newline();
      print_between
	(function _ -> force_newline();print_string "|"; force_newline())
	(rule_elem arity)
	res;
      force_newline(); print_string ")"

and forinfo = function
    Ast.ForExp(e1,sem1) ->
      print_option expression e1; mcode print_string sem1
  | Ast.ForDecl (bef,allminus,decl) ->
      mcode (function _ -> ()) ((),Ast.no_info,bef,[]);
      declaration decl

and print_define_parameters params =
  match Ast.unwrap params with
    Ast.NoParams -> ()
  | Ast.DParams(lp,params,rp) ->
      mcode print_string lp;
      dots (function _ -> ()) print_define_param params; mcode print_string rp

and print_define_param param =
  match Ast.unwrap param with
    Ast.DParam(id) -> ident id
  | Ast.DPComma(comma) -> mcode print_string comma
  | Ast.DPdots(dots) -> mcode print_string dots
  | Ast.DPcircles(circles) -> mcode print_string circles
  | Ast.OptDParam(dp) -> print_string "?"; print_define_param dp
  | Ast.UniqueDParam(dp) -> print_string "!"; print_define_param dp

and statement arity s =
  match Ast.unwrap s with
    Ast.Seq(lbrace,body,rbrace) ->
      rule_elem arity lbrace;
      dots force_newline (statement arity) body;
      rule_elem arity rbrace
  | Ast.IfThen(header,branch,(_,_,_,aft)) ->
      rule_elem arity header; statement arity branch;
      mcode (function _ -> ()) ((),Ast.no_info,aft,[])
  | Ast.IfThenElse(header,branch1,els,branch2,(_,_,_,aft)) ->
      rule_elem arity header; statement arity branch1; print_string " ";
      rule_elem arity els; statement arity branch2;
      mcode (function _ -> ()) ((),Ast.no_info,aft,[])
  | Ast.While(header,body,(_,_,_,aft)) ->
      rule_elem arity header; statement arity body;
      mcode (function _ -> ()) ((),Ast.no_info,aft,[])
  | Ast.Do(header,body,tail) ->
      rule_elem arity header; statement arity body;
      rule_elem arity tail
  | Ast.For(header,body,(_,_,_,aft)) ->
      rule_elem arity header; statement arity body;
      mcode (function _ -> ()) ((),Ast.no_info,aft,[])
  | Ast.Iterator(header,body,(_,_,_,aft)) ->
      rule_elem arity header; statement arity body;
      mcode (function _ -> ()) ((),Ast.no_info,aft,[])
  | Ast.Switch(header,lb,decls,cases,rb) ->
      rule_elem arity header; rule_elem arity lb;
      dots force_newline (statement arity) decls;
      List.iter (function x -> case_line arity x; force_newline()) cases;
      rule_elem arity rb
  | Ast.Atomic(re) -> rule_elem arity re
  | Ast.FunDecl(header,lbrace,body,rbrace) ->
      rule_elem arity header; rule_elem arity lbrace;
      dots force_newline (statement arity) body;
      rule_elem arity rbrace
  | Ast.Disj([stmt_dots]) ->
      print_string arity;
      dots (function _ -> if !print_newlines_disj then force_newline())
	(statement arity) stmt_dots
  | Ast.Disj(stmt_dots_list) -> (* ignores newline directive for readability *)
      print_string arity;
      force_newline(); print_string "("; force_newline();
      print_between
	(function _ -> force_newline();print_string "|"; force_newline())
	(dots force_newline (statement arity))
	stmt_dots_list;
      force_newline(); print_string ")"
  | Ast.Define(header,body) ->
      rule_elem arity header; print_string " ";
      dots force_newline (statement arity) body
  | Ast.AsStmt(stm,asstm) ->
      statement arity stm; print_string "@"; statement arity asstm
  | Ast.Nest(starter,stmt_dots,ender,whn,multi,_,_) ->
      print_string arity;
      nest_dots starter ender (statement arity)
	(function _ ->
	  open_box 0;
	  print_between force_newline
	    (whencode (dots force_newline (statement "")) (statement "")) whn;
	  close_box(); force_newline())
	stmt_dots
  | Ast.Dots(d,whn,_,_) | Ast.Circles(d,whn,_,_) | Ast.Stars(d,whn,_,_) ->
      print_string arity; mcode print_string d;
      open_box 0;
      print_between force_newline
	(whencode (dots force_newline (statement "")) (statement "")) whn;
      close_box(); force_newline()
  | Ast.OptStm(s) -> statement "?" s
  | Ast.UniqueStm(s) -> statement "!" s

and print_statement_when whencode =
  print_string "   WHEN != ";
  open_box 0;
  print_between (function _ -> print_string " &"; force_newline())
    (dots force_newline (statement "")) whencode;
  close_box()


and whencode notfn alwaysfn = function
    Ast.WhenNot a ->
      print_string "   WHEN != "; open_box 0; notfn a; close_box()
  | Ast.WhenAlways a ->
      print_string "   WHEN = "; open_box 0; alwaysfn a; close_box()
  | Ast.WhenModifier x -> print_string "   WHEN "; print_when_modif x
  | Ast.WhenNotTrue a ->
      print_string "   WHEN != TRUE "; open_box 0; rule_elem "" a; close_box()
  | Ast.WhenNotFalse a ->
      print_string "   WHEN != FALSE "; open_box 0; rule_elem "" a; close_box()

and print_when_modif = function
  | Ast.WhenAny    -> print_string "ANY"
  | Ast.WhenStrict -> print_string "STRICT"
  | Ast.WhenForall -> print_string "FORALL"
  | Ast.WhenExists -> print_string "EXISTS"

and case_line arity c =
  match Ast.unwrap c with
    Ast.CaseLine(header,code) ->
      rule_elem arity header; print_string " ";
      dots force_newline (statement arity) code
  | Ast.OptCase(case) -> case_line "?" case

(* --------------------------------------------------------------------- *)
(* CPP code *)

and inc_file = function
    Ast.Local(elems) ->
      print_string "\"";
      print_between (function _ -> print_string "/") inc_elem elems;
      print_string "\""
  | Ast.NonLocal(elems) ->
      print_string "<";
      print_between (function _ -> print_string "/") inc_elem elems;
      print_string ">"

and inc_elem = function
    Ast.IncPath s -> print_string s
  | Ast.IncDots -> print_string "..."

(* for export only *)
let statement_dots l = dots force_newline (statement "") l

let top_level t =
  match Ast.unwrap t with
    Ast.FILEINFO(old_file,new_file) ->
      print_string "--- "; mcode print_string old_file; force_newline();
      print_string "+++ "; mcode print_string new_file
  | Ast.NONDECL(stmt) -> statement "" stmt
  | Ast.CODE(stmt_dots) ->
      dots force_newline (statement "") stmt_dots
  | Ast.ERRORWORDS(exps) ->
      print_string "error words = [";
      print_between (function _ -> print_string ", ") expression exps;
      print_string "]"

let rule =
  print_between (function _ -> force_newline(); force_newline()) top_level

let pp_print_anything x = !anything x

let _ =
  anything := function
      Ast.FullTypeTag(x) -> fullType x
    | Ast.BaseTypeTag(x) -> baseType x
    | Ast.StructUnionTag(x) -> structUnion x
    | Ast.SignTag(x) -> sign x
    | Ast.IdentTag(x) -> ident x
    | Ast.ExpressionTag(x) -> expression x
    | Ast.ConstantTag(x) -> constant x
    | Ast.UnaryOpTag(x) -> unaryOp x
    | Ast.AssignOpTag(x) -> assignOp x
    | Ast.FixOpTag(x) -> fixOp x
    | Ast.BinaryOpTag(x) -> binaryOp x
    | Ast.ArithOpTag(x) -> arithOp x
    | Ast.LogicalOpTag(x) -> logicalOp x
    | Ast.InitTag(x) -> initialiser x
    | Ast.DeclarationTag(x) -> declaration x
    | Ast.StorageTag(x) -> storage x
    | Ast.IncFileTag(x) -> inc_file x
    | Ast.Rule_elemTag(x) -> rule_elem "" x
    | Ast.StatementTag(x) -> statement "" x
    | Ast.ForInfoTag(x) -> forinfo x
    | Ast.CaseLineTag(x) -> case_line "" x
    | Ast.ConstVolTag(x) -> const_vol x
    | Ast.Token(x,Some info) -> print_string_befaft print_string x info
    | Ast.Token(x,None) -> print_string x
    | Ast.Pragma(xs) ->
	let print = function
	    Ast.Noindent s | Ast.Indent s | Ast.Space s -> print_string s in
	print_between force_newline print xs
    | Ast.Code(x) -> let _ = top_level x in ()
    | Ast.ExprDotsTag(x) -> dots (function _ -> ()) expression x
    | Ast.ParamDotsTag(x) -> parameter_list x
    | Ast.StmtDotsTag(x) -> dots (function _ -> ()) (statement "") x
    | Ast.DeclDotsTag(x) -> dots (function _ -> ()) declaration x
    | Ast.TypeCTag(x) -> typeC x
    | Ast.ParamTag(x) -> parameterTypeDef x
    | Ast.SgrepStartTag(x) -> print_string x
    | Ast.SgrepEndTag(x) -> print_string x

let rec dep in_and = function
    Ast.Dep(s) -> print_string s
  | Ast.AntiDep(s) -> print_string "!"; print_string s
  | Ast.EverDep(s) -> print_string "ever "; print_string s
  | Ast.NeverDep(s) -> print_string "never "; print_string s
  | Ast.AndDep(s1,s2) ->
      let print_and _ = dep true s1; print_string " && "; dep true s2 in
      if in_and
      then print_and ()
      else (print_string "("; print_and(); print_string ")")
  | Ast.OrDep(s1,s2) ->
      let print_or _ = dep false s1; print_string " || "; dep false s2 in
      if not in_and
      then print_or ()
      else (print_string "("; print_or(); print_string ")")
  | Ast.NoDep   -> print_string "no_dep"
  | Ast.FailDep -> print_string "fail_dep"

let script_header str lang deps code =
  print_string "@@";
  force_newline();
  print_string (str ^ ":" ^ lang);
  (match deps with
    Ast.NoDep -> ()
  | _ -> print_string " depends on "; dep true deps);
  force_newline();
  print_string "@@";
  force_newline();
  let code =
    String.concat "\n"
      (Str.split (Str.regexp "[\n\r\011\012]#.*[\n\r\011\012]") code) in
  print_string code;
  force_newline()

let unparse z =
  match z with
    Ast.InitialScriptRule (name,lang,deps,code) ->
      script_header "initialize" lang deps code
  | Ast.FinalScriptRule (name,lang,deps,code) ->
      script_header "finalize" lang deps code
  | Ast.ScriptRule (name,lang,deps,bindings,script_vars,code) ->
      script_header "script" lang deps code
  | Ast.CocciRule (nm, (deps, drops, exists), x, _, _) ->
      print_string "@@";
      force_newline();
      print_string nm;
      (match deps with
	Ast.NoDep -> ()
      | _ -> print_string " depends on "; dep true deps);
    (*
    print_string "line ";
    print_int (Ast.get_line (List.hd x));
    *)
      force_newline();
      print_string "@@";
      print_newlines_disj := true;
      force_newline();
      force_newline();
      rule x;
      force_newline()

let rule_elem_to_string x =
  print_newlines_disj := true;
  Common.format_to_string (function _ -> rule_elem "" x)

let ident_to_string x =
  print_newlines_disj := true;
  Common.format_to_string (function _ -> ident x)

let unparse_to_string x =
  print_newlines_disj := true;
  Common.format_to_string (function _ -> unparse x)

let print_rule_elem re =
  let nl = !print_newlines_disj in
  print_newlines_disj := false;
  rule_elem "" re;
  print_newlines_disj := nl

