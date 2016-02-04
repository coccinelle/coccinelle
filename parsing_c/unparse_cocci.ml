(*
 * Copyright (C) 2010, University of Copenhagen DIKU and INRIA.
 * Copyright (C) 2006, 2007 Julia Lawall
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *
 * This file was part of Coccinelle.
 *)
open Common

(*****************************************************************************)
(* mostly a copy paste of parsing_cocci/pretty_print_cocci.ml
 * todo?: try to factorize ?
 *)
(*****************************************************************************)

module Ast = Ast_cocci

let term s = Ast.unwrap_mcode s

(* or perhaps can have in plus, for instance a Disj, but those Disj must be
 *  handled by interactive tool (by proposing alternatives)
 *)
exception CantBeInPlus

(*****************************************************************************)

type pos = Before | After | InPlace
type nlhint = StartBox | EndBox | SpaceOrNewline of string ref

let get_string_info = function
    Ast.Noindent s | Ast.Indent s | Ast.Space s -> s

let unknown = -1

let rec do_all
    (env, pr, pr_celem, pr_cspace, pr_space, pr_arity, pr_barrier,
     indent, unindent, eatspace)
    generating xxs before =

(* Just to be able to copy paste the code from pretty_print_cocci.ml. *)
let print_string s line lcol =
  let rcol = if lcol = unknown then unknown else lcol + (String.length s) in
  pr s line lcol rcol None in
let print_string_with_hint hint s line lcol =
  let rcol = if lcol = unknown then unknown else lcol + (String.length s) in
  pr s line lcol rcol (Some hint) in
let print_text s = pr s unknown unknown unknown None in
let close_box _ = () in
let force_newline _ = print_text "\n" in

let start_block () = force_newline(); indent() in
let end_block () = unindent true; force_newline () in
let print_string_box s = print_string s in

let print_option = Common.do_option in
let print_option_space fn = function
    None -> ()
  | Some x -> fn x; pr_space() in
let print_option_prespace fn = function
    None -> ()
  | Some x -> pr_space(); fn x in
let print_between = Common.print_between in

let outdent _ = () (* should go to leftmost col, does nothing now *) in

let pretty_print_c =
  Pretty_print_c.mk_pretty_printers pr_celem pr_cspace
    force_newline indent outdent (function _ -> unindent true) in

(* --------------------------------------------------------------------- *)
(* Only for make_hrule, print plus code, unbound metavariables *)

(* avoid polyvariance problems *)
let anything : (Ast.anything -> unit) ref = ref (function _ -> ()) in

let rec print_anything = function
    [] -> ()
  | stream ->
      start_block();
      print_between force_newline print_anything_list stream;
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
      if space then pr_space ();
      print_anything_list rest in

let print_around printer term = function
    Ast.NOTHING -> printer term
  | Ast.BEFORE(bef,_) -> print_anything bef; printer term
  | Ast.AFTER(aft,_) -> printer term; print_anything aft
  | Ast.BEFOREAFTER(bef,aft,_) ->
      print_anything bef; printer term; print_anything aft in

let print_string_befaft fn fn1 x info =
  let print ln col s = print_string (get_string_info s) ln col in
  List.iter
    (function (s,ln,col) -> fn1(); print ln col s; force_newline())
    info.Ast.strbef;
  fn x;
  List.iter
    (function (s,ln,col) -> force_newline(); fn1(); print ln col s)
    info.Ast.straft in
let print_meta (r,x) = print_text x in

let print_pos l =
  List.iter
    (function
	Ast.MetaPos(name,_,_,_,_) ->
	  let name = Ast.unwrap_mcode name in
	  print_text "@"; print_meta name)
    l in

(* --------------------------------------------------------------------- *)

let mcode fn (s,info,mc,pos) =
  let line = info.Ast.line in
  let lcol = info.Ast.column in
  match (generating,mc) with
    (false,_) ->
    (* printing for transformation *)
    (* Here we don't care about the annotation on s. *)
      let print_comments lb comments =
	List.fold_left
	  (function line_before ->
	    function (str,line,col) ->
	      match line_before with
		None ->
		  let str =
		    match str with
		      Ast.Noindent s -> unindent false; s
		    | Ast.Indent s -> s
		    | Ast.Space s -> s in
		  print_string str line col; Some line
	      |	Some lb when line =|= lb ->
		  print_string (get_string_info str) line col; Some line
	      |	_ ->
		  force_newline();
		  (* not super elegant to put side-effecting unindent in a let
		     expression... *)
		  let str =
		    match str with
		      Ast.Noindent s -> unindent false; s
		    | Ast.Indent s -> s
		    | Ast.Space s -> s in
		  print_string str line col; Some line)
	  lb comments in
      let line_before = print_comments None info.Ast.strbef in
      (match line_before with
	None -> ()
      |	Some lb when lb =|= info.Ast.line -> ()
      |	_ -> force_newline());
      fn s line lcol;
      let _ = print_comments (Some info.Ast.line) info.Ast.straft in
      (* newline after a pragma
	 should really store parsed versions of the strings, but make a cheap
	 effort here
         print_comments takes care of interior newlines *)
      ()
      (* printing for rule generation *)
  | (true, Ast.MINUS(_,_,_,plus_stream)) ->
      force_newline();
      print_text "- ";
      fn s line lcol; print_pos pos;
      (match plus_stream with
	Ast.NOREPLACEMENT -> ()
      |	Ast.REPLACEMENT(plus_stream,ct) -> print_anything plus_stream)
  | (true, Ast.CONTEXT(_,plus_streams)) ->
      let fn s = force_newline(); fn s line lcol; print_pos pos in
      print_around fn s plus_streams
  | (true,Ast.PLUS Ast.ONE) ->
      let fn s =
	force_newline(); print_text "+ "; fn s line lcol; print_pos pos in
      print_string_befaft fn (function _ -> print_text "+ ") s info
  | (true,Ast.PLUS Ast.MANY) ->
      let fn s =
 	force_newline(); print_text "++ "; fn s line lcol; print_pos pos in
      print_string_befaft fn (function _ -> print_text "++ ") s info
in


(* --------------------------------------------------------------------- *)

let handle_metavar name fn =
  let ((_,b) as s,info,mc,pos) = name in
  let line = info.Ast.line in
  let lcol = info.Ast.column in
  match Common.optionise (fun () -> List.assoc s env) with
    None ->
      let name_string (_,s) = s in
      if generating
      then
	mcode (function _ -> print_string (name_string s)) name
      else
	failwith
	  (Printf.sprintf "SP line %d: Not found a value in env for: %s"
	     line (name_string s))
  | Some e  ->
      pr_barrier line lcol;
      (if generating
      then
	(* call mcode to preserve the -+ annotation *)
	mcode (fun _ _ _ -> fn e) name
      else fn e);
      let rcol =
	if lcol = unknown then unknown else lcol + (String.length b) in
      pr_barrier line rcol
in
(* --------------------------------------------------------------------- *)
let dots between fn d =
  match Ast.unwrap d with
    Ast.DOTS(l) -> print_between between fn l
  | Ast.CIRCLES(l) -> print_between between fn l
  | Ast.STARS(l) -> print_between between fn l
in

let nest_dots starter ender fn f d =
  mcode print_string starter;
  f(); start_block();
  (match Ast.unwrap d with
    Ast.DOTS(l)    -> print_between force_newline fn l
  | Ast.CIRCLES(l) -> print_between force_newline fn l
  | Ast.STARS(l)   -> print_between force_newline fn l);
  end_block();
  mcode print_string ender
in

let print_disj_list fn l =
  print_text "\n(\n";
  print_between (function _ -> print_text "\n|\n") fn l;
  print_text "\n)\n" in

(* --------------------------------------------------------------------- *)
(* Identifier *)

let rec ident i =
  match Ast.unwrap i with
      Ast.Id(name) -> mcode print_string name
    | Ast.MetaId(name,_,_,_) ->
	handle_metavar name (function
			       | (Ast_c.MetaIdVal (id,_)) -> print_text id
			       | _ -> raise Impossible
			    )
    | Ast.MetaFunc(name,_,_,_) ->
	handle_metavar name (function
			       | (Ast_c.MetaFuncVal id) -> print_text id
			       | _ -> raise Impossible
			    )
    | Ast.MetaLocalFunc(name,_,_,_) ->
	handle_metavar name (function
			       | (Ast_c.MetaLocalFuncVal id) -> print_text id
			       | _ -> raise Impossible
			    )

    | Ast.AsIdent(id,asid) -> ident id

    | Ast.DisjId(id_list) ->
	if generating
	then print_disj_list ident id_list
	else raise CantBeInPlus
    | Ast.OptIdent(_) | Ast.UniqueIdent(_) ->
	raise CantBeInPlus

in

(* --------------------------------------------------------------------- *)
(* Expression *)

let rec expression e =
  match Ast.unwrap e with
    Ast.Ident(id) -> ident id
  | Ast.Constant(const) -> mcode constant const
  | Ast.FunCall(fn,lp,args,rp) ->
      expression fn; mcode (print_string_with_hint StartBox) lp;
      dots (function _ -> ()) arg_expression args;
      mcode (print_string_with_hint EndBox) rp
  | Ast.Assignment(left,op,right,_) ->
      expression left; pr_space(); mcode assignOp op;
      pr_space(); expression right
  | Ast.Sequence(left,op,right) ->
      expression left; mcode print_string op;
      pr_space(); expression right
  | Ast.CondExpr(exp1,why,exp2,colon,exp3) ->
      expression exp1; pr_space(); mcode print_string why;
      print_option (function e -> pr_space(); expression e) exp2;
      pr_space(); mcode print_string colon; pr_space(); expression exp3
  | Ast.Postfix(exp,op) -> expression exp; mcode fixOp op
  | Ast.Infix(exp,op) -> mcode fixOp op; expression exp
  | Ast.Unary(exp,op) -> mcode unaryOp op; expression exp
  | Ast.Binary(left,op,right) ->
      expression left; pr_space(); mcode binaryOp op; pr_space();
      expression right
  | Ast.Nested(left,op,right) -> failwith "nested only in minus code"
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
      mcode print_string rp; initialiser true init

  | Ast.MetaErr(name,_,_,_) ->
      failwith "metaErr not handled"

  | Ast.MetaExpr (name,_,_,_typedontcare,_formdontcare,_) ->
      handle_metavar name  (function
        | Ast_c.MetaExprVal (exp,_) ->
            pretty_print_c.Pretty_print_c.expression exp
        | _ -> raise Impossible
      )

  | Ast.MetaExprList (name,_,_,_) ->
      handle_metavar name  (function
        | Ast_c.MetaExprListVal args ->
            pretty_print_c.Pretty_print_c.arg_list args
	| Ast_c.MetaParamListVal _ ->
	    failwith "have meta param list matching meta exp list\n";
        | _ -> raise Impossible
      )

  | Ast.AsExpr(expr,asexpr) -> expression expr

  | Ast.EComma(cm) -> mcode print_string cm

  | Ast.DisjExpr(exp_list) ->
      if generating
      then print_disj_list expression exp_list
      else raise CantBeInPlus
  | Ast.NestExpr(starter,expr_dots,ender,Some whencode,multi)
    when generating ->
      nest_dots starter ender expression
	(function _ -> print_text "   when != "; expression whencode)
	expr_dots
  | Ast.NestExpr(starter,expr_dots,ender,None,multi) when generating ->
      nest_dots starter ender expression (function _ -> ()) expr_dots
  | Ast.NestExpr _ -> raise CantBeInPlus
  | Ast.Edots(dots,Some whencode)
  | Ast.Ecircles(dots,Some whencode)
  | Ast.Estars(dots,Some whencode) ->
      if generating
      then
	(mcode print_string dots;
	 print_text "   when != ";
	 expression whencode)
      else raise CantBeInPlus
  | Ast.Edots(dots,None)
  | Ast.Ecircles(dots,None)
  | Ast.Estars(dots,None) ->
      if generating
      then mcode print_string dots
      else raise CantBeInPlus

  | Ast.OptExp(exp) | Ast.UniqueExp(exp) ->
      raise CantBeInPlus

and arg_expression e =
  match Ast.unwrap e with
    Ast.EComma(cm) ->
      (* space is only used by add_newline, and only if not using SMPL
	 spacing.  pr_cspace uses a " " in unparse_c.ml.  Not so nice... *)
      mcode (print_string_with_hint (SpaceOrNewline (ref " ")))  cm
  | _ -> expression e

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
  | Ast.OpAssign(aop) ->
      (function line -> function lcol ->
	arithOp aop line lcol; print_string "=" line lcol)

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
    Ast.String(s) -> print_string ("\""^s^"\"")
  | Ast.Char(s) -> print_string ("\'"^s^"\'")
  | Ast.Int(s) -> print_string s
  | Ast.Float(s) -> print_string s

(* --------------------------------------------------------------------- *)
(* Types *)


and fullType ft =
  match Ast.unwrap ft with
    Ast.Type(_,cv,ty) ->
      (match Ast.unwrap ty with
	Ast.Pointer(_,_) ->
	  typeC ty; print_option_prespace (mcode const_vol) cv
      |	_ -> print_option_space (mcode const_vol) cv; typeC ty)
      
  | Ast.AsType(ty, asty) -> fullType ty
  | Ast.DisjType _ -> failwith "can't be in plus"
  | Ast.OptType(_) | Ast.UniqueType(_) ->
      raise CantBeInPlus

and print_function_pointer (ty,lp1,star,rp1,lp2,params,rp2) fn =
  fullType ty; mcode print_string lp1; mcode print_string star; fn();
  mcode print_string rp1; mcode print_string lp1;
  parameter_list params; mcode print_string rp2

and print_function_type (ty,lp1,params,rp1) fn =
  print_option fullType ty; fn(); mcode print_string lp1;
  parameter_list params; mcode print_string rp1

and typeC ty =
  match Ast.unwrap ty with
    Ast.BaseType(ty,strings) ->
      print_between pr_space (mcode print_string) strings
  | Ast.SignedT(sgn,ty) -> mcode sign sgn; print_option_prespace typeC ty
  | Ast.Pointer(ty,star) ->
      fullType ty; ft_space ty; mcode print_string star; eatspace()
  | Ast.FunctionPointer(ty,lp1,star,rp1,lp2,params,rp2) ->
      print_function_pointer (ty,lp1,star,rp1,lp2,params,rp2)
	(function _ -> ())
  | Ast.FunctionType (am,ty,lp1,params,rp1) ->
      print_function_type (ty,lp1,params,rp1) (function _ -> ())
  | Ast.Array(ty,lb,size,rb) ->
      fullType ty; mcode print_string lb; print_option expression size;
      mcode print_string rb
  | Ast.EnumName(kind,name) ->
      mcode print_string kind;
      print_option_prespace ident name
  | Ast.EnumDef(ty,lb,ids,rb) ->
      fullType ty; ft_space ty;
      mcode print_string lb;
      dots force_newline expression ids;
      mcode print_string rb
  | Ast.StructUnionName(kind,name) ->
      mcode structUnion kind; print_option_prespace ident name
  | Ast.StructUnionDef(ty,lb,decls,rb) ->
      fullType ty; ft_space ty;
      mcode print_string lb;
      dots force_newline declaration decls;
      mcode print_string rb
  | Ast.TypeName(name)-> mcode print_string name
  | Ast.MetaType(name,_,_) ->
      handle_metavar name  (function
          Ast_c.MetaTypeVal exp ->
            pretty_print_c.Pretty_print_c.ty exp
        | _ -> raise Impossible)

and baseType = function
    Ast.VoidType -> print_string "void"
  | Ast.CharType -> print_string "char"
  | Ast.ShortType -> print_string "short"
  | Ast.ShortIntType -> print_string "short int"
  | Ast.IntType -> print_string "int"
  | Ast.DoubleType -> print_string "double"
  | Ast.LongDoubleType -> print_string "long double"
  | Ast.FloatType -> print_string "float"
  | Ast.LongType -> print_string "long"
  | Ast.LongIntType -> print_string "long int"
  | Ast.LongLongType -> print_string "long long"
  | Ast.LongLongIntType -> print_string "long long int"
  | Ast.SizeType -> print_string "size_t "
  | Ast.SSizeType -> print_string "ssize_t "
  | Ast.PtrDiffType -> print_string "ptrdiff_t "

and structUnion = function
    Ast.Struct -> print_string "struct"
  | Ast.Union -> print_string "union"

and sign = function
    Ast.Signed -> print_string "signed"
  | Ast.Unsigned -> print_string "unsigned"


and const_vol = function
    Ast.Const -> print_string "const"
  | Ast.Volatile -> print_string "volatile"

(* --------------------------------------------------------------------- *)
(* Function declaration *)

and storage = function
    Ast.Static -> print_string "static"
  | Ast.Auto -> print_string "auto"
  | Ast.Register -> print_string "register"
  | Ast.Extern -> print_string "extern"

(* --------------------------------------------------------------------- *)
(* Variable declaration *)

and print_named_type ty id =
  match Ast.unwrap ty with
    Ast.Type(_,None,ty1) ->
      (match Ast.unwrap ty1 with
	Ast.FunctionPointer(ty,lp1,star,rp1,lp2,params,rp2) ->
	  print_function_pointer (ty,lp1,star,rp1,lp2,params,rp2)
	    (function _ -> pr_space(); ident id)
      | Ast.FunctionType(am,ty,lp1,params,rp1) ->
	  print_function_type (ty,lp1,params,rp1)
	    (function _ -> pr_space(); ident id)
      | Ast.Array(_,_,_,_) ->
	  let rec loop ty k =
	    match Ast.unwrap ty with
	      Ast.Array(ty,lb,size,rb) ->
		(match Ast.unwrap ty with
		  Ast.Type(_,cv,ty) ->
		    print_option_space (mcode const_vol) cv;
		    loop ty
		      (function _ ->
			k ();
			mcode print_string lb;
			print_option expression size;
			mcode print_string rb)
		| _ -> failwith "complex array types not supported")
	    | _ -> typeC ty; ty_space ty; ident id; k () in
	  loop ty1 (function _ -> ())
    (*| should have a case here for pointer to array or function type
        that would put ( * ) around the variable.  This makes one wonder
        why we really need a special case for function pointer *)
      | _ -> fullType ty; ft_space ty; ident id)
  | _ -> fullType ty; ft_space ty; ident id

and ty_space ty =
  match Ast.unwrap ty with
    Ast.Pointer(_,_) -> ()
  | _ -> pr_space()

and ft_space ty =
  match Ast.unwrap ty with
    Ast.Type(_,cv,ty) ->
      (match Ast.unwrap ty with
	Ast.Pointer(_,_) -> ()
      | Ast.MetaType(name,_,_) ->
	  (match List.assoc (Ast.unwrap_mcode name) env with
            Ast_c.MetaTypeVal (tq,ty) ->
	      (match Ast_c.unwrap ty with
		Ast_c.Pointer(_,_) -> ()
	      |	_ -> pr_space())
	  | _ -> pr_space())
      | _ -> pr_space())
  | _ -> pr_space()

and declaration d =
  match Ast.unwrap d with
    Ast.MetaDecl(name,_,_) ->
      handle_metavar name
	(function
	    Ast_c.MetaDeclVal d ->
              pretty_print_c.Pretty_print_c.decl d
          | _ -> raise Impossible)
  | Ast.MetaField(name,_,_) ->
      handle_metavar name
	(function
	    Ast_c.MetaFieldVal f ->
              pretty_print_c.Pretty_print_c.field f
          | _ -> raise Impossible)

  | Ast.MetaFieldList(name,_,_,_) ->
      handle_metavar name
	(function
	    Ast_c.MetaFieldListVal f ->
	      print_between force_newline pretty_print_c.Pretty_print_c.field f
          | _ -> raise Impossible)

  | Ast.AsDecl(decl,asdecl) -> declaration decl

  | Ast.Init(stg,ty,id,eq,ini,sem) ->
      print_option (mcode storage) stg;
      print_option (function _ -> pr_space()) stg;
      print_named_type ty id;
      pr_space(); mcode print_string eq;
      pr_space(); initialiser true ini; mcode print_string sem
  | Ast.UnInit(stg,ty,id,sem) ->
      print_option (mcode storage) stg;
      print_option (function _ -> pr_space()) stg;
      print_named_type ty id;
      mcode print_string sem
  | Ast.MacroDecl(name,lp,args,rp,sem) ->
      ident name; mcode print_string_box lp;
      dots (function _ -> ()) expression args;
      close_box(); mcode print_string rp; mcode print_string sem
  | Ast.MacroDeclInit(name,lp,args,rp,eq,ini,sem) ->
      ident name; mcode print_string_box lp;
      dots (function _ -> ()) expression args;
      close_box(); mcode print_string rp;
      pr_space(); mcode print_string eq;
      pr_space(); initialiser true ini; mcode print_string sem
  | Ast.TyDecl(ty,sem) -> fullType ty; mcode print_string sem
  | Ast.Typedef(stg,ty,id,sem) ->
      mcode print_string stg;
      fullType ty; typeC id;
      mcode print_string sem
  | Ast.DisjDecl(_) -> raise CantBeInPlus
  | Ast.Ddots(_,_) -> raise CantBeInPlus
  | Ast.OptDecl(decl)  | Ast.UniqueDecl(decl) ->
      raise CantBeInPlus

(* --------------------------------------------------------------------- *)
(* Initialiser *)

and initialiser nlcomma i =
  match Ast.unwrap i with
    Ast.MetaInit(name,_,_) ->
      handle_metavar name  (function
          Ast_c.MetaInitVal ini ->
            pretty_print_c.Pretty_print_c.init ini
        | _ -> raise Impossible)
  | Ast.MetaInitList(name,_,_,_) ->
      handle_metavar name  (function
          Ast_c.MetaInitListVal ini ->
	    pretty_print_c.Pretty_print_c.init_list ini
        | _ -> raise Impossible)
  | Ast.AsInit(init,asinit) -> initialiser nlcomma init
  | Ast.InitExpr(exp) -> expression exp
  | Ast.ArInitList(lb,initlist,rb) ->
      (match Ast.undots initlist with
	[] -> mcode print_string lb; mcode print_string rb
      |	lst ->
	  mcode print_string lb; start_block();
	  initialiser_list nlcomma lst;
	  end_block(); mcode print_string rb)
  | Ast.StrInitList(_,lb,[],rb,[]) ->
      mcode print_string lb; mcode print_string rb
  | Ast.StrInitList(_,lb,initlist,rb,[]) ->
      mcode print_string lb; start_block();
      initialiser_list nlcomma initlist;
      end_block(); mcode print_string rb
  | Ast.StrInitList(_,lb,initlist,rb,_) ->
      failwith "unexpected whencode in plus"
  | Ast.InitGccExt(designators,eq,ini) ->
      List.iter designator designators; pr_space();
      mcode print_string eq; pr_space(); initialiser nlcomma ini
  | Ast.InitGccName(name,eq,ini) ->
      ident name; mcode print_string eq; initialiser nlcomma ini
  | Ast.IComma(comma) ->
      mcode print_string comma;
      if nlcomma then force_newline() else pr_space()
  | Ast.Idots(dots,Some whencode) ->
      if generating
      then
	(mcode print_string dots;
	 print_text "   when != ";
	 initialiser nlcomma whencode)
      else raise CantBeInPlus
  | Ast.Idots(dots,None) ->
      if generating
      then mcode print_string dots
      else raise CantBeInPlus
  | Ast.OptIni(ini) | Ast.UniqueIni(ini) ->
      raise CantBeInPlus

and initialiser_list nlcomma = function
  (* awkward, because the comma is separate from the initialiser *)
    [] -> ()
  | [x] -> initialiser false x
  | x::xs -> initialiser nlcomma x; initialiser_list nlcomma xs

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

  | Ast.MetaParam(name,_,_) ->
      handle_metavar name
	(function
	    Ast_c.MetaParamVal p ->
              pretty_print_c.Pretty_print_c.param p
          | _ -> raise Impossible)
  | Ast.MetaParamList(name,_,_,_) ->
      handle_metavar name
	(function
	    Ast_c.MetaParamListVal p ->
              pretty_print_c.Pretty_print_c.paramlist p
          | _ -> raise Impossible)

  | Ast.PComma(cm) -> mcode print_string cm
  | Ast.Pdots(dots) | Ast.Pcircles(dots) when generating ->
      mcode print_string dots
  | Ast.Pdots(dots) | Ast.Pcircles(dots) -> raise CantBeInPlus
  | Ast.OptParam(param) | Ast.UniqueParam(param) -> raise CantBeInPlus

and parameter_list l =
  let comma p =
    parameterTypeDef p;
    match Ast.unwrap p with
      Ast.PComma(cm) -> pr_space()
    | _ -> () in
  dots (function _ -> ()) comma l
in


(* --------------------------------------------------------------------- *)
(* CPP code *)

let rec inc_file = function
    Ast.Local(elems) ->
      print_string ("\""^(String.concat "/" (List.map inc_elem elems))^"\"")
  | Ast.NonLocal(elems) ->
      print_string ("<"^(String.concat "/" (List.map inc_elem elems))^">")

and inc_elem = function
    Ast.IncPath s -> s
  | Ast.IncDots -> "..."

(* --------------------------------------------------------------------- *)
(* Top-level code *)

and rule_elem arity re =
  match Ast.unwrap re with
    Ast.FunHeader(_,_,fninfo,name,lp,params,rp) ->
      pr_arity arity; List.iter print_fninfo fninfo;
      ident name; mcode print_string_box lp;
      parameter_list params; close_box(); mcode print_string rp;
      pr_space()
  | Ast.Decl(_,_,decl) -> pr_arity arity; declaration decl

  | Ast.SeqStart(brace) ->
      pr_arity arity; mcode print_string brace; start_block()
  | Ast.SeqEnd(brace) ->
      end_block(); pr_arity arity; mcode print_string brace

  | Ast.ExprStatement(exp,sem) ->
      pr_arity arity; print_option expression exp; mcode print_string sem

  | Ast.IfHeader(iff,lp,exp,rp) ->
      pr_arity arity;
      mcode print_string iff; pr_space(); mcode print_string_box lp;
      expression exp; close_box(); mcode print_string rp
  | Ast.Else(els) ->
      pr_arity arity; mcode print_string els

  | Ast.WhileHeader(whl,lp,exp,rp) ->
      pr_arity arity;
      mcode print_string whl; pr_space(); mcode print_string_box lp;
      expression exp; close_box(); mcode print_string rp
  | Ast.DoHeader(d) ->
      pr_arity arity; mcode print_string d
  | Ast.WhileTail(whl,lp,exp,rp,sem) ->
      pr_arity arity;
      mcode print_string whl; pr_space(); mcode print_string_box lp;
      expression exp; close_box(); mcode print_string rp;
      mcode print_string sem
  | Ast.ForHeader(fr,lp,first,e2,sem2,e3,rp) ->
      pr_arity arity;
      mcode print_string fr; mcode print_string_box lp; forinfo first;
      print_option expression e2; mcode print_string sem2;
      print_option expression e3; close_box();
      mcode print_string rp
  | Ast.IteratorHeader(nm,lp,args,rp) ->
      pr_arity arity;
      ident nm; pr_space(); mcode print_string_box lp;
      dots (function _ -> ()) expression args; close_box();
      mcode print_string rp

  | Ast.SwitchHeader(switch,lp,exp,rp) ->
      pr_arity arity;
      mcode print_string switch; pr_space(); mcode print_string_box lp;
      expression exp; close_box(); mcode print_string rp

  | Ast.Break(br,sem) ->
      pr_arity arity; mcode print_string br; mcode print_string sem
  | Ast.Continue(cont,sem) ->
      pr_arity arity; mcode print_string cont; mcode print_string sem
  | Ast.Label(l,dd) -> ident l; mcode print_string dd
  | Ast.Goto(goto,l,sem) ->
      mcode print_string goto; ident l; mcode print_string sem
  | Ast.Return(ret,sem) ->
      pr_arity arity; mcode print_string ret;
      mcode print_string sem
  | Ast.ReturnExpr(ret,exp,sem) ->
      pr_arity arity; mcode print_string ret; pr_space();
      expression exp; mcode print_string sem

  | Ast.Exp(exp) -> pr_arity arity; expression exp
  | Ast.TopExp(exp) -> pr_arity arity; expression exp
  | Ast.Ty(ty) -> pr_arity arity; fullType ty
  | Ast.TopInit(init) -> initialiser false init
  | Ast.Include(inc,s) ->
      mcode print_string inc; print_text " "; mcode inc_file s
  | Ast.Undef(def,id) ->
      mcode print_string def; pr_space(); ident id
  | Ast.DefineHeader(def,id,params) ->
      mcode print_string def; pr_space(); ident id;
      print_define_parameters params
  | Ast.Default(def,colon) ->
      mcode print_string def; mcode print_string colon; pr_space()
  | Ast.Case(case,exp,colon) ->
      mcode print_string case; pr_space(); expression exp;
      mcode print_string colon; pr_space()
  | Ast.DisjRuleElem(res) ->
      if generating
      then
	(pr_arity arity; print_text "\n(\n";
	 print_between (function _ -> print_text "\n|\n") (rule_elem arity)
	   res;
	 print_text "\n)")
      else raise CantBeInPlus

  | Ast.MetaRuleElem(name,_,_) ->
      raise Impossible

  | Ast.MetaStmt(name,_,_,_) ->
      handle_metavar name  (function
        | Ast_c.MetaStmtVal stm ->
            pretty_print_c.Pretty_print_c.statement stm
        | _ -> raise Impossible
                           )
  | Ast.MetaStmtList(name,_,_) ->
      failwith
	"MetaStmtList not supported (not even in ast_c metavars binding)"

and forinfo = function
    Ast.ForExp(e1,sem1) ->
      print_option expression e1; mcode print_string sem1
  | Ast.ForDecl (_,_,decl) -> declaration decl

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
  | Ast.OptDParam(dp) -> print_text "?"; print_define_param dp
  | Ast.UniqueDParam(dp) -> print_text "!"; print_define_param dp

and print_fninfo = function
    Ast.FStorage(stg) -> mcode storage stg
  | Ast.FType(ty) -> fullType ty
  | Ast.FInline(inline) -> mcode print_string inline; pr_space()
  | Ast.FAttr(attr) -> mcode print_string attr; pr_space() in

let indent_if_needed s f =
  match Ast.unwrap s with
    Ast.Seq(lbrace,body,rbrace) -> pr_space(); f()
  | _ ->
      (*no newline at the end - someone else will do that*)
      start_block(); f(); unindent true in

let rec statement arity s =
  match Ast.unwrap s with
    Ast.Seq(lbrace,body,rbrace) ->
      rule_elem arity lbrace;
      dots force_newline (statement arity) body;
      rule_elem arity rbrace

  | Ast.IfThen(header,branch,_) ->
      rule_elem arity header;
      indent_if_needed branch (function _ -> statement arity branch)
  | Ast.IfThenElse(header,branch1,els,branch2,_) ->
      rule_elem arity header;
      indent_if_needed branch1 (function _ -> statement arity branch1);
      force_newline();
      rule_elem arity els;
      indent_if_needed branch2 (function _ -> statement arity branch2)
  | Ast.While(header,body,_) ->
      rule_elem arity header;
      indent_if_needed body (function _ -> statement arity body)
  | Ast.Do(header,body,tail) ->
      rule_elem arity header;
      indent_if_needed body (function _ -> statement arity body);
      rule_elem arity tail
  | Ast.For(header,body,_) ->
      rule_elem arity header;
      indent_if_needed body (function _ -> statement arity body)
  | Ast.Iterator(header,body,(_,_,_,aft)) ->
      rule_elem arity header;
      indent_if_needed body (function _ -> statement arity body);
      mcode (fun _ _ _ -> ()) ((),Ast.no_info,aft,[])

  | Ast.Switch(header,lb,decls,cases,rb) ->
      rule_elem arity header; pr_space(); rule_elem arity lb;
      dots force_newline (statement arity) decls;
      List.iter (function x -> case_line arity x; force_newline()) cases;
      rule_elem arity rb

  | Ast.Atomic(re) -> rule_elem arity re

  | Ast.FunDecl(header,lbrace,body,rbrace) ->
      rule_elem arity header; rule_elem arity lbrace;
      dots force_newline (statement arity) body; rule_elem arity rbrace

  | Ast.Define(header,body) ->
      rule_elem arity header; pr_space();
      dots force_newline (statement arity) body

  | Ast.AsStmt(stmt,asstmt) -> statement arity stmt

  | Ast.Disj([stmt_dots]) ->
      if generating
      then
	(pr_arity arity;
	 dots force_newline (statement arity) stmt_dots)
      else raise CantBeInPlus
  | Ast.Disj(stmt_dots_list) -> (* ignores newline directive for readability *)
      if generating
      then
	(pr_arity arity; print_text "\n(\n";
	 print_between (function _ -> print_text "\n|\n")
	   (dots force_newline (statement arity))
	   stmt_dots_list;
	 print_text "\n)")
      else raise CantBeInPlus
  | Ast.Nest(starter,stmt_dots,ender,whn,multi,_,_) when generating ->
      pr_arity arity;
      nest_dots starter ender (statement arity)
	(function _ ->
	  print_between force_newline
	    (whencode (dots force_newline (statement "")) (statement "")) whn;
	  force_newline())
	stmt_dots
  | Ast.Nest(_) -> raise CantBeInPlus
  | Ast.Dots(d,whn,_,_) | Ast.Circles(d,whn,_,_) | Ast.Stars(d,whn,_,_) ->
      if generating
      then
	(pr_arity arity; mcode print_string d;
	 print_between force_newline
	   (whencode (dots force_newline (statement "")) (statement "")) whn;
	 force_newline())
      else raise CantBeInPlus

  | Ast.OptStm(s) | Ast.UniqueStm(s) ->
      raise CantBeInPlus

and whencode notfn alwaysfn = function
    Ast.WhenNot a ->
      print_text "   WHEN != "; notfn a
  | Ast.WhenAlways a ->
      print_text "   WHEN = "; alwaysfn a
  | Ast.WhenModifier x -> print_text "   WHEN "; print_when_modif x
  | Ast.WhenNotTrue a ->
      print_text "   WHEN != TRUE "; rule_elem "" a
  | Ast.WhenNotFalse a ->
      print_text "   WHEN != FALSE "; rule_elem "" a

and print_when_modif = function
  | Ast.WhenAny    -> print_text "ANY"
  | Ast.WhenStrict -> print_text "STRICT"
  | Ast.WhenForall -> print_text "FORALL"
  | Ast.WhenExists -> print_text "EXISTS"

and case_line arity c =
  match Ast.unwrap c with
    Ast.CaseLine(header,code) ->
      rule_elem arity header; pr_space();
      dots force_newline (statement arity) code
  | Ast.OptCase(case) -> raise CantBeInPlus in

let top_level t =
  match Ast.unwrap t with
    Ast.FILEINFO(old_file,new_file) -> raise CantBeInPlus
  | Ast.NONDECL(stmt) -> statement "" stmt
  | Ast.CODE(stmt_dots) -> dots force_newline (statement "") stmt_dots
  | Ast.ERRORWORDS(exps) -> raise CantBeInPlus
in

(*
let rule =
  print_between (function _ -> force_newline(); force_newline()) top_level
in
*)

let if_open_brace  = function "{" -> true | _ -> false in

(* boolean result indicates whether an indent is needed *)
let rec pp_any = function
  (* assert: normally there is only CONTEXT NOTHING tokens in any *)
    Ast.FullTypeTag(x) -> fullType x; false
  | Ast.BaseTypeTag(x) -> baseType x unknown unknown; false
  | Ast.StructUnionTag(x) -> structUnion x unknown unknown; false
  | Ast.SignTag(x) -> sign x unknown unknown; false

  | Ast.IdentTag(x) -> ident x; false

  | Ast.ExpressionTag(x) -> expression x; false

  | Ast.ConstantTag(x) -> constant x unknown unknown; false
  | Ast.UnaryOpTag(x) -> unaryOp x unknown unknown; false
  | Ast.AssignOpTag(x) -> assignOp x unknown unknown; false
  | Ast.FixOpTag(x) -> fixOp x unknown unknown; false
  | Ast.BinaryOpTag(x) -> binaryOp x unknown unknown; false
  | Ast.ArithOpTag(x) -> arithOp x unknown unknown; false
  | Ast.LogicalOpTag(x) -> logicalOp x unknown unknown; false

  | Ast.InitTag(x) -> initialiser false x; false
  | Ast.DeclarationTag(x) -> declaration x; false

  | Ast.StorageTag(x) -> storage x unknown unknown; false
  | Ast.IncFileTag(x) -> inc_file x unknown unknown; false

  | Ast.Rule_elemTag(x) -> rule_elem "" x; false
  | Ast.StatementTag(x) -> statement "" x; false
  | Ast.ForInfoTag(x) -> forinfo x; false
  | Ast.CaseLineTag(x) -> case_line "" x; false

  | Ast.ConstVolTag(x) -> const_vol x unknown unknown; false
  | Ast.Pragma(xs) ->
      (match xs with (Ast.Space s)::_ -> pr_space() | _ -> ());
      let rec loop = function
	  [] -> ()
	| [Ast.Noindent s] -> unindent false; print_text s
	| [Ast.Indent s] -> print_text s
	| (Ast.Space s) :: (((Ast.Indent _ | Ast.Noindent _) :: _) as rest) ->
	    print_text s; force_newline(); loop rest
	| (Ast.Space s) :: rest -> print_text s; pr_space(); loop rest
	| Ast.Noindent s :: rest ->
	    unindent false; print_text s; force_newline(); loop rest
	| Ast.Indent s :: rest ->
	    print_text s; force_newline(); loop rest in
      loop xs; false
  | Ast.Token(x,None) -> print_text x; if_open_brace x
  | Ast.Token(x,Some info) ->
      mcode
	(fun x line lcol ->
	  (match x with
	    "else" -> force_newline()
	  | _ -> ());
	  print_string x line lcol)
	(let nomcodekind = Ast.CONTEXT(Ast.DontCarePos,Ast.NOTHING) in
	(x,info,nomcodekind,[]));
      if_open_brace x

  | Ast.Code(x) -> let _ = top_level x in false

  (* this is not '...', but a list of expr/statement/params, and
     normally there should be no '...' inside them *)
  | Ast.ExprDotsTag(x) -> dots (function _ -> ()) expression x; false
  | Ast.ParamDotsTag(x) -> parameter_list x; false
  | Ast.StmtDotsTag(x) -> dots force_newline (statement "") x; false
  | Ast.DeclDotsTag(x) -> dots force_newline declaration x; false

  | Ast.TypeCTag(x) -> typeC x; false
  | Ast.ParamTag(x) -> parameterTypeDef x; false
  | Ast.SgrepStartTag(x) -> failwith "unexpected start tag"
  | Ast.SgrepEndTag(x) -> failwith "unexpected end tag"
in

(*Printf.printf "start of the function\n";*)

  anything := (function x -> let _ = pp_any x in ());

  (* todo? imitate what is in pretty_print_cocci ? *)
  match xxs with
    [] -> ()
  | x::xs ->
      (* for many tags, we must not do a newline before the first '+' *)
      let isfn s =
	match Ast.unwrap s with Ast.FunDecl _ -> true | _ -> false in
      let unindent_before = function
        (* need to get unindent before newline for } *)
	  (Ast.Token ("}",_)::_) -> true
	| _ -> false in
      let prnl x =
	(if unindent_before x then unindent true);
	force_newline() in
      let newline_before _ =
	if before =*= After
	then
	  let hd = List.hd xxs in
	  match hd with
	    (Ast.Pragma l::_)
	      when List.for_all (function Ast.Space x -> true | _ -> false) l ->
		()
          | (Ast.StatementTag s::_) when isfn s ->
	      force_newline(); force_newline()
	  | (Ast.Pragma _::_)
          | (Ast.Rule_elemTag _::_) | (Ast.StatementTag _::_)
	  | (Ast.InitTag _::_)
	  | (Ast.DeclarationTag _::_) | (Ast.Token ("}",_)::_) -> prnl hd
          | _ -> () in
      let newline_after _ =
	if before =*= Before
	then
	  match List.rev(List.hd(List.rev xxs)) with
	    (Ast.StatementTag s::_) ->
	      (if isfn s then force_newline());
	      force_newline()
	  | (Ast.Pragma _::_)
          | (Ast.Rule_elemTag _::_) | (Ast.InitTag _::_)
	  | (Ast.DeclarationTag _::_) | (Ast.Token ("{",_)::_) ->
	      force_newline()
          | _ -> () in
      (* print a newline at the beginning, if needed *)
      newline_before();
      (* print a newline before each of the rest *)
      let rec loop leading_newline indent_needed = function
	  [] -> ()
	| x::xs ->
	    (if leading_newline
	    then
	      match (indent_needed,unindent_before x) with
		(true,true) -> force_newline()
	      | (true,false) -> force_newline(); indent()
	      | (false,true) -> unindent true; force_newline()
	      | (false,false) -> force_newline());
	    let space_needed_before = function
		Ast.ParamTag(x) ->
		  (match Ast.unwrap x with
		    Ast.PComma _ -> false
		  | _ -> true)
	      |	Ast.ExpressionTag(x) ->
		  (match Ast.unwrap x with
		    Ast.EComma _ -> false
		  | _ -> true)
	      |	Ast.InitTag(x) ->
		  (match Ast.unwrap x with
		    Ast.IComma _ -> false
		  | _ -> true)
	      |	Ast.Token(t,_) when List.mem t [",";";";"(";")";".";"->"] ->
		  false
	      |	_ -> true in
	    let space_needed_after = function
		Ast.Token(t,_)
		when List.mem t ["(";".";"->"] -> (*never needed*) false
	      |	Ast.Token(t,_) when List.mem t ["if";"for";"while";"do"] ->
		  (* space always needed *)
		  pr_space(); false
	      |	Ast.ExpressionTag(e) ->
		  (match Ast.unwrap e with
		    Ast.EComma _ ->
		      (* space always needed *)
		      pr_space(); false 
		  | _ -> true)
	      |	t -> true in
	    let indent_needed =
	      let rec loop space_after indent_needed = function
		  [] -> indent_needed
		| x::xs ->
		    (if space_after && space_needed_before x
		    then pr_space());
		    let indent_needed = pp_any x in
		    let space_after = space_needed_after x in
		    loop space_after indent_needed xs in
	      loop false false x in
	    loop true indent_needed xs in
      loop false false (x::xs);
      (* print a newline at the end, if needed *)
      newline_after()

let rec pp_list_list_any (envs, pr, pr_celem, pr_cspace, pr_space, pr_arity,
			  pr_barrier, indent, unindent, eatspace)
    generating xxs before =
  List.iter
    (function env ->
      do_all (env, pr, pr_celem, pr_cspace, pr_space, pr_arity, pr_barrier,
	      indent, unindent, eatspace)
	generating xxs before)
    envs
