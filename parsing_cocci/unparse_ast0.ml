(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

open Format
module Ast = Ast_cocci
module Ast0 = Ast0_cocci
module U = Pretty_print_cocci

let quiet = ref true (* true = no decoration on - context, etc *)

let full_ids = ref false (* true = print rule name as well *)

let start_block _str =
  force_newline(); print_string "  "; open_box 0

let end_block _str =
  close_box(); force_newline ()

let print_option = Common.do_option
let print_between = Common.print_between

(* --------------------------------------------------------------------- *)
(* Positions *)

let rec meta_pos l =
  List.iter
    (function var ->
      let current_name = Ast0.meta_pos_name var in
      let (_,name) = Ast0.unwrap_mcode current_name in
      print_string "@";
      print_string name;
      meta_pos (Ast0.get_pos current_name))
    l

(* --------------------------------------------------------------------- *)
(* Modified code *)

let mcodekind brackets fn x info mc =
  let print = function
      Ast.Noindent s | Ast.Indent s | Ast.Space s -> print_string s in
  List.iter (function (s,_) -> print s) info.Ast0.strings_before;
  (match mc with
    Ast0.MINUS(plus_stream) ->
      let (lb,rb) =
	if !quiet
	then ("","")
	else
	  match brackets with
	    Some x -> ("[","]^"^(string_of_int x))
	  | None -> ("","") in
      let (plus_stream,_) = !plus_stream in
      if !quiet
      then fn x
      else (print_string "-";
	    print_string lb; fn x; print_string rb);
      (match plus_stream with
	Ast.NOREPLACEMENT -> ()
      | Ast.REPLACEMENT(plus_stream,_) -> U.print_anything ">>> " plus_stream)
  | Ast0.CONTEXT(plus_streams) ->
      let (lb,rb) =
	if !quiet
	then ("","")
	else
	  match brackets with
	    Some x -> ("[",("]^"^(string_of_int x))) | None -> ("","") in
      let (plus_streams,_t1,_t2) = !plus_streams in
      U.print_around
	(function x ->
	  print_string lb; fn x; print_string rb)
	x plus_streams
  | Ast0.PLUS _ -> print_int (info.Ast0.pos_info.Ast0.column); fn x
  | Ast0.MIXED(plus_streams) ->
      let (lb,rb) =
	if !quiet
	then ("","")
	else
	  let n =
	    match brackets with Some x -> "^"^(string_of_int x) | None -> "" in
	  ("§","½"^n) in
      let (plus_streams,_,_) = !plus_streams in
      U.print_around (function x -> print_string lb; fn x; print_string rb)
	x plus_streams);
  List.iter (function (s,_) -> print s) info.Ast0.strings_after

let mcode fn (x,_,info,mc,pos,_adj) =
  let fn x = fn x; meta_pos !pos in
  mcodekind (Some info.Ast0.pos_info.Ast0.line_start)(*None*) fn x info mc

let print_context x fn =
  mcodekind (Some (Ast0.get_line x)) fn () (Ast0.get_info x)
    (Ast0.get_mcodekind x)

let print_meta (ctx,name) =
  (if !full_ids
  then (print_string ctx; print_string ":"));
  print_string name

(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
(* Dots *)

let dots between fn d =
  print_context d
    (function _ -> print_between between fn (Ast0.unwrap d))

(* --------------------------------------------------------------------- *)
(* Disjunctions *)

let do_disj lst processor sep =
  print_string "\n("; force_newline();
  print_between (function _ -> print_string ("\n"^sep); force_newline())
    processor lst;
  print_string "\n)"

(* --------------------------------------------------------------------- *)

let print_type ty =
  print_string (Ast.string_of_fullType (Ast0toast.typeC false ty))

let print_types = function
    None -> ()
  | Some ty ->
      print_string "/* ";
      Format.print_flush();
      print_between (function _ -> print_string ", ") print_type ty;
      Format.print_flush();
      print_string " */"

(* --------------------------------------------------------------------- *)
(* Identifier *)

let rec ident i =
  print_context i
    (function _ ->
      match Ast0.unwrap i with
	Ast0.Id(name) -> mcode print_string name
      | Ast0.MetaId(name,_,_,_) -> mcode print_meta name
      | Ast0.MetaFunc(name,_,_) -> mcode print_meta name
      | Ast0.MetaLocalFunc(name,_,_) -> mcode print_meta name
      | Ast0.DisjId(_,id_list,_,_) -> do_disj id_list ident "|"
      | Ast0.ConjId(_,id_list,_,_) -> do_disj id_list ident "&"
      | Ast0.OptIdent(id) -> print_string "?"; ident id
      | Ast0.AsIdent(id,asid) -> ident id; print_string "@"; ident asid)

(* --------------------------------------------------------------------- *)
(* Expression *)

let print_string_box s = print_string s; open_box 0

let assignOp op =
  print_context op
    (function _ -> match Ast0.unwrap op with
      | Ast0.SimpleAssign op' -> mcode U.simpleAssignOp op'
      | Ast0.OpAssign op' -> mcode U.opAssignOp op'
      | Ast0.MetaAssign(name,_,_) -> mcode print_meta name)

let binaryOp op =
  print_context op
    (function _ -> match Ast0.unwrap op with
      | Ast0.Arith op' -> mcode U.arithOp op'
      | Ast0.Logical op' -> mcode U.logicalOp op'
      | Ast0.MetaBinary(name,_,_) -> mcode print_meta name)

let rec expression e =
  print_option print_type (Ast0.get_type e);
  print_context e
    (function _ ->
      match Ast0.unwrap e with
	Ast0.Ident(id) -> ident id
      | Ast0.Constant(const) -> mcode U.constant const
      | Ast0.StringConstant(lq,str,rq,_sz) ->
	  mcode print_string lq;
	  let _ = dots (function _ -> ()) string_fragment str in
	  mcode print_string rq
      | Ast0.FunCall(fn,lp,args,rp) ->
	  expression fn; mcode print_string_box lp;
	  let _ = dots (function _ -> ()) expression args in
	  close_box(); mcode print_string rp
      | Ast0.Assignment(left,op,right,_) ->
	expression left;
        print_string " ";
        assignOp op;
	print_string " ";
        expression right
      | Ast0.Sequence(left,op,right) ->
	  expression left; mcode print_string op;
	  print_string " "; expression right
      | Ast0.CondExpr(exp1,why,exp2,colon,exp3) ->
	  expression exp1; print_string " "; mcode print_string why;
	  print_option (function e -> print_string " "; expression e) exp2;
	  print_string " "; mcode print_string colon; expression exp3
      | Ast0.Postfix(exp,op) -> expression exp; mcode U.fixOp op
      | Ast0.Infix(exp,op) -> mcode U.fixOp op; expression exp
      | Ast0.Unary(exp,op) -> mcode U.unaryOp op; expression exp
      | Ast0.Binary(left,op,right) ->
	  print_string "(";
	  expression left;
          print_string " ";
          binaryOp op;
          print_string " ";
          expression right;
	  print_string ")"
      | Ast0.Nested(left,op,right) ->
	  print_string "(";
	  expression left;
          print_string " ";
          binaryOp op;
	  print_string " ";
          expression right;
	  print_string ")"
      | Ast0.Paren(lp,exp,rp) ->
	  mcode print_string_box lp; expression exp; close_box();
	  mcode print_string rp
      | Ast0.ArrayAccess(exp1,lb,exp2,rb) ->
	  expression exp1; mcode print_string_box lb; expression exp2;
	  close_box(); mcode print_string rb
      | Ast0.RecordAccess(exp,pt,field) ->
	  expression exp; mcode print_string pt; ident field
      | Ast0.RecordPtAccess(exp,ar,field) ->
	  expression exp; mcode print_string ar; ident field
      | Ast0.Cast(lp,ty,attr,rp,exp) ->
	  mcode print_string_box lp; typeC ty; close_box();
          print_attribute_list attr;
	  mcode print_string rp; expression exp
      | Ast0.SizeOfExpr(szf,exp) ->
	  mcode print_string szf; expression exp
      | Ast0.SizeOfType(szf,lp,ty,rp) ->
          mcode print_string szf;
	  mcode print_string_box lp; typeC ty; close_box();
	  mcode print_string rp
      | Ast0.TypeExp(ty) -> typeC ty
      | Ast0.Constructor(lp,ty,rp,init) ->
	  mcode print_string_box lp; typeC ty; close_box();
	  mcode print_string rp; initialiser init
      | Ast0.MetaErr(name,_,_) -> mcode print_meta name
      | Ast0.MetaExpr(name,_,ty,_,_pure,_bitfield) ->
	  mcode print_meta name; print_types ty(*;
	  print_string "^";
	  (match pure with
	    Ast0.Pure -> print_string "pure"
	  | Ast0.Impure -> print_string "impure"
	  | Ast0.Context -> print_string "context"
	  | Ast0.PureContext -> print_string "pure_context")*)
      | Ast0.MetaExprList(name,_,_,_) -> mcode print_meta name
      | Ast0.EComma(cm) -> mcode print_string cm; print_space()
      | Ast0.DisjExpr(_,exp_list,_,_) -> do_disj exp_list expression "|"
      | Ast0.ConjExpr(_,exp_list,_,_) -> do_disj exp_list expression "&"
      | Ast0.NestExpr(starter,expr_dots,ender,None,multi) ->
	  mcode print_string starter;
	  start_block(); dots force_newline expression expr_dots; end_block();
	  mcode print_string ender
      | Ast0.NestExpr(starter,expr_dots,ender,Some (_,_,whencode),multi) ->
	  mcode print_string starter; print_string "   WHEN != ";
	  expression whencode;
	  start_block(); dots force_newline expression expr_dots; end_block();
	  mcode print_string ender
      | Ast0.Edots(dots,Some (_,_,whencode)) ->
	  mcode print_string dots; print_string "   WHEN != ";
	  expression whencode
      | Ast0.Edots(dots,None) -> mcode print_string dots
      | Ast0.OptExp(exp) -> print_string "?"; expression exp
      |	Ast0.AsExpr(exp,asexp) -> expression exp; print_string "@";
	  expression asexp
      |	Ast0.AsSExpr(exp,asstm) -> expression exp; print_string "@";
	  statement "" asstm)

and expression_dots x = dots (function _ -> ()) expression x

and string_fragment e =
  match Ast0.unwrap e with
    Ast0.ConstantFragment(str) -> mcode print_string str
  | Ast0.FormatFragment(pct,fmt) ->
      mcode print_string pct;
      string_format fmt
  | Ast0.Strdots dots -> mcode print_string dots
  | Ast0.MetaFormatList(pct,name,_,lenname) ->
      mcode print_string pct;
      mcode print_meta name

and string_format e =
  match Ast0.unwrap e with
    Ast0.ConstantFormat(str) -> mcode print_string str
  | Ast0.MetaFormat(name,_) -> mcode print_meta name

(* --------------------------------------------------------------------- *)
(* Types *)

and print_parentype (lp,ty,rp) fn =
  let function_pointer ty1 array_decs =
    match Ast0.unwrap ty1 with
      Ast0.Pointer(ty2,star) ->
        (match Ast0.unwrap ty2 with
          Ast0.FunctionType(ty3,lp3,params,rp3) ->
            typeC ty3;
            mcode print_string lp;
            mcode print_string star;
            let _ =
              match array_decs with
                Some(lb1,size,rb1) ->
                  mcode print_string lb1;
                  print_option expression size;
                  mcode print_string rb1
              | None -> () in
            mcode print_string rp;
            mcode print_string lp3;
            parameter_list params;
            mcode print_string rp3;
	| _ -> failwith "ParenType Unparse_ast0")
    | _ -> failwith "ParenType Unparse_ast0" in
  match Ast0.unwrap ty with
    Ast0.Array(ty1,lb1,size,rb1) ->
      function_pointer ty1 (Some(lb1,size,rb1))
  | Ast0.Pointer(ty1,star) ->
      function_pointer ty None
  | _ -> failwith "ParenType Unparse_ast0"

and typeC t =
  print_context t
    (function _ ->
      match Ast0.unwrap t with
	Ast0.ConstVol(cv,ty) ->
	  mcode U.const_vol cv; print_string " "; typeC ty
      |	Ast0.BaseType(ty,strings) ->
	  List.iter (function s -> mcode print_string s; print_string " ")
	    strings
      |	Ast0.Signed(sgn,ty) -> mcode U.sign sgn; print_option typeC ty
      | Ast0.Pointer(ty,star) -> typeC ty; mcode print_string star
      | Ast0.ParenType(lp,ty,rp) ->
          print_parentype (lp,ty,rp) (function _ -> ())
      | Ast0.FunctionType(ty,lp,params,rp) ->
          typeC ty;
          mcode print_string lp;
          parameter_list params;
          mcode print_string rp
      | Ast0.Array(ty,lb,size,rb) ->
	  typeC ty; mcode print_string lb; print_option expression size;
	  mcode print_string rb
      | Ast0.Decimal(dec,lp,length,comma,precision_opt,rp) ->
	  mcode print_string dec;
	  mcode print_string lp;
	  expression length;
	  print_option (mcode print_string) comma;
	  print_option expression precision_opt;
	  mcode print_string rp
      | Ast0.EnumName(kind,name) ->
	  mcode print_string kind;
	  print_option (function x -> ident x; print_string " ") name
      | Ast0.EnumDef(ty,lb,ids,rb) ->
	  typeC ty; mcode print_string lb;
	  dots force_newline enum_decl ids;
	  mcode print_string rb
      | Ast0.StructUnionName(kind,name) ->
	  mcode U.structUnion kind;
	  print_option (function x -> ident x; print_string " ") name
      | Ast0.StructUnionDef(ty,lb,decls,rb) ->
	  typeC ty; mcode print_string lb;
	  dots force_newline field decls;
	  mcode print_string rb
      | Ast0.TypeOfExpr(tf,lp,exp,rp) ->
          mcode print_string tf;
	  mcode print_string_box lp; expression exp; close_box();
	  mcode print_string rp
      | Ast0.TypeOfType(tf,lp,ty,rp) ->
          mcode print_string tf;
	  mcode print_string_box lp; typeC ty; close_box();
	  mcode print_string rp
      | Ast0.TypeName(name)-> mcode print_string name; print_string " "
      | Ast0.AutoType(auto) -> mcode print_string auto; print_string " "
      | Ast0.MetaType(name,_,_)-> mcode print_meta name; print_string " "
      | Ast0.DisjType(_,types,_,_) -> do_disj types typeC "|"
      | Ast0.ConjType(_,types,_,_) -> do_disj types typeC "&"
      | Ast0.OptType(ty) -> print_string "?"; typeC ty
      | Ast0.AsType(ty,asty) -> typeC ty; print_string "@"; typeC asty)

(* --------------------------------------------------------------------- *)
(* Variable declaration *)
(* Even if the Cocci program specifies a list of declarations, they are
   split out into multiple declarations of a single variable each. *)

and print_named_type ty id =
  match Ast0.unwrap ty with
    Ast0.Array(ty,lb,size,rb) ->
      let rec loop ty k =
	match Ast0.unwrap ty with
	  Ast0.Array(ty,lb,size,rb) ->
	    loop ty
	      (function _ ->
		k ();
		mcode print_string lb;
		print_option expression size;
		mcode print_string rb)
	| _ -> typeC ty; ident id; k () in
      loop ty (function _ -> ())
  | Ast0.ParenType(lp,ty,rp) ->
      print_parentype (lp,ty,rp) (function _ -> ident id)
  | _ -> typeC ty; ident id

and declaration d =
  print_context d
    (function _ ->
      match Ast0.unwrap d with
	Ast0.MetaDecl(name,_,_) ->
	  mcode print_meta name
      |	Ast0.Init(stg,ty,id,attr,eq,ini,sem) ->
	  print_option (mcode U.storage) stg;
	  print_named_type ty id;
          print_attribute_list attr;
	  print_string " ";
	  mcode print_string eq; print_string " "; initialiser ini;
	  mcode print_string sem
      | Ast0.UnInit(stg,ty,id,attr,sem) ->
	  print_option (mcode U.storage) stg; print_named_type ty id;
          print_attribute_list attr;
	  mcode print_string sem
      | Ast0.FunProto(fninfo,name,lp1,params,va,rp1,sem) ->
	  List.iter print_fninfo fninfo;
	  ident name; mcode print_string_box lp1;
	  parameter_list params; varargs va;
	  close_box(); mcode print_string rp1;
	  mcode print_string sem
      | Ast0.MacroDecl(stg,name,lp,args,rp,attr,sem) ->
	  print_option (mcode U.storage) stg;
	  ident name; mcode print_string_box lp;
	  let _ = dots (function _ -> ()) expression args in
	  close_box(); mcode print_string rp;
          print_attribute_list attr;
	  mcode print_string sem
      | Ast0.MacroDeclInit(stg,name,lp,args,rp,eq,ini,sem) ->
	  print_option (mcode U.storage) stg;
	  ident name; mcode print_string_box lp;
	  let _ = dots (function _ -> ()) expression args in
	  close_box(); mcode print_string rp;
          print_string " ";
          mcode print_string eq; print_string " "; initialiser ini;
	  mcode print_string sem
      | Ast0.TyDecl(ty,attr,sem) ->
          typeC ty;
          print_attribute_list attr;
          mcode print_string sem
      | Ast0.Typedef(stg,ty,id,sem) ->
	  mcode print_string stg; typeC ty; typeC id;
	  mcode print_string sem
      | Ast0.DisjDecl(_,decls,_,_) ->
	  do_disj decls declaration "|"
      | Ast0.ConjDecl(_,decls,_,_) ->
	  do_disj decls declaration "&"
      | Ast0.OptDecl(decl) -> print_string "?"; declaration decl
      | Ast0.AsDecl(decl,asdecl) ->
	  declaration decl; print_string "@"; declaration asdecl)

and declaration_dots l = dots (function _ -> ()) declaration l

(* --------------------------------------------------------------------- *)
(* Field declaration *)

and field d =
  print_context d
    (function _ ->
      match Ast0.unwrap d with
	Ast0.MetaField(name,_,_)
      | Ast0.MetaFieldList(name,_,_,_) ->
	  mcode print_meta name
      | Ast0.Field(ty,id,bf,sem) ->
	  begin
	    match id with
	      None -> typeC ty
	    | Some id -> print_named_type ty id
	  end;
	  let bitfield (c, e) =
	    mcode print_string c;
	    expression e in
	  Common.do_option bitfield bf;
	  mcode print_string sem
      | Ast0.DisjField(_,fields,_,_) ->
	  do_disj fields field "|"
      | Ast0.ConjField(_,fields,_,_) ->
	  do_disj fields field "&"
      | Ast0.OptField(decl) -> print_string "?"; field decl
      | Ast0.Fdots(dots,Some (_,_,whencode)) ->
	  mcode print_string dots; print_string "   when != ";
	  field whencode
      | Ast0.Fdots(dots,None) -> mcode print_string dots)

and field_dots l = dots (function _ -> ()) field l

and enum_decl d =
  print_context d
    (function _ ->
      match Ast0.unwrap d with
	Ast0.Enum(name,enum_val) ->
          (match enum_val with
            None -> ident name
          | Some(eq,eval) ->
              ident name; mcode print_string eq; expression eval)
      | Ast0.EnumComma(cm) ->
          mcode print_string cm; force_newline()
      | Ast0.EnumDots(dots,Some (_,_,whencode)) ->
	  mcode print_string dots; print_string "   when != ";
	  enum_decl whencode
      | Ast0.EnumDots(dots,None) -> mcode print_string dots)

and enum_decl_dots l = dots (function _ -> ()) enum_decl l

(* --------------------------------------------------------------------- *)
(* Initialiser *)

and initialiser i =
  print_context i
    (function _ ->
      match Ast0.unwrap i with
	Ast0.MetaInit(name,_,_)-> mcode print_meta name; print_string " "
      |	Ast0.MetaInitList(name,_,_,_)-> mcode print_meta name; print_string " "
      |	Ast0.InitExpr(exp) -> expression exp
      | Ast0.InitList(lb,initlist,rb,ordered) ->
          (*doesn't show commas dropped in unordered case*)
	  mcode print_string lb; open_box 0;
	  let _ = dots (function _ -> ()) initialiser initlist in
	  close_box(); mcode print_string rb
      | Ast0.InitGccExt(designators,eq,ini) ->
	  List.iter designator designators; print_string " ";
	  mcode print_string eq; print_string " "; initialiser ini
      | Ast0.InitGccName(name,eq,ini) ->
	  ident name; mcode print_string eq; initialiser ini
      | Ast0.IComma(cm) -> mcode print_string cm; force_newline()
      | Ast0.Idots(d,Some (_,_,whencode)) ->
	  mcode print_string d; print_string "   WHEN != ";
	  initialiser whencode
      | Ast0.Idots(d,None) -> mcode print_string d
      | Ast0.OptIni(ini) -> print_string "?"; initialiser ini
      | Ast0.AsInit(ini,asini) -> initialiser ini; print_string "@";
	  initialiser asini)

and designator = function
      Ast0.DesignatorField(dot,id) -> mcode print_string dot; ident id
    | Ast0.DesignatorIndex(lb,exp,rb) ->
	mcode print_string lb; expression exp; mcode print_string rb
    | Ast0.DesignatorRange(lb,min,dots,max,rb) ->
	mcode print_string lb; expression min; mcode print_string dots;
	expression max; mcode print_string rb

and initialiser_list l = dots (function _ -> ()) initialiser l

(* --------------------------------------------------------------------- *)
(* Parameter *)

and parameterTypeDef p =
  print_context p
    (function _ ->
      match Ast0.unwrap p with
        Ast0.VoidParam(ty,attr) ->
          typeC ty;
          print_attribute_list attr;
      | Ast0.Param(ty,Some id,attr) ->
          print_named_type ty id;
          print_attribute_list attr;
      | Ast0.Param(ty,None,attr) ->
          typeC ty;
          print_attribute_list attr;
      | Ast0.MetaParam(name,_,_) -> mcode print_meta name
      | Ast0.MetaParamList(name,_,_,_) -> mcode print_meta name
      | Ast0.PComma(cm) -> mcode print_string cm; print_space()
      | Ast0.Pdots(dots) -> mcode print_string dots
      | Ast0.OptParam(param) -> print_string "?"; parameterTypeDef param
      |	Ast0.AsParam(p,asexp) -> parameterTypeDef p; print_string "@";
	  expression asexp)

and parameter_list l = dots (function _ -> ()) parameterTypeDef l
and varargs va = match va with
  | None -> ()
  | Some (comma, ellipsis) ->
    mcode print_string comma; mcode print_string ellipsis
(* --------------------------------------------------------------------- *)
(* Top-level code *)

and statement arity s =
  print_context s
    (function _ ->
      match Ast0.unwrap s with
	Ast0.FunDecl(_,fninfo,name,lp,params,va,rp,lbrace,body,rbrace,_) ->
	  print_string arity;
	  List.iter print_fninfo fninfo;
	  ident name; mcode print_string_box lp;
	  parameter_list params; varargs va; close_box(); mcode print_string rp;
	  print_string " ";
	  print_string arity; mcode print_string lbrace; start_block();
	  dots force_newline (statement arity) body;
	  end_block(); print_string arity; mcode print_string rbrace
      | Ast0.Decl(_,decl) ->
	  print_string arity; declaration decl
      | Ast0.Seq(lbrace,body,rbrace) ->
	  print_string arity; mcode print_string lbrace; start_block();
	  dots force_newline (statement arity) body;
	  end_block(); print_string arity; mcode print_string rbrace
      | Ast0.ExprStatement(exp,sem) ->
	  print_string arity; print_option expression exp;
	  mcode print_string sem
      | Ast0.IfThen(iff,lp,exp,rp,branch1,(info,aft,adj)) ->
	  print_string arity;
	  mcode print_string iff; print_string " "; mcode print_string_box lp;
	  expression exp; close_box(); mcode print_string rp; print_string " ";
	  statement arity branch1;
	  mcode (function _ -> ()) ((),(),info,aft,ref [],adj)
      | Ast0.IfThenElse(iff,lp,exp,rp,branch1,els,branch2,(info,aft,adj)) ->
	  print_string arity;
	  mcode print_string iff; print_string " "; mcode print_string_box lp;
	  expression exp; close_box(); mcode print_string rp; print_string " ";
	  statement arity branch1;
	  print_string arity; mcode print_string els; print_string " ";
	  statement arity branch2;
	  mcode (function _ -> ()) ((),(),info,aft,ref [],adj)
      | Ast0.While(whl,lp,exp,rp,body,(info,aft,adj)) ->
	  print_string arity;
	  mcode print_string whl; print_string " "; mcode print_string_box lp;
	  expression exp; close_box(); mcode print_string rp; print_string " ";
	  statement arity body;
	  mcode (function _ -> ()) ((),(),info,aft,ref [],adj)
      | Ast0.Do(d,body,whl,lp,exp,rp,sem) ->
	  print_string arity; mcode print_string d; print_string " ";
	  statement arity body;
	  print_string arity;
	  mcode print_string whl; print_string " "; mcode print_string_box lp;
	  expression exp; close_box(); mcode print_string rp;
	  mcode print_string sem
      | Ast0.For(fr,lp,first,e2,sem2,e3,rp,body,(info,aft,adj)) ->
	  print_string arity;
	  mcode print_string fr; mcode print_string_box lp;
	  (match Ast0.unwrap first with
	    Ast0.ForExp(e1,sem1) ->
	      print_option expression e1; mcode print_string sem1
	  | Ast0.ForDecl (_,decl) -> declaration decl);
	  print_option expression e2; mcode print_string sem2;
	  print_option expression e3; close_box();
	  mcode print_string rp; print_string " "; statement arity body;
	  mcode (function _ -> ()) ((),(),info,aft,ref [],adj)
      | Ast0.Iterator(nm,lp,args,rp,body,(info,aft,adj)) ->
	  print_string arity;
	  ident nm; print_string " "; mcode print_string_box lp;
	  let _ = dots (function _ -> ()) expression args in
	  close_box(); mcode print_string rp; print_string " ";
	  statement arity body;
	  mcode (function _ -> ()) ((),(),info,aft,ref [],adj)
      |	Ast0.Switch(switch,lp,exp,rp,lb,decls,cases,rb) ->
	  print_string arity;
	  mcode print_string switch; print_string " ";
	  mcode print_string_box lp; expression exp; close_box();
	  mcode print_string rp; print_string " "; mcode print_string lb;
	  dots force_newline (statement arity) decls;
	  dots force_newline (case_line arity) cases;
	  mcode print_string rb
      | Ast0.Break(br,sem) ->
	  print_string arity; mcode print_string br; mcode print_string sem
      | Ast0.Continue(cont,sem) ->
	  print_string arity; mcode print_string cont; mcode print_string sem
      |	Ast0.Label(l,dd) -> ident l; print_string ":"
      | Ast0.Goto(goto,l,sem) ->
	  mcode print_string goto; ident l; mcode print_string sem
      | Ast0.Return(ret,sem) ->
	  print_string arity; mcode print_string ret; mcode print_string sem
      | Ast0.ReturnExpr(ret,exp,sem) ->
	  print_string arity; mcode print_string ret; print_string " ";
	  expression exp; mcode print_string sem
      | Ast0.Exec(exec,lang,code,sem) ->
	  print_string arity; mcode print_string exec; print_string " ";
	  mcode print_string lang; print_string " ";
	  dots (function _ -> print_string " ") exec_code code;
	  mcode print_string sem
      | Ast0.MetaStmt(name,_,pure) ->
	  print_string arity; mcode print_meta name;(*
	  print_string "^";
	  (match pure with
	    Ast0.Pure -> print_string "pure"
	  | Ast0.Impure -> print_string "impure"
	  | Ast0.Context -> print_string "context"
	  | Ast0.PureContext -> print_string "pure_context")*)
      | Ast0.MetaStmtList(name,_,_,_) ->
	  print_string arity;  mcode print_meta name
      | Ast0.Disj(starter,statement_dots_list,_,ender) ->
	  print_string arity;
	  print_string "\n"; mcode print_string starter; force_newline();
	  print_between
	    (function _ -> print_string "\n|"; force_newline())
	    (dots force_newline (statement arity))
	    statement_dots_list;
	  print_string "\n"; mcode print_string ender
      | Ast0.Conj(starter,statement_dots_list,_,ender) ->
	  print_string arity;
	  print_string "\n"; mcode print_string starter; force_newline();
	  print_between
	    (function _ -> print_string "\n&"; force_newline())
	    (dots force_newline (statement arity))
	    statement_dots_list;
	  print_string "\n"; mcode print_string ender
      | Ast0.Nest(starter,stmt_dots,ender,whn,multi) ->
	  print_string arity;
	  mcode print_string starter;
	  open_box 0;
	  List.iter
	    (whencode (dots force_newline (statement "")) (statement ""))
	    whn;
	  close_box();
	  start_block();
	  dots force_newline (statement arity) stmt_dots;
	  end_block();
	  mcode print_string ender
      | Ast0.Exp(exp) -> print_string arity; expression exp
      | Ast0.TopExp(exp) -> print_string arity; expression exp
      | Ast0.Ty(ty) -> print_string arity; typeC ty
      | Ast0.TopId(id) -> print_string arity; ident id
      |	Ast0.TopInit(init) -> initialiser init
      | Ast0.Dots(d,whn) ->
	  print_string arity; mcode print_string d;
	  List.iter
	    (whencode (dots force_newline (statement "")) (statement ""))
	    whn
      | Ast0.Include(inc,s) ->
	  mcode print_string inc; print_string " "; mcode U.inc_file s
      | Ast0.MetaInclude(inc,s) ->
	  mcode print_string inc; print_string " "; expression s
      | Ast0.Undef(def,id) ->
	  mcode print_string def; print_string " "; ident id
      | Ast0.Define(def,id,params,body) ->
	  mcode print_string def; print_string " "; ident id;
	  print_define_parameters params;
	  print_string " ";
	  dots force_newline (statement arity) body
      | Ast0.Pragma(prg,id,body) ->
	  mcode print_string prg; print_string " "; ident id;
	  print_string " "; pragmainfo body
      | Ast0.OptStm(re) -> statement "?" re
      | Ast0.AsStmt(stm,asstm) -> statement arity stm; print_string "@";
	  statement arity asstm)

and pragmainfo pi =
  match Ast0.unwrap pi with
      Ast0.PragmaString(s) -> mcode print_string s
    | Ast0.PragmaDots(dots) -> mcode print_string dots

and print_define_parameters params =
  match Ast0.unwrap params with
    Ast0.NoParams -> ()
  | Ast0.DParams(lp,params,rp) ->
      mcode print_string lp;
      dots (function _ -> ()) print_define_param params; mcode print_string rp

and print_define_param param =
  match Ast0.unwrap param with
    Ast0.DParam(id) -> ident id
  | Ast0.MetaDParamList(name,_,_,_) -> mcode print_meta name
  | Ast0.DPComma(comma) -> mcode print_string comma
  | Ast0.DPdots(dots) -> mcode print_string dots
  | Ast0.OptDParam(dp) -> print_string "?"; print_define_param dp

and define_param_dots l = dots (function _ -> ()) print_define_param l

and print_fninfo = function
    Ast0.FStorage(stg) -> mcode U.storage stg
  | Ast0.FType(ty) -> typeC ty
  | Ast0.FInline(inline) -> mcode print_string inline
  | Ast0.FAttr(attr) -> print_attribute attr

and print_attribute_list attrs =
  if not (attrs = []) then print_string " ";
  print_between (fun _ -> print_string " ") print_attribute attrs

and print_attribute a =
  match Ast0.unwrap a with
    Ast0.Attribute(attr) -> print_attr_arg attr

and print_attr_arg a =
  match Ast0.unwrap a with
    Ast0.AttrName(arg) -> mcode print_string arg
  | Ast0.MetaAttr(name,_,_) -> mcode print_meta name

and whencode notfn alwaysfn = function
    Ast0.WhenNot (_,_,a) ->
      print_string "   WHEN != "; open_box 0; notfn a; close_box()
  | Ast0.WhenAlways (_,_,a) ->
      print_string "   WHEN = "; open_box 0; alwaysfn a; close_box()
  | Ast0.WhenModifier (_,x) -> print_string "   WHEN "; U.print_when_modif x
  | Ast0.WhenNotTrue (_,_,a) ->
      print_string "   WHEN != TRUE "; open_box 0; expression a; close_box()
  | Ast0.WhenNotFalse (_,_,a) ->
      print_string "   WHEN != FALSE "; open_box 0; expression a; close_box()

and case_line arity c =
  print_context c
    (function _ ->
      match Ast0.unwrap c with
	Ast0.Default(def,colon,code) ->
	  print_string arity;
	  mcode print_string def; mcode print_string colon; print_string " ";
	  dots force_newline (statement arity) code
      | Ast0.Case(case,exp,colon,code) ->
	  print_string arity;
	  mcode print_string case; print_string " "; expression exp;
	  mcode print_string colon; print_string " ";
	  dots force_newline (statement arity) code
      | Ast0.DisjCase(starter,case_lines,mids,ender) ->
	  do_disj case_lines (case_line arity) "|"
      | Ast0.OptCase(case) -> case_line "?" case)

and statement_dots l = dots (function _ -> ()) (statement "") l
and case_dots l = dots (function _ -> ()) (case_line "") l

and exec_code e =
  match Ast0.unwrap e with
    Ast0.ExecEval(colon,id) -> mcode print_string colon; expression id
  | Ast0.ExecToken(tok) -> mcode print_string tok
  | Ast0.ExecDots(dots) -> mcode print_string dots

(* --------------------------------------------------------------------- *)
(* Top level code *)

let top_level t =
  print_context t
    (function _ ->
      match Ast0.unwrap t with
	Ast0.FILEINFO(old_file,new_file) ->
	  print_string "--- "; mcode print_string old_file; force_newline();
	  print_string "+++ "; mcode print_string new_file
      | Ast0.NONDECL(stmt) -> statement "" stmt
      | Ast0.CODE(stmt_dots) | Ast0.TOPCODE(stmt_dots) ->
	  dots force_newline (statement "") stmt_dots
      | Ast0.ERRORWORDS(exps) ->
	  print_string "error words = [";
	  print_between (function _ -> print_string ", ") expression exps;
	  print_string "]"
      | Ast0.OTHER(s) ->
	  print_string "OTHER("; statement "" s; print_string ")")

let rule =
  print_between (function _ -> force_newline(); force_newline()) top_level

let rec unparse_anything x =
  let q = !quiet in
  quiet := true;
  (match x with
    Ast0.DotsExprTag(d) ->
      print_string "ExpDots:"; force_newline();
      expression_dots d
  | Ast0.DotsParamTag(d) ->
      parameter_list d
  | Ast0.DotsInitTag(d) ->
      initialiser_list d
  | Ast0.DotsStmtTag(d) ->
      print_string "StmDots:"; force_newline();
      statement_dots d
  | Ast0.DotsDeclTag(d) -> declaration_dots d
  | Ast0.DotsFieldTag(d) -> field_dots d
  | Ast0.DotsEnumDeclTag(d) -> enum_decl_dots d
  | Ast0.DotsCaseTag(d) -> case_dots d
  | Ast0.DotsDefParamTag(d) -> define_param_dots d
  | Ast0.IdentTag(d)    -> ident d
  | Ast0.ExprTag(d) | Ast0.ArgExprTag(d) | Ast0.TestExprTag(d) ->
      print_string "Exp:"; force_newline();
      expression d
  | Ast0.AssignOpTag(d) ->
      print_string ("AssignOp: " ^ (Ast0.string_of_assignOp d)); force_newline();
  | Ast0.BinaryOpTag(d) ->
      print_string ("BinaryOp: " ^ (Ast0.string_of_binaryOp d)); force_newline();
  | Ast0.TypeCTag(d) -> typeC d
  | Ast0.ParamTag(d) -> parameterTypeDef d
  | Ast0.InitTag(d)  -> initialiser d
  | Ast0.DeclTag(d)  -> declaration d
  | Ast0.FieldTag(d)  -> field d
  | Ast0.EnumDeclTag(d)  -> enum_decl d
  | Ast0.StmtTag(d)  ->
      print_string "Stm:"; force_newline();
      statement "" d
  | Ast0.ForInfoTag(fi)  ->
      print_string "ForInfo:"; force_newline();
      (match Ast0.unwrap fi with
	Ast0.ForExp(e1,sem1) ->
	  print_option expression e1; mcode print_string sem1
      | Ast0.ForDecl (_,decl) -> declaration decl)
  | Ast0.CaseLineTag(d)  -> case_line "" d
  | Ast0.StringFragmentTag(d)  -> string_fragment d
  | Ast0.AttributeTag(d) -> print_attribute d
  | Ast0.AttrArgTag(d)   -> print_attr_arg d
  | Ast0.TopTag(d)       -> top_level d
  | Ast0.IsoWhenTag(x)   -> U.print_when_modif x
  | Ast0.IsoWhenTTag(e)  -> expression e
  | Ast0.IsoWhenFTag(e)  -> expression e
  | Ast0.MetaPosTag(var) -> meta_pos [x]
  | Ast0.HiddenVarTag(var) -> failwith "should not need to be printed"
  | Ast0.WhenTag(w,e,a) -> unparse_anything a);
  quiet := q;
  print_newline()

let unparse x =
  print_string "\n@@\n@@";
  force_newline();
  force_newline();
  rule x;
  print_newline()

let unparse_to_string x = Common.format_to_string (function _ -> unparse x)
let unparse_x_to_string fn x =
  let q = !quiet in
  quiet := true;
  let res = Common.format_to_string (function _ -> fn x) in
  quiet := q;
  res

let show_cocci_parse_tree comment parse_tree =
  Printf.printf "%s\n" comment;
  top_level parse_tree; Format.print_newline()
