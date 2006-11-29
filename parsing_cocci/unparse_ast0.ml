open Format
module Ast0 = Ast0_cocci
module Ast = Ast_cocci
module U = Pretty_print_cocci

let quiet = ref false (* false = no decoration on - context, etc *)

let start_block str =
  force_newline(); print_string "  "; open_box 0

let end_block str =
  close_box(); force_newline ()

let print_option fn = Common.do_option fn
let print_between between fn = Common.print_between between fn

(* --------------------------------------------------------------------- *)
(* Modified code *)

let mcodekind brackets fn x = function
    Ast0.MINUS(plus_stream) ->
      let (lb,rb) =
	if !quiet
	then ("","")
	else match brackets with Some _ -> ("[","]") | None -> ("","") in
      let (plus_stream,_) = !plus_stream in
      if !quiet
      then fn x
      else (print_string "-"; print_string lb; fn x; print_string rb);
      U.print_anything ">>> " plus_stream
  | Ast0.CONTEXT(plus_streams) ->
      let (lb,rb) =
	if !quiet
	then ("","")
	else
	  match brackets with
	    Some x -> ("[",("]^"^(string_of_int x))) | None -> ("","") in
      let (plus_streams,_,_) = !plus_streams in
      U.print_around (function x -> print_string lb; fn x; print_string rb)
	x plus_streams
  | Ast0.PLUS -> fn x
  | Ast0.MIXED(plus_streams) ->
      let (lb,rb) =if !quiet then ("","") else  ("§","½") in
      let (plus_streams,_,_) = !plus_streams in
      U.print_around (function x -> print_string lb; fn x; print_string rb)
	x plus_streams

let mcode fn (x,_,_,mc) = mcodekind None fn x mc

let print_context (_,info,i,mc,ty,_) fn =
  mcodekind (Some info.Ast0.line_start) fn () !mc

(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
(* Dots *)

let dots between fn d =
  print_context d
    (function _ ->
      match Ast0.unwrap d with
	Ast0.DOTS(l) -> print_between between fn l
      | Ast0.CIRCLES(l) -> print_between between fn l
      | Ast0.STARS(l) -> print_between between fn l)

(* --------------------------------------------------------------------- *)
(* Identifier *)

let rec ident i =
  print_context i
    (function _ ->
      match Ast0.unwrap i with
	Ast0.Id(name) -> mcode print_string name
      | Ast0.MetaId(name) -> mcode print_string name
      | Ast0.MetaFunc(name) -> mcode print_string name
      | Ast0.MetaLocalFunc(name) -> mcode print_string name
      | Ast0.OptIdent(id) -> print_string "?"; ident id
      | Ast0.UniqueIdent(id) -> print_string "!"; ident id
      | Ast0.MultiIdent(id) -> print_string "\\+"; ident id)

(* --------------------------------------------------------------------- *)
(* Expression *)

let print_string_box s = print_string s; open_box 0

let rec expression e =
  print_context e
    (function _ ->
      match Ast0.unwrap e with
	Ast0.Ident(id) -> ident id
      | Ast0.Constant(const) -> mcode U.constant const
      | Ast0.FunCall(fn,lp,args,rp) ->
	  expression fn; mcode print_string_box lp;
	  let _ = dots (function _ -> ()) expression args in
	  close_box(); mcode print_string rp
      | Ast0.Assignment(left,op,right) ->
	  expression left; print_string " "; mcode U.assignOp op;
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
	  expression left; print_string " "; mcode U.binaryOp op;
	  print_string " "; expression right;
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
      | Ast0.Cast(lp,ty,rp,exp) ->
	  mcode print_string_box lp; typeC ty; close_box();
	  mcode print_string rp; expression exp
      | Ast0.SizeOfExpr(szf,exp) ->
	  mcode print_string szf; expression exp
      | Ast0.SizeOfType(szf,lp,ty,rp) ->
          mcode print_string szf;
	  mcode print_string_box lp; typeC ty; close_box();
	  mcode print_string rp; 
      | Ast0.MetaConst(name,None) -> mcode print_string name
      | Ast0.MetaConst(name,Some ty) ->
	  mcode print_string name; print_string "/* ";
	  print_between (function _ -> print_string ", ") Type_cocci.typeC ty;
	  print_string "*/"
      | Ast0.MetaErr(name) -> mcode print_string name
      | Ast0.MetaExpr(name,None) -> mcode print_string name
      | Ast0.MetaExpr(name,Some ty) ->
	  mcode print_string name; print_string "/*";
	  print_between (function _ -> print_string ", ") Type_cocci.typeC ty;
	  print_string "*/"
      | Ast0.MetaExprList(name) -> mcode print_string name
      | Ast0.EComma(cm) -> mcode print_string cm; print_space()
      | Ast0.DisjExpr(_,exp_list,_,_) ->
	  print_string "\n("; force_newline();
	  print_between
	    (function _ -> print_string "\n|"; force_newline())
	    expression exp_list;
	  print_string "\n)"
      | Ast0.NestExpr(starter,expr_dots,ender,None) ->
	  mcode print_string starter;
	  start_block(); dots force_newline expression expr_dots; end_block();
	  mcode print_string ender
      | Ast0.NestExpr(starter,expr_dots,ender,Some whencode) ->
	  mcode print_string starter; print_string "   WHEN != ";
	  expression whencode;
	  start_block(); dots force_newline expression expr_dots; end_block();
	  mcode print_string ender
      | Ast0.Edots(dots,Some whencode)
      | Ast0.Ecircles(dots,Some whencode)
      | Ast0.Estars(dots,Some whencode) ->
	  mcode print_string dots; print_string "   WHEN != ";
	  expression whencode
      | Ast0.Edots(dots,None)
      | Ast0.Ecircles(dots,None)
      | Ast0.Estars(dots,None) -> mcode print_string dots
      | Ast0.OptExp(exp) -> print_string "?"; expression exp
      | Ast0.UniqueExp(exp) -> print_string "!"; expression exp
      | Ast0.MultiExp(exp) -> print_string "\\+"; expression exp)

and expression_dots x = dots (function _ -> ()) expression x

(* --------------------------------------------------------------------- *)
(* Types *)

and typeC t =
  print_context t
    (function _ ->
      match Ast0.unwrap t with
	Ast0.ConstVol(cv,ty) ->
	  mcode U.const_vol cv; print_string " "; typeC ty
      |	Ast0.BaseType(ty,sgn) ->
	  mcode U.baseType ty; print_option (mcode U.sign) sgn
      | Ast0.Pointer(ty,star) -> typeC ty; mcode print_string star
      | Ast0.Array(ty,lb,size,rb) ->
	  typeC ty; mcode print_string lb; print_option expression size;
	  mcode print_string rb
      | Ast0.StructUnionName(kind,name) ->
	  mcode U.structUnion kind; ident name; print_string " "
      | Ast0.StructUnionDef(kind,name,lb,decls,rb) ->
	  mcode U.structUnion kind; ident name; print_string " ";
	  mcode print_string lb;
	  print_between force_newline declaration decls;
	  mcode print_string rb
      | Ast0.TypeName(name)-> mcode print_string name; print_string " "
      | Ast0.MetaType(name)-> mcode print_string name; print_string " "
      | Ast0.OptType(ty) -> print_string "?"; typeC ty
      | Ast0.UniqueType(ty) -> print_string "!"; typeC ty
      | Ast0.MultiType(ty) -> print_string "\\+"; typeC ty)

(* --------------------------------------------------------------------- *)
(* Variable declaration *)
(* Even if the Cocci program specifies a list of declarations, they are
   split out into multiple declarations of a single variable each. *)

and declaration d =
  print_context d
    (function _ ->
      match Ast0.unwrap d with
	Ast0.Init(stg,ty,id,eq,ini,sem) ->
	  print_option (mcode U.storage) stg;
	  typeC ty; ident id; print_string " ";
	  mcode print_string eq; print_string " "; initialiser ini;
	  mcode print_string sem
      | Ast0.UnInit(stg,ty,id,sem) ->
	  print_option (mcode U.storage) stg;
	  typeC ty; ident id; mcode print_string sem
      | Ast0.TyDecl(ty,sem) -> typeC ty; mcode print_string sem
      | Ast0.DisjDecl(_,decls,_,_) ->
	  print_string "\n("; force_newline();
	  print_between
	    (function _ -> print_string "\n|"; force_newline())
	    declaration decls;
	  print_string "\n)"
      | Ast0.OptDecl(decl) -> print_string "?"; declaration decl
      | Ast0.UniqueDecl(decl) -> print_string "!"; declaration decl
      | Ast0.MultiDecl(decl) -> print_string "\\+"; declaration decl)

(* --------------------------------------------------------------------- *)
(* Initialiser *)

and initialiser i =
  print_context i
    (function _ ->
      match Ast0.unwrap i with
	Ast0.InitExpr(exp) -> expression exp
      | Ast0.InitList(lb,initlist,rb) ->
	  mcode print_string lb;
	  let _ = dots (function _ -> ()) initialiser initlist in
	  mcode print_string rb
      | Ast0.InitGccDotName(dot,name,eq,ini) ->
	  mcode print_string dot; ident name; print_string " ";
	  mcode print_string eq; print_string " "; initialiser ini
      | Ast0.InitGccName(name,eq,ini) ->
	  ident name; mcode print_string eq; initialiser ini
      | Ast0.InitGccIndex(lb,exp,rb,eq,ini) ->
	  mcode print_string lb; expression exp; mcode print_string rb;
	  print_string " "; mcode print_string eq; print_string " ";
	  initialiser ini
      | Ast0.InitGccRange(lb,exp1,dots,exp2,rb,eq,ini) ->
	  mcode print_string lb; expression exp1; mcode print_string dots;
	  expression exp2; mcode print_string rb;
	  print_string " "; mcode print_string eq; print_string " ";
	  initialiser ini
      | Ast0.IComma(cm) -> mcode print_string cm
      | Ast0.Idots(d,Some whencode) ->
	  mcode print_string d; print_string "   WHEN != ";
	  initialiser whencode
      | Ast0.Idots(d,None) -> mcode print_string d
      | Ast0.OptIni(ini) -> print_string "?"; initialiser ini
      | Ast0.UniqueIni(ini) -> print_string "!"; initialiser ini
      | Ast0.MultiIni(ini) -> print_string "+"; initialiser ini)

let initialiser_list = dots (function _ -> ()) initialiser

(* --------------------------------------------------------------------- *)
(* Parameter *)

let rec parameterTypeDef p =
  print_context p
    (function _ ->
      match Ast0.unwrap p with
	Ast0.VoidParam(ty) -> typeC ty
      | Ast0.Param(id,ty) -> typeC ty; ident id
      | Ast0.MetaParam(name) -> mcode print_string name
      | Ast0.MetaParamList(name) -> mcode print_string name
      | Ast0.PComma(cm) -> mcode print_string cm; print_space()
      | Ast0.Pdots(dots) -> mcode print_string dots
      | Ast0.Pcircles(dots) -> mcode print_string dots
      | Ast0.OptParam(param) -> print_string "?"; parameterTypeDef param
      | Ast0.UniqueParam(param) -> print_string "!"; parameterTypeDef param)

let parameter_list = dots (function _ -> ()) parameterTypeDef

(* --------------------------------------------------------------------- *)
(* Top-level code *)

let rec statement arity s =
  print_context s
    (function _ ->
      match Ast0.unwrap s with
	Ast0.FunDecl(stg,ty,name,lp,params,rp,lbrace,body,rbrace) ->
	  print_string arity;
	  print_option (mcode U.storage) stg;
	  print_option typeC ty;
	  ident name; mcode print_string_box lp;
	  parameter_list params; close_box(); mcode print_string rp;
	  print_string " ";
	  print_string arity; mcode print_string lbrace; start_block();
	  dots force_newline (statement arity) body;
	  end_block(); print_string arity; mcode print_string rbrace
      | Ast0.Decl(decl) -> print_string arity; declaration decl
      | Ast0.Seq(lbrace,body,rbrace) ->
	  print_string arity; mcode print_string lbrace; start_block();
	  dots force_newline (statement arity) body;
	  end_block(); print_string arity; mcode print_string rbrace
      | Ast0.ExprStatement(exp,sem) ->
	  print_string arity; expression exp; mcode print_string sem
      | Ast0.IfThen(iff,lp,exp,rp,branch1,_) ->
	  print_string arity;
	  mcode print_string iff; print_string " "; mcode print_string_box lp;
	  expression exp; close_box(); mcode print_string rp; print_string " ";
	  statement arity branch1
      | Ast0.IfThenElse(iff,lp,exp,rp,branch1,els,branch2,_) ->
	  print_string arity;
	  mcode print_string iff; print_string " "; mcode print_string_box lp;
	  expression exp; close_box(); mcode print_string rp; print_string " ";
	  statement arity branch1;
	  print_string arity; mcode print_string els; print_string " ";
	  statement arity branch2
      | Ast0.While(whl,lp,exp,rp,body,_) ->
	  print_string arity;
	  mcode print_string whl; print_string " "; mcode print_string_box lp;
	  expression exp; close_box(); mcode print_string rp; print_string " ";
	  statement arity body
      | Ast0.Do(d,body,whl,lp,exp,rp,sem) ->
	  print_string arity; mcode print_string d; print_string " ";
	  statement arity body;
	  print_string arity;
	  mcode print_string whl; print_string " "; mcode print_string_box lp;
	  expression exp; close_box(); mcode print_string rp;
	  mcode print_string sem
      | Ast0.For(fr,lp,e1,sem1,e2,sem2,e3,rp,body,_) ->
	  print_string arity;
	  mcode print_string fr; mcode print_string_box lp;
	  print_option expression e1; mcode print_string sem1;
	  print_option expression e2; mcode print_string sem2;
	  print_option expression e3; close_box();
	  mcode print_string rp; print_string " "; statement arity body
      | Ast0.Break(br,sem) ->
	  print_string arity; mcode print_string br; mcode print_string sem
      | Ast0.Continue(cont,sem) ->
	  print_string arity; mcode print_string cont; mcode print_string sem
      | Ast0.Return(ret,sem) ->
	  print_string arity; mcode print_string ret; mcode print_string sem
      | Ast0.ReturnExpr(ret,exp,sem) ->
	  print_string arity; mcode print_string ret; print_string " ";
	  expression exp; mcode print_string sem
      | Ast0.MetaStmt(name) ->
	  print_string arity; mcode print_string name
      | Ast0.MetaStmtList(name) ->
	  print_string arity;  mcode print_string name
      | Ast0.Disj(_,statement_dots_list,_,_) ->
	  print_string arity;
	  print_string "\n("; force_newline();
	  print_between
	    (function _ -> print_string "\n|"; force_newline())
	    (dots force_newline (statement arity))
	    statement_dots_list;
	  print_string "\n)"
      | Ast0.Nest(starter,stmt_dots,ender,whencode) ->
	  print_string arity;
	  mcode print_string starter;
	  print_option
	    (function x ->
	      print_string "   WHEN != "; open_box 0;
	      dots force_newline (statement "") x) whencode;
	  start_block();
	  dots force_newline (statement arity) stmt_dots;
	  end_block();
	  mcode print_string ender
      | Ast0.Exp(exp) -> print_string arity; expression exp
      | Ast0.Dots(d,whn) | Ast0.Circles(d,whn) | Ast0.Stars(d,whn) ->
	  print_string arity; mcode print_string d;
	  whencode (dots force_newline (statement "")) (statement "") whn
      | Ast0.OptStm(re) -> statement "?" re
      | Ast0.UniqueStm(re) -> statement "!" re
      | Ast0.MultiStm(re) -> statement "\\+" re)

and whencode notfn alwaysfn = function
    Ast0.NoWhen -> ()
  | Ast0.WhenNot a ->
      print_string "   WHEN != "; open_box 0; notfn a; close_box()
  | Ast0.WhenAlways a ->
      print_string "   WHEN = "; open_box 0; alwaysfn a; close_box()

let statement_dots = dots (function _ -> ()) (statement "")

(* --------------------------------------------------------------------- *)
(* CPP code *)

let define_body s =
  print_context s
    (function _ ->
      match Ast0.unwrap s with
	Ast0.DMetaId(name) -> mcode print_string name
      | Ast0.Ddots(dots) -> mcode print_string dots)

let rec meta s =
  print_context s
    (function _ ->
      match Ast0.unwrap s with
	Ast0.Include(inc,s) ->
	  mcode print_string inc; print_string " "; mcode print_string s
      | Ast0.Define(def,id,body) ->
	  mcode print_string def; print_string " "; ident id;
	  print_string " "; define_body body
      | Ast0.OptMeta(m) -> print_string "?"; meta m
      | Ast0.UniqueMeta(m) -> print_string "!"; meta m
      | Ast0.MultiMeta(m) -> print_string "\\+"; meta m)

let top_level t =
  print_context t
    (function _ ->
      match Ast0.unwrap t with
	Ast0.DECL(decl) -> declaration decl
      | Ast0.META(m) -> meta m
      | Ast0.FILEINFO(old_file,new_file) ->
	  print_string "--- "; mcode print_string old_file; force_newline();
	  print_string "+++ "; mcode print_string new_file
      | Ast0.FUNCTION(stmt) -> statement "" stmt
      | Ast0.CODE(stmt_dots) ->
	  dots force_newline (statement "") stmt_dots
      | Ast0.ERRORWORDS(exps) ->
	  print_string "error words = [";
	  print_between (function _ -> print_string ", ") expression exps;
	  print_string "]"
      | Ast0.OTHER(s) ->
	  print_string "OTHER("; statement "" s; print_string ")")

let rule =
  print_between (function _ -> force_newline(); force_newline()) top_level

let unparse_anything x =
  let q = !quiet in
  quiet := true;
  (match x with
    Ast0.DotsExprTag(d) -> expression_dots d
  | Ast0.DotsParamTag(d) -> parameter_list d
  | Ast0.DotsInitTag(d) -> initialiser_list d
  | Ast0.DotsStmtTag(d) -> statement_dots d
  | Ast0.IdentTag(d) -> ident d
  | Ast0.ExprTag(d) -> expression d
  | Ast0.TypeCTag(d) -> typeC d
  | Ast0.ParamTag(d) -> parameterTypeDef d
  | Ast0.InitTag(d) -> initialiser d
  | Ast0.DeclTag(d) -> declaration d
  | Ast0.StmtTag(d) -> statement "" d
  | Ast0.MetaTag(d) -> meta d
  | Ast0.TopTag(d) -> top_level d);
  quiet := q;
  print_newline()

let unparse x =
  print_string "\n@@\n@@";
  force_newline();
  force_newline();
  rule x;
  print_newline()

let unparse_to_string x = Common.format_to_string (function _ -> unparse x)
