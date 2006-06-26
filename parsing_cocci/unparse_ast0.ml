open Format
module Ast0 = Ast0_cocci
module Ast = Ast_cocci
module U = Unparse_cocci

let start_block str =
  force_newline(); print_string "  "; open_box 0

let end_block str =
  close_box(); force_newline ()

let print_option fn = function
    None -> ()
  | Some x -> fn x

let rec print_between between fn = function
    [] -> ()
  | [x] -> fn x
  | x::xs -> fn x; between(); print_between between fn xs

(* --------------------------------------------------------------------- *)
(* Modified code *)

let mcodekind brackets fn x = function
    Ast0.MINUS(plus_stream) ->
      let (lb,rb) =
	match brackets with Some _ -> ("[","]") | None -> ("","") in
      let (plus_stream,_) = !plus_stream in
      print_string "-"; print_string lb; fn x; print_string rb;
      U.print_anything ">>> " plus_stream
  | Ast0.CONTEXT(plus_streams) ->
      let (lb,rb) =
	match brackets with
	  Some x -> ("[",("]^"^(string_of_int x))) | None -> ("","") in
      let (plus_streams,_,_) = !plus_streams in
      U.print_around (function x -> print_string lb; fn x; print_string rb)
	x plus_streams
  | Ast0.PLUS -> fn x
  | Ast0.MIXED -> fn x

let mcode fn (x,_,_,mc) = mcodekind None fn x mc

let print_context (_,info,i,mc) fn = mcodekind (Some !i) fn () !mc

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
	  expression left; print_string " "; mcode U.binaryOp op;
	  print_string " "; expression right
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
      | Ast0.MetaConst(name,None) -> mcode print_string name
      | Ast0.MetaConst(name,Some ty) ->
	  mcode print_string name; print_string "/* ";
	  print_between (function _ -> print_string ", ") typeC ty;
	  print_string "*/"
      | Ast0.MetaErr(name) -> mcode print_string name
      | Ast0.MetaExpr(name,None) -> mcode print_string name
      | Ast0.MetaExpr(name,Some ty) ->
	  mcode print_string name; print_string "/*";
	  print_between (function _ -> print_string ", ") typeC ty;
	  print_string "*/"
      | Ast0.MetaExprList(name) -> mcode print_string name
      | Ast0.EComma(cm) -> mcode print_string cm; print_space()
      | Ast0.DisjExpr(_,exp_list,_) ->
	  print_string "\n("; force_newline();
	  print_between
	    (function _ -> print_string "\n|"; force_newline())
	    expression exp_list;
	  print_string "\n)"
      | Ast0.NestExpr(starter,expr_dots,ender) ->
	  mcode print_string starter;
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
      | Ast0.StructUnionName(name,kind) ->
	  mcode U.structUnion kind; mcode print_string name; print_string " "
      | Ast0.TypeName(name)-> mcode print_string name; print_string " "
      | Ast0.MetaType(name)-> mcode print_string name; print_string " "
      | Ast0.OptType(ty) -> print_string "?"; typeC ty
      | Ast0.UniqueType(ty) -> print_string "!"; typeC ty
      | Ast0.MultiType(ty) -> print_string "\\+"; typeC ty)

(* --------------------------------------------------------------------- *)
(* Variable declaration *)
(* Even if the Cocci program specifies a list of declarations, they are
   split out into multiple declarations of a single variable each. *)

let rec declaration d =
  print_context d
    (function _ ->
      match Ast0.unwrap d with
	Ast0.Init(ty,id,eq,exp,sem) ->
	  typeC ty; ident id; mcode print_string eq; expression exp;
	  mcode print_string sem
      | Ast0.UnInit(ty,id,sem) -> typeC ty; ident id; mcode print_string sem
      | Ast0.OptDecl(decl) -> print_string "?"; declaration decl
      | Ast0.UniqueDecl(decl) -> print_string "!"; declaration decl
      | Ast0.MultiDecl(decl) -> print_string "\\+"; declaration decl)

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
	Ast0.FunDecl(stg,name,lp,params,rp,lbrace,body,rbrace) ->
	  print_string arity;
	  print_option (mcode U.storage) stg;
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
      | Ast0.IfThen(iff,lp,exp,rp,branch1) ->
	  print_string arity;
	  mcode print_string iff; print_string " "; mcode print_string_box lp;
	  expression exp; close_box(); mcode print_string rp; print_string " ";
	  statement arity branch1
      | Ast0.IfThenElse(iff,lp,exp,rp,branch1,els,branch2) ->
	  print_string arity;
	  mcode print_string iff; print_string " "; mcode print_string_box lp;
	  expression exp; close_box(); mcode print_string rp; print_string " ";
	  statement arity branch1;
	  print_string arity; mcode print_string els; print_string " ";
	  statement arity branch2
      | Ast0.While(whl,lp,exp,rp,body) ->
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
      | Ast0.For(fr,lp,e1,sem1,e2,sem2,e3,rp,body) ->
	  print_string arity;
	  mcode print_string fr; mcode print_string_box lp;
	  print_option expression e1; mcode print_string sem1;
	  print_option expression e2; mcode print_string sem2;
	  print_option expression e3; close_box();
	  mcode print_string rp; print_string " "; statement arity body
      | Ast0.Return(ret,sem) ->
	  print_string arity; mcode print_string ret; mcode print_string sem
      | Ast0.ReturnExpr(ret,exp,sem) ->
	  print_string arity; mcode print_string ret; print_string " ";
	  expression exp; mcode print_string sem
      | Ast0.MetaStmt(name) ->
	  print_string arity; mcode print_string name
      | Ast0.MetaStmtList(name) ->
	  print_string arity;  mcode print_string name
      | Ast0.Disj(_,statement_dots_list,_) ->
	  print_string arity;
	  print_string "\n("; force_newline();
	  print_between
	    (function _ -> print_string "\n|"; force_newline())
	    (dots force_newline (statement arity))
	    statement_dots_list;
	  print_string "\n)"
      | Ast0.Nest(starter,stmt_dots,ender) ->
	  print_string arity;
	  mcode print_string starter;
	  start_block();
	  dots force_newline (statement arity) stmt_dots;
	  end_block();
	  mcode print_string ender
      | Ast0.Exp(exp) -> print_string arity; expression exp
      | Ast0.Dots(d,whencode) | Ast0.Circles(d,whencode)
      | Ast0.Stars(d,whencode) ->
	  print_string arity; mcode print_string d;
	  print_option
	    (function x ->
	      print_string "   WHEN != "; open_box 0;
	      dots force_newline (statement "") x) whencode;
	  close_box()
      | Ast0.OptStm(re) -> statement "?" re
      | Ast0.UniqueStm(re) -> statement "!" re
      | Ast0.MultiStm(re) -> statement "\\+" re)

let statement_dots = dots (function _ -> ()) (statement "")

let top_level t =
  print_context t
    (function _ ->
      match Ast0.unwrap t with
	Ast0.DECL(decl) -> declaration decl
      | Ast0.INCLUDE(inc,s) ->
	  mcode print_string inc; print_string " "; mcode print_string s
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
      | Ast0.OTHER(_) -> failwith "unexpected code")

let rule =
  print_between (function _ -> force_newline(); force_newline()) top_level

let unparse x =
  print_string "\n@@\n@@";
  force_newline();
  force_newline();
  rule x;
  force_newline();
  print_flush()

let unparse_to_string x =
  let o = open_out "/tmp/out" in
  set_formatter_out_channel o;
  let _ = unparse x in
  print_flush();
  set_formatter_out_channel stdout;
  close_out o;
  let i = open_in "/tmp/out" in
  let lines = ref [] in
  let rec loop _ =
    let cur = input_line i in
    lines := cur :: !lines;
    loop() in
  (try loop() with End_of_file -> ());
  String.concat "\n" (List.rev !lines)
