(* given parsed minus code and a stream of + code, figure out where to put
the + code in the mcode of the minus code *)

(* Need to be able to find the nearest inhabited line rather than just
adding 1 or subtracting 1 to the actual line number.  This is an issue for
plus.ml as well.  This problem is dealt with by the logical line field,
which is not incremented for blank lines. *)

module Ast = Ast_cocci
module Ast0 = Ast0_cocci

(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
(* Step 1: convert minus/context code to an ordered stream of tokens *)

type position =
    Minus of Ast.info * Ast.anything list list ref
  | Context of Ast.info * Ast.anything Ast.befaft ref
  | Bad of Ast.info

let mcode = function
    (_,_,Ast.MINUS(info,plus_stream)) -> Minus (info,plus_stream)
  | (_,_,Ast.CONTEXT(info,plus_stream)) -> Context (info,plus_stream)
  | _ -> failwith "not possible 1"

let bad_mcode = function
    (_,_,Ast.MINUS(info,plus_stream)) -> Bad(info)
  | (_,_,Ast.CONTEXT(info,plus_stream)) -> Bad(info)
  | _ -> failwith "not possible 2"

let make_bad l =
  List.map
    (function
	Minus(info,plus_stream) -> Bad(info)
      |	Context(info,plus_stream) -> Bad(info)
      |	x -> x)
    l

let get_option fn x =
  match x with
    None -> []
  | Some e -> fn e

(* --------------------------------------------------------------------- *)
(* Dots *)

let dots fn = function
    Ast0.DOTS(l) -> List.concat (List.map fn l)
  | Ast0.CIRCLES(l) -> List.concat (List.map fn l)
  | Ast0.STARS(l) -> List.concat (List.map fn l)

(* --------------------------------------------------------------------- *)
(* Identifier *)

let rec ident = function
    Ast0.Id(name) -> [mcode name]
  | Ast0.MetaId(name) -> [mcode name]
  | Ast0.MetaFunc(name) -> [mcode name]
  | Ast0.MetaLocalFunc(name) -> [mcode name]
  | Ast0.OptIdent(id) -> ident id
  | Ast0.UniqueIdent(id) -> ident id
  | Ast0.MultiIdent(id) -> ident id

(* --------------------------------------------------------------------- *)
(* Expression *)

let rec expression = function
    Ast0.Ident(id) -> ident id
  | Ast0.Constant(const) -> [mcode const]
  | Ast0.FunCall(fn,lp,args,rp) ->
      (expression fn) @ (mcode lp) :: (dots expression args) @ [mcode rp]
  | Ast0.Assignment(left,op,right) ->
      (expression left) @ (mcode op) :: (expression right)
  | Ast0.CondExpr(exp1,why,exp2,colon,exp3) ->
      (expression exp1) @ (mcode why) :: (get_option expression exp2) @
      (mcode colon) :: (expression exp3)
  | Ast0.Postfix(exp,op) -> (expression exp) @ [mcode op]
  | Ast0.Infix(exp,op) -> (mcode op) :: (expression exp)
  | Ast0.Unary(exp,op) -> (mcode op) :: (expression exp)
  | Ast0.Binary(left,op,right) ->
      (expression left) @ [mcode op] @ (expression right)
  | Ast0.Paren(lp,exp,rp) -> (mcode lp) :: (expression exp) @ [mcode rp]
  | Ast0.ArrayAccess(exp1,lb,exp2,rb) ->
      (expression exp1) @ (mcode lb) :: (expression exp2) @ [mcode rb]
  | Ast0.RecordAccess(exp,pt,field) ->
      (expression exp) @ (mcode pt) :: (ident field)
  | Ast0.RecordPtAccess(exp,ar,field) ->
      (expression exp) @ (mcode ar) :: (ident field)
  | Ast0.Cast(lp,ty,rp,exp) ->
      (mcode lp) :: (typeC ty) @ (mcode rp) :: (expression exp)
  | Ast0.MetaConst(name,ty) -> [mcode name]
  | Ast0.MetaErr(name) -> [mcode name]
  | Ast0.MetaExpr(name,ty) -> [mcode name]
  | Ast0.MetaExprList(name) -> [mcode name]
  | Ast0.EComma(cm) -> [mcode cm]
  | Ast0.DisjExpr(expr_dots_list) ->
      List.concat (List.map expression expr_dots_list)
  | Ast0.NestExpr(expr_dots) -> dots expression expr_dots
  | Ast0.Edots(dots,whencode) | Ast0.Ecircles(dots,whencode)
  | Ast0.Estars(dots,whencode) ->
      (bad_mcode dots) ::
      (get_option (function x -> make_bad(expression x)) whencode)
  | Ast0.OptExp(exp) -> expression exp
  | Ast0.UniqueExp(exp) -> expression exp
  | Ast0.MultiExp(exp) -> expression exp

(* --------------------------------------------------------------------- *)
(* Types *)

and typeC ty =
  match ty with
    Ast0.BaseType(ty,Some sign) -> [mcode sign;mcode ty]
  | Ast0.BaseType(ty,None) -> [mcode ty]
  | Ast0.Pointer(ty,star) -> (typeC ty) @ [mcode star]
  | Ast0.Array(ty,lb,size,rb) ->
      (typeC ty) @ (mcode lb) :: (get_option expression size) @ [mcode rb]
  | Ast0.StructUnionName(name,kind) -> [mcode name;mcode kind]
  | Ast0.TypeName(name) -> [mcode name]
  | Ast0.MetaType(name) -> [mcode name]
  | Ast0.OptType(ty) -> typeC ty
  | Ast0.UniqueType(ty) -> typeC ty
  | Ast0.MultiType(ty) -> typeC ty

(* --------------------------------------------------------------------- *)
(* Variable declaration *)
(* Even if the Cocci program specifies a list of declarations, they are
   split out into multiple declarations of a single variable each. *)

let rec declaration = function
    Ast0.Init(ty,id,eq,exp,sem) ->
      (typeC ty) @ (ident id) @ (mcode eq) :: (expression exp) @ [mcode sem]
  | Ast0.UnInit(ty,id,sem) ->
      (typeC ty) @ (ident id) @ [mcode sem]
  | Ast0.OptDecl(decl) -> declaration decl
  | Ast0.UniqueDecl(decl) -> declaration decl
  | Ast0.MultiDecl(decl) -> declaration decl

(* --------------------------------------------------------------------- *)
(* Parameter *)

let rec parameterTypeDef = function
    Ast0.VoidParam(ty) -> typeC ty
  | Ast0.Param(id,None,ty) -> (typeC ty) @ (ident id)
  | Ast0.Param(id,Some vs,ty) -> (typeC ty) @ (mcode vs) :: (ident id)
  | Ast0.MetaParam(name) -> [mcode name]
  | Ast0.MetaParamList(name) -> [mcode name]
  | Ast0.PComma(cm) -> [mcode cm]
  | Ast0.Pdots(dots) -> [bad_mcode dots]
  | Ast0.Pcircles(dots) -> [bad_mcode dots]
  | Ast0.OptParam(param) -> parameterTypeDef param
  | Ast0.UniqueParam(param) -> parameterTypeDef param

let parameter_list = dots parameterTypeDef

(* --------------------------------------------------------------------- *)
(* Top-level code *)

let rec statement = function
    Ast0.FunDecl(stg,name,lp,params,rp,lbrace,body,rbrace) ->
      (get_option (function x -> [mcode x]) stg) @ (ident name) @ (mcode lp) ::
      (parameter_list params) @ [mcode rp; mcode lbrace] @
      (dots statement body) @ [mcode lbrace]
  | Ast0.Decl(decl) -> declaration decl
  | Ast0.Seq(lbrace,body,rbrace) ->
      [mcode lbrace] @ (dots statement body) @ [mcode rbrace]
  | Ast0.ExprStatement(exp,sem) -> (expression exp) @ [mcode sem]
  | Ast0.IfThen(iff,lp,exp,rp,branch1) ->
      [mcode iff; mcode lp] @ (expression exp) @ [mcode rp]
      @ (statement branch1)
  | Ast0.IfThenElse(iff,lp,exp,rp,branch1,els,branch2) ->
      [mcode iff; mcode lp] @ (expression exp) @ [mcode rp]
      @ (statement branch1) @ [mcode els] @ (statement branch2)
  | Ast0.While(whl,lp,exp,rp,body) ->
      [mcode whl; mcode lp] @ (expression exp) @ [mcode rp] @ (statement body)
  | Ast0.Do(d,body,whl,lp,exp,rp,sem) ->
      [mcode d] @ (statement body) @
	[mcode whl; mcode lp] @ (expression exp) @ [mcode rp;mcode sem]
  | Ast0.For(fr,lp,e1,sem1,e2,sem2,e3,rp,body) ->
      [mcode fr; mcode lp] @ (get_option expression e1) @ (mcode sem1) ::
      (get_option expression e2) @ (mcode sem2) :: (get_option expression e3) @
      [mcode rp] @ (statement body)
  | Ast0.Return(ret,sem) -> [mcode ret; mcode sem]
  | Ast0.ReturnExpr(ret,exp,sem) ->
      (mcode ret) :: (expression exp) @ [mcode sem]
  | Ast0.MetaStmt(name) -> [mcode name]
  | Ast0.MetaStmtList(name) -> [mcode name]
  | Ast0.Disj(statement_dots_list) ->
      List.concat (List.map (dots statement) statement_dots_list)
  | Ast0.Nest(statement_dots) -> dots statement statement_dots
  | Ast0.Exp(exp) -> expression exp
  | Ast0.Dots(d,whencode) | Ast0.Circles(d,whencode)
  | Ast0.Stars(d,whencode) ->
      (bad_mcode d) ::
      (get_option (function x -> make_bad(dots statement x)) whencode)
  | Ast0.OptStm(re) -> statement re
  | Ast0.UniqueStm(re) -> statement re
  | Ast0.MultiStm(re) -> statement re

let top_level = function
    Ast0.DECL(decl) -> declaration decl
  | Ast0.INCLUDE(inc,name) -> [mcode inc;mcode name]
  | Ast0.FILEINFO(old_file,new_file) -> [bad_mcode old_file; bad_mcode new_file]
  | Ast0.FUNCTION(statement_dots) -> statement statement_dots
  | Ast0.CODE(statement_dots) -> dots statement statement_dots
  | Ast0.ERRORWORDS(exps) -> make_bad (List.concat (List.map expression exps))
  | Ast0.OTHER(_) -> failwith "unexpected code"

let rule code = List.concat (List.map top_level code)

(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
(* Step 2: merge the plus stream with the minus/context tokens *)

(* Mcode *)

let get_start l =
  let (_,_,_,start,_) = List.hd (List.hd l) in
  start

let get_finish l =
  let (_,_,_,_,finish) = List.hd (List.rev (List.hd (List.rev l))) in
  finish

let get_real_start l =
  let (_,start,_,_,_) = List.hd (List.hd l) in
  start

let get_real_finish l =
  let (_,_,finish,_,_) = List.hd (List.rev (List.hd (List.rev l))) in
  finish

let get_minus_next_line mline = function
    [] -> mline + 1
  | Bad(info)::xs -> info.Ast.logical_line
  | Minus(info,_)::xs -> info.Ast.logical_line
  | Context(info,_)::xs -> info.Ast.logical_line

let drop_lines l = List.map (List.map (function (x,_,_,_,_) -> x)) l

let rec merge minus_stream plus_stream =
  match (minus_stream,plus_stream) with
    (_,[]) -> ()
  | ([],_) -> failwith "minus stream ran out before plus stream"
  | (Bad(info)::minus_stream,plus::plus_stream) ->
      let pfinish = get_finish plus in
      if info.Ast.logical_line > pfinish
      then failwith "error in merge"
      else merge minus_stream (plus::plus_stream)
  | (((Minus(info,cell)::minus_stream) as all_minus),plus::plus_stream) ->
      let mline = info.Ast.logical_line in
      let mnext_line = get_minus_next_line mline minus_stream in
      let pstart = get_start plus in
      let pfinish = get_finish plus in
      if pstart < mline && pfinish > mline
      then (cell := (drop_lines plus) @ !cell; merge minus_stream plus_stream)
      else if pfinish + 1 = mline
      then (cell := (drop_lines plus) @ !cell; merge all_minus plus_stream)
      else if not(mline = mnext_line) && (pstart - 1 = mline)
      then (cell := !cell @ (drop_lines plus); merge minus_stream plus_stream)
      else if pfinish < mline
      then
	Printf.printf "failed to merge + code between lines %d and %d"
	  (get_real_start plus) (get_real_finish plus)
      else merge minus_stream (plus::plus_stream)
  | (((Context(info,cell)::minus_stream) as all_minus),plus::plus_stream) ->
      let mline = info.Ast.logical_line in
      let mnext_line = get_minus_next_line mline minus_stream in
      let pstart = get_start plus in
      let pfinish = get_finish plus in
      if pfinish + 1 = mline
      then (cell := Ast.BEFORE (drop_lines plus); merge all_minus plus_stream)
      else if not(mline = mnext_line) && (pstart - 1 = mline)
      then
	begin
	  (match !cell with
	    Ast.BEFORE x -> cell := Ast.BEFOREAFTER (x,drop_lines plus)
	  | _ -> cell := Ast.AFTER (drop_lines plus));
	  merge minus_stream plus_stream
	end
      else if pfinish < mline
      then
	Printf.printf "failed to merge + code between lines %d and %d"
	  (get_real_start plus) (get_real_finish plus)
      else merge minus_stream (plus::plus_stream)

(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
(* Entry point *)

let do_merge minus plus_stream =
  let minus_tokens = rule minus in
  merge minus_tokens plus_stream
