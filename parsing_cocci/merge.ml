(* given parsed minus code and a stream of + code, figure out where to put
the + code in the mcode of the minus code *)

(* Need to be able to find the nearest inhabited line rather than just
adding 1 or subtracting 1 to the actual line number.  This is an issue for
plus.ml as well.  This problem is dealt with by the logical line field,
which is not incremented for blank lines. *)

module Ast = Ast_cocci

(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
(* Step 1: convert minus/context code to an ordered stream of tokens *)

type position =
    Minus of Ast.info * Ast.anything list list ref
  | Context of Ast.info * Ast.anything Ast.befaft ref
  | Bad of Ast.info

let mcode = function
    _,Ast.MINUS(info,plus_stream) -> Minus (info,plus_stream)
  | _,Ast.CONTEXT(info,plus_stream) -> Context (info,plus_stream)
  | _ -> failwith "not possible 1"

let bad_mcode = function
    _,Ast.MINUS(info,plus_stream) -> Bad(info)
  | _,Ast.CONTEXT(info,plus_stream) -> Bad(info)
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
    Ast.DOTS(l) -> List.concat (List.map fn l)
  | Ast.CIRCLES(l) -> List.concat (List.map fn l)
  | Ast.STARS(l) -> List.concat (List.map fn l)

(* --------------------------------------------------------------------- *)
(* Identifier *)

let rec ident = function
    Ast.Id(name) -> [mcode name]
  | Ast.MetaId(name) -> [mcode name]
  | Ast.MetaFunc(name) -> [mcode name]
  | Ast.MetaLocalFunc(name) -> [mcode name]
  | Ast.OptIdent(id) -> ident id
  | Ast.UniqueIdent(id) -> ident id
  | Ast.MultiIdent(id) -> ident id

(* --------------------------------------------------------------------- *)
(* Expression *)

let rec expression = function
    Ast.Ident(id) -> ident id
  | Ast.Constant(const) -> [mcode const]
  | Ast.FunCall(fn,lp,args,rp) ->
      (expression fn) @ (mcode lp) :: (dots expression args) @ [mcode rp]
  | Ast.Assignment(left,op,right) ->
      (expression left) @ (mcode op) :: (expression right)
  | Ast.CondExpr(exp1,why,exp2,colon,exp3) ->
      (expression exp1) @ (mcode why) :: (get_option expression exp2) @
      (mcode colon) :: (expression exp3)
  | Ast.Postfix(exp,op) -> (expression exp) @ [mcode op]
  | Ast.Infix(exp,op) -> (mcode op) :: (expression exp)
  | Ast.Unary(exp,op) -> (mcode op) :: (expression exp)
  | Ast.Binary(left,op,right) ->
      (expression left) @ [mcode op] @ (expression right)
  | Ast.Paren(lp,exp,rp) -> (mcode lp) :: (expression exp) @ [mcode rp]
  | Ast.ArrayAccess(exp1,lb,exp2,rb) ->
      (expression exp1) @ (mcode lb) :: (expression exp2) @ [mcode rb]
  | Ast.RecordAccess(exp,pt,field) ->
      (expression exp) @ (mcode pt) :: (ident field)
  | Ast.RecordPtAccess(exp,ar,field) ->
      (expression exp) @ (mcode ar) :: (ident field)
  | Ast.Cast(lp,ty,rp,exp) ->
      (mcode lp) :: (typeC ty) @ (mcode rp) :: (expression exp)
  | Ast.MetaConst(name,ty) -> [mcode name]
  | Ast.MetaErr(name) -> [mcode name]
  | Ast.MetaExpr(name,ty) -> [mcode name]
  | Ast.MetaExprList(name) -> [mcode name]
  | Ast.EComma(cm) -> [mcode cm]
  | Ast.DisjExpr(expr_dots_list) ->
      List.concat (List.map expression expr_dots_list)
  | Ast.NestExpr(expr_dots) -> dots expression expr_dots
  | Ast.Edots(dots,whencode) | Ast.Ecircles(dots,whencode)
  | Ast.Estars(dots,whencode) ->
      (bad_mcode dots) ::
      (get_option (function x -> make_bad(expression x)) whencode)
  | Ast.OptExp(exp) -> expression exp
  | Ast.UniqueExp(exp) -> expression exp
  | Ast.MultiExp(exp) -> expression exp

(* --------------------------------------------------------------------- *)
(* Types *)

and typeC ty =
  match ty with
    Ast.BaseType(ty,Some sign) -> [mcode sign;mcode ty]
  | Ast.BaseType(ty,None) -> [mcode ty]
  | Ast.Pointer(ty,star) -> (typeC ty) @ [mcode star]
  | Ast.Array(ty,lb,size,rb) ->
      (typeC ty) @ (mcode lb) :: (get_option expression size) @ [mcode rb]
  | Ast.StructUnionName(name,kind) -> [mcode name;mcode kind]
  | Ast.TypeName(name) -> [mcode name]
  | Ast.MetaType(name) -> [mcode name]
  | Ast.OptType(ty) -> typeC ty
  | Ast.UniqueType(ty) -> typeC ty
  | Ast.MultiType(ty) -> typeC ty

(* --------------------------------------------------------------------- *)
(* Variable declaration *)
(* Even if the Cocci program specifies a list of declarations, they are
   split out into multiple declarations of a single variable each. *)

let rec declaration = function
    Ast.Init(ty,id,eq,exp,sem) ->
      (typeC ty) @ (ident id) @ (mcode eq) :: (expression exp) @ [mcode sem]
  | Ast.UnInit(ty,id,sem) ->
      (typeC ty) @ (ident id) @ [mcode sem]
  | Ast.OptDecl(decl) -> declaration decl
  | Ast.UniqueDecl(decl) -> declaration decl
  | Ast.MultiDecl(decl) -> declaration decl

(* --------------------------------------------------------------------- *)
(* Parameter *)

let rec parameterTypeDef = function
    Ast.VoidParam(ty) -> typeC ty
  | Ast.Param(id,None,ty) -> (typeC ty) @ (ident id)
  | Ast.Param(id,Some vs,ty) -> (typeC ty) @ (mcode vs) :: (ident id)
  | Ast.MetaParam(name) -> [mcode name]
  | Ast.MetaParamList(name) -> [mcode name]
  | Ast.PComma(cm) -> [mcode cm]
  | Ast.Pdots(dots) -> [bad_mcode dots]
  | Ast.Pcircles(dots) -> [bad_mcode dots]
  | Ast.OptParam(param) -> parameterTypeDef param
  | Ast.UniqueParam(param) -> parameterTypeDef param

let parameter_list = dots parameterTypeDef

(* --------------------------------------------------------------------- *)
(* Top-level code *)

let rec rule_elem = function
    Ast.FunDecl(stg,name,lp,params,rp) ->
      (get_option (function x -> [mcode x]) stg) @ (ident name) @ (mcode lp) ::
      (parameter_list params) @ [mcode rp]
  | Ast.Decl(decl) -> declaration decl
  | Ast.SeqStart(brace) -> [mcode brace]
  | Ast.SeqEnd(brace) -> [mcode brace]
  | Ast.ExprStatement(exp,sem) -> (expression exp) @ [mcode sem]
  | Ast.IfHeader(iff,lp,exp,rp) ->
      [mcode iff; mcode lp] @ (expression exp) @ [mcode rp]
  | Ast.Else(els) -> [mcode els]
  | Ast.WhileHeader(whl,lp,exp,rp) ->
      [mcode whl; mcode lp] @ (expression exp) @ [mcode rp]
  | Ast.Do(d) -> [mcode d]
  | Ast.WhileTail(whl,lp,exp,rp,sem) ->
      [mcode whl; mcode lp] @ (expression exp) @ [mcode rp;mcode sem]
  | Ast.ForHeader(fr,lp,e1,sem1,e2,sem2,e3,rp) ->
      [mcode fr; mcode lp] @ (get_option expression e1) @ (mcode sem1) ::
      (get_option expression e2) @ (mcode sem2) :: (get_option expression e3) @
      [mcode rp]
  | Ast.Return(ret,sem) -> [mcode ret; mcode sem]
  | Ast.ReturnExpr(ret,exp,sem) ->
      (mcode ret) :: (expression exp) @ [mcode sem]
  | Ast.MetaStmt(name) -> [mcode name]
  | Ast.MetaStmtList(name) -> [mcode name]
  | Ast.Disj(rule_elem_dots_list) ->
      List.concat (List.map (dots rule_elem) rule_elem_dots_list)
  | Ast.Nest(rule_elem_dots) -> dots rule_elem rule_elem_dots
  | Ast.Exp(exp) -> expression exp
  | Ast.Dots(d,whencode) | Ast.Circles(d,whencode) | Ast.Stars(d,whencode) ->
      (bad_mcode d) ::
      (get_option (function x -> make_bad(dots rule_elem x)) whencode)
  | Ast.OptRuleElem(re) -> List.concat (List.map rule_elem re)
  | Ast.UniqueRuleElem(re) -> List.concat (List.map rule_elem re)
  | Ast.MultiRuleElem(re) -> List.concat (List.map rule_elem re)

let top_level = function
    Ast.DECL(decl) -> declaration decl
  | Ast.INCLUDE(inc,name) -> [mcode inc;mcode name]
  | Ast.FILEINFO(old_file,new_file) -> [bad_mcode old_file; bad_mcode new_file]
  | Ast.FUNCTION(rule_elem_dots) -> dots rule_elem rule_elem_dots
  | Ast.CODE(rule_elem_dots) -> dots rule_elem rule_elem_dots
  | Ast.ERRORWORDS(exps) -> make_bad (List.concat (List.map expression exps))

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
