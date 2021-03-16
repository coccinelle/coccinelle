(*
 * Copyright (C) 2012, INRIA.
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

let error name e str =
  failwith
    (Printf.sprintf "%s: line %d %s\n" (snd (Ast.unwrap_mcode name))
       (Ast.get_line e) str)

let get_string_info = function
    Ast.Noindent s | Ast.Indent s | Ast.Space s -> s

let unknown = -1

let do_all
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

let start_block () = (*indent();*) force_newline() in
let end_block = function [] -> () | _ -> (*unindent true;*) force_newline () in
let print_string_box s = print_string s in

let print_option = Common.do_option in
let print_option_space fn = function
    None -> ()
  | Some x -> fn x; pr_space() in
let print_option_prespace fn = function
    None -> ()
  | Some x -> pr_space(); fn x in
let print_between = Common.print_between in

let rec param_print_between between fn = function
  | [] -> ()
  | [x] -> fn x
  | x::xs -> fn x; between x; param_print_between between fn xs in

let rec param_print_before_and_after before fn = function
  | [] -> before ()
  | x::xs -> before (); fn x; param_print_before_and_after before fn xs in


let outdent _ = () (* should go to leftmost col, does nothing now *) in

let pretty_print_c =
  Pretty_print_c.mk_pretty_printers pr_celem pr_cspace
    force_newline (fun _ -> ()) outdent (function _ -> ()) in

(* --------------------------------------------------------------------- *)
(* Only for make_hrule, print plus code, unbound metavariables *)

(* avoid polyvariance problems *)
let anything : (Ast.anything -> unit) ref = ref (function _ -> ()) in

let rec print_anything = function
    [] -> ()
  | stream ->
      start_block();
      print_between force_newline print_anything_list stream;
      end_block stream

and print_anything_list = function
    [] -> ()
  | [x] -> !anything x
  | bef::((aft::_) as rest) ->
      !anything bef;
      let space =
	(match bef with
	  Ast.Rule_elemTag(_) | Ast.AssignOpTag(_) | Ast.BinaryOpTag(_)
	| Ast.ArithOpTag(_) | Ast.LogicalOpTag(_)
	| Ast.Token("if",_) | Ast.Token("while",_) -> true | _ -> false) ||
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
	  print_text "@"; print_meta name
      | Ast.MetaCom(name,_,_,_) ->
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
	      |	Some lb when line = lb ->
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
      |	Some lb when lb = info.Ast.line -> ()
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

let lookup_metavar name =
  let ((_,b) as s,info,mc,pos) = name in
  let line = info.Ast.line in
  let lcol = info.Ast.column in
  let rcol = if lcol = unknown then unknown else lcol + (String.length b) in
  let res = Common.optionise (fun () -> List.assoc s env) in
  (res,b,line,lcol,rcol) in

let handle_metavar name fn =
  let (res,name_string,line,lcol,rcol) = lookup_metavar name in
  match res with
    None ->
      if generating
      then mcode (function _ -> print_string name_string) name
      else
	failwith
	  (Printf.sprintf "SP line %d: Not found a value in env for: %s"
	     line name_string)
  | Some e  ->
      pr_barrier line lcol;
      (* call mcode to preserve the -+ annotation *)
      (* This used to only call mcode if generating was true.  Not clear
	 how that could work. *)
      mcode (fun _ _ _ -> fn e) name;
      pr_barrier line rcol
in
(* --------------------------------------------------------------------- *)
let dots between fn d = param_print_between between fn (Ast.unwrap d) in

let dots_before_and_after before fn d =
  param_print_before_and_after before fn (Ast.unwrap d) in

let nest_dots starter ender fn f d =
  mcode print_string starter;
  f(); start_block();
  let l = Ast.unwrap d in
  print_between force_newline fn l; end_block l;
  mcode print_string ender
in

let print_disj_list fn l sep =
  print_text "\n(\n";
  print_between (function _ -> print_text ("\n"^sep^"\n")) fn l;
  print_text "\n)\n" in

(* --------------------------------------------------------------------- *)
(* Identifier *)

let rec ident i =
  match Ast.unwrap i with
      Ast.Id(name) -> mcode print_string name
    | Ast.MetaId(name,_,_,_) ->
	handle_metavar name (function
			       | (Ast_c.MetaIdVal id) -> print_text id
			       | _ -> error name i "identifier value expected"
			    )
    | Ast.MetaFunc(name,_,_,_) ->
	handle_metavar name (function
			       | (Ast_c.MetaFuncVal id) -> print_text id
			       | _ ->
				   error name i "function name value expected"
			    )
    | Ast.MetaLocalFunc(name,_,_,_) ->
	handle_metavar name (function
			       | (Ast_c.MetaLocalFuncVal id) -> print_text id
			       | _ ->
				   error name i
				     "local function name value expected"
			    )

    | Ast.AsIdent(id,asid) -> ident id

    | Ast.DisjId(id_list) ->
	if generating
	then print_disj_list ident id_list "|"
	else raise CantBeInPlus
    | Ast.ConjId(id_list) ->
	if generating
	then print_disj_list ident id_list "&"
	else raise CantBeInPlus
    | Ast.OptIdent(_) -> raise CantBeInPlus in


(* --------------------------------------------------------------------- *)
(* Expression *)

let rec expression e =
  let top = 0 in
  let assign = 1 in
  let cond = 2 in
  let log_or = 3 in
  let log_and = 4 in
  let bit_or = 5 in
  let bit_xor = 6 in
  let bit_and = 7 in
  let equal = 8 in
  let relat = 9 in
  let shift = 10 in
  let addit = 11 in
  let mulit = 12 in
  let cast = 13 in
  let unary = 14 in
  let postfix = 15 in
  let primary = 16 in
  let minprio = top in
  let left_prec_of_c op =
    let left_prec_of_arith = function
	Ast_c.Plus | Ast_c.Minus -> addit
      | Ast_c.Mul | Ast_c.Div | Ast_c.Mod -> mulit
      | Ast_c.Min | Ast_c.Max -> relat
      | Ast_c.DecLeft | Ast_c.DecRight -> shift
      | Ast_c.And -> bit_and
      | Ast_c.Or -> bit_or
      | Ast_c.Xor -> bit_xor in
    let left_prec_of_logical = function
	Ast_c.Inf | Ast_c.Sup | Ast_c.InfEq | Ast_c.SupEq -> relat
      | Ast_c.Eq | Ast_c.NotEq -> equal
      | Ast_c.AndLog -> log_and
      | Ast_c.OrLog -> log_or in
    match Ast_c.unwrap op with
      | Ast_c.Arith op' -> left_prec_of_arith op'
      | Ast_c.Logical op' -> left_prec_of_logical op' in
  let right_prec_of_c op =
    let right_prec_of_arith = function
	Ast_c.Plus | Ast_c.Minus -> mulit
      | Ast_c.Mul | Ast_c.Div | Ast_c.Mod -> cast
      | Ast_c.Min | Ast_c.Max -> shift
      | Ast_c.DecLeft | Ast_c.DecRight -> addit
      | Ast_c.And -> equal
      | Ast_c.Or -> bit_xor
      | Ast_c.Xor -> bit_and in
    let right_prec_of_logical = function
	Ast_c.Inf -> shift
      | Ast_c.Sup -> shift
      | Ast_c.InfEq -> shift
      | Ast_c.SupEq -> shift
      | Ast_c.Eq -> relat
      | Ast_c.NotEq -> relat
      | Ast_c.AndLog -> bit_or
      | Ast_c.OrLog -> log_and in
    match Ast_c.unwrap op with
      Ast_c.Arith op' -> right_prec_of_arith op'
    | Ast_c.Logical op' -> right_prec_of_logical op' in
  let left_prec_of op =
    let left_prec_of_arith op =
      match Ast.unwrap_mcode op with
	Ast.Plus | Ast.Minus -> addit
      | Ast.Mul | Ast.Div | Ast.Mod -> mulit
      | Ast.Min | Ast.Max -> relat
      | Ast.DecLeft | Ast.DecRight -> shift
      | Ast.And -> bit_and
      | Ast.Or -> bit_or
      | Ast.Xor -> bit_xor in
    let left_prec_of_logical op =
      match Ast.unwrap_mcode op with
	Ast.Inf | Ast.Sup | Ast.InfEq | Ast.SupEq -> relat
      | Ast.Eq | Ast.NotEq -> equal
      | Ast.AndLog -> log_and
      | Ast.OrLog -> log_or in
    match Ast.unwrap op with
      Ast.Arith op' -> left_prec_of_arith op'
    | Ast.Logical op' -> left_prec_of_logical op'
    | Ast.MetaBinary (mv,_,_,_) ->
	let (res,name_string,line,lcol,rcol) = lookup_metavar mv in
	(match res with
	  None ->
	    if generating then minprio else failwith "unbound MetaBinary"
	| Some (Ast_c.MetaBinaryOpVal bop) -> left_prec_of_c bop
	| Some _ -> failwith "bad MetaBinary value") in
  let right_prec_of op =
    let right_prec_of_arith op =
      match Ast.unwrap_mcode op with
	Ast.Plus | Ast.Minus -> mulit
      | Ast.Mul | Ast.Div | Ast.Mod -> cast
      | Ast.Min | Ast.Max -> shift
      | Ast.DecLeft | Ast.DecRight -> addit
      | Ast.And -> equal
      | Ast.Or -> bit_xor
      | Ast.Xor -> bit_and in
    let right_prec_of_logical op =
      match Ast.unwrap_mcode op with
	Ast.Inf -> shift
      | Ast.Sup -> shift
      | Ast.InfEq -> shift
      | Ast.SupEq -> shift
      | Ast.Eq -> relat
      | Ast.NotEq -> relat
      | Ast.AndLog -> bit_or
      | Ast.OrLog -> log_and in
    match Ast.unwrap op with
      Ast.Arith op' -> right_prec_of_arith op'
    | Ast.Logical op' -> right_prec_of_logical op'
    | Ast.MetaBinary (mv,_,_,_) ->
	let (res,name_string,line,lcol,rcol) = lookup_metavar mv in
	(match res with
	  None ->
	    if generating then minprio else failwith "unbound MetaBinary"
	| Some (Ast_c.MetaBinaryOpVal bop) -> right_prec_of_c bop
	| Some _ -> failwith "bad MetaBinary value") in
  let prec_of_c = function
    | Ast_c.Ident (ident) -> primary
    | Ast_c.Constant (c) -> primary
    | Ast_c.StringConstant (c,os,w) -> primary
    | Ast_c.FunCall  (e, es) -> postfix
    | Ast_c.CondExpr (e1, e2, e3) -> cond
    | Ast_c.Sequence (e1, e2) -> top
    | Ast_c.Assignment (e1, op, e2) -> assign
    | Ast_c.Postfix(e, op) -> postfix
    | Ast_c.Infix  (e, op) -> unary
    | Ast_c.Unary  (e, op) -> unary
    | Ast_c.Binary (e1, (Ast_c.Arith Ast_c.Plus, _), e2) -> addit
    | Ast_c.Binary (e1, (Ast_c.Arith Ast_c.Minus, _), e2) -> addit
    | Ast_c.Binary (e1, (Ast_c.Arith Ast_c.Mul, _), e2) -> addit
    | Ast_c.Binary (e1, (Ast_c.Arith Ast_c.Div, _), e2) -> addit
    | Ast_c.Binary (e1, (Ast_c.Arith Ast_c.Min, _), e2) -> relat
    | Ast_c.Binary (e1, (Ast_c.Arith Ast_c.Max, _), e2) -> relat
    | Ast_c.Binary (e1, (Ast_c.Arith Ast_c.Mod, _), e2) -> addit
    | Ast_c.Binary (e1, (Ast_c.Arith Ast_c.DecLeft, _), e2) -> addit
    | Ast_c.Binary (e1, (Ast_c.Arith Ast_c.DecRight, _), e2) -> addit
    | Ast_c.Binary (e1, (Ast_c.Arith Ast_c.And, _), e2) -> addit
    | Ast_c.Binary (e1, (Ast_c.Arith Ast_c.Or, _), e2) -> addit
    | Ast_c.Binary (e1, (Ast_c.Arith Ast_c.Xor, _), e2) -> addit
    | Ast_c.Binary (e1, (Ast_c.Logical Ast_c.AndLog, _), e2) -> addit
    | Ast_c.Binary (e1, (Ast_c.Logical Ast_c.OrLog, _), e2) -> addit
    | Ast_c.Binary (e1, (Ast_c.Logical Ast_c.Eq, _), e2) -> addit
    | Ast_c.Binary (e1, (Ast_c.Logical Ast_c.NotEq, _), e2) -> addit
    | Ast_c.Binary (e1, (Ast_c.Logical Ast_c.Sup, _), e2) -> addit
    | Ast_c.Binary (e1, (Ast_c.Logical Ast_c.Inf, _), e2) -> addit
    | Ast_c.Binary (e1, (Ast_c.Logical Ast_c.SupEq, _), e2) -> addit
    | Ast_c.Binary (e1, (Ast_c.Logical Ast_c.InfEq, _), e2) -> addit
    | Ast_c.ArrayAccess (e1, e2) -> postfix
    | Ast_c.RecordAccess (e, name) -> postfix
    | Ast_c.RecordPtAccess (e, name) -> postfix
    | Ast_c.SizeOfExpr (e) -> unary
    | Ast_c.SizeOfType (t) -> unary
    | Ast_c.Cast (t, a, e) -> cast
    | Ast_c.StatementExpr (statxs, _) -> top
    | Ast_c.Constructor (t, init) -> unary
    | Ast_c.ParenExpr (e) -> primary
    | Ast_c.New (_, t) -> unary
    | Ast_c.Delete(_,t) -> unary
    | Ast_c.Defined _ -> primary
  in

  let rec loop e prec =
  match Ast.unwrap e with
    Ast.Ident(id) -> ident id
  | Ast.Constant(const) -> mcode constant const
  | Ast.StringConstant(lq,str,rq,sz) ->
      print_text (sz2c sz); mcode print_string lq;
      dots (function _ -> ()) string_fragment str;
      mcode print_string rq
  | Ast.FunCall(fn,lp,args,rp) ->
      loop fn postfix; mcode (print_string_with_hint StartBox) lp;
      dots (function _ -> ()) arg_expression args;
      mcode (print_string_with_hint EndBox) rp
  | Ast.Assignment(left,op,right,_) ->
      loop left unary; pr_space(); assignOp op;
      pr_space(); loop right assign
  | Ast.Sequence(left,op,right) ->
      loop left top; mcode print_string op;
      pr_space(); loop right assign
  | Ast.CondExpr(exp1,why,exp2,colon,exp3) ->
      loop exp1 log_or; pr_space(); mcode print_string why;
      print_option (function e -> pr_space(); loop e top) exp2;
      pr_space(); mcode print_string colon; pr_space(); loop exp3 cond
  | Ast.Postfix(exp,op) -> loop exp postfix; mcode fixOp op
  | Ast.Infix(exp,op) -> mcode fixOp op; loop exp unary
  | Ast.Unary(exp,op) -> mcode unaryOp op; loop exp unary
  | Ast.Binary(left,op,right) ->
      loop left (left_prec_of op); pr_space(); binaryOp op; pr_space();
      loop right (right_prec_of op)
  | Ast.Nested(left,op,right) -> failwith "nested only in minus code"
  | Ast.Paren(lp,exp,rp) ->
      mcode print_string_box lp; loop exp top; close_box();
      mcode print_string rp
  | Ast.ArrayAccess(exp1,lb,exp2,rb) ->
      loop exp1 postfix; mcode print_string_box lb; loop exp2 top; close_box();
      mcode print_string rb
  | Ast.RecordAccess(exp,pt,field) ->
      loop exp postfix; mcode print_string pt; ident field
  | Ast.RecordPtAccess(exp,ar,field) ->
      loop exp postfix; mcode print_string ar; ident field
  | Ast.Cast(lp,ty,attr,rp,exp) ->
      mcode print_string_box lp; fullType ty; close_box();
      print_attribute_list attr;
      mcode print_string rp; loop exp cast
  | Ast.SizeOfExpr(sizeof,exp) ->
      mcode print_string sizeof; loop exp unary
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

  | Ast.MetaExpr (name,_,_,_typedontcare,_formdontcare,_,_bitfield) ->
      handle_metavar name (function
        | Ast_c.MetaExprVal ((((e, _), _) as exp),_,_) ->
	    if prec_of_c e < prec then
	      begin
		print_text "(";
		pretty_print_c.Pretty_print_c.expression exp;
		print_text ")"
	      end
	    else
              pretty_print_c.Pretty_print_c.expression exp
        | _ -> error name e "expression value expected"
      )

  | Ast.MetaExprList (name,_,_,_,_) ->
      handle_metavar name (function
        | Ast_c.MetaExprListVal args ->
            pretty_print_c.Pretty_print_c.arg_list args
	| Ast_c.MetaParamListVal _ ->
	    failwith "have meta param list matching meta exp list\n";
        | _ -> error name e "expression list value expected"
      )

  | Ast.AsExpr(expr,asexpr) -> loop expr prec
  | Ast.AsSExpr(expr,asstm) -> loop expr prec

  | Ast.EComma(cm) ->
      mcode (print_string_with_hint (SpaceOrNewline (ref " ")))  cm

  | Ast.DisjExpr(exp_list) ->
      if generating
      then print_disj_list expression exp_list "|"
      else raise CantBeInPlus
  | Ast.ConjExpr(exp_list) ->
      if generating
      then print_disj_list expression exp_list "&"
      else raise CantBeInPlus
  | Ast.NestExpr(starter,expr_dots,ender,Some whencode,multi)
    when generating ->
      nest_dots starter ender expression
	(function _ -> print_text "   when != "; expression whencode)
	expr_dots
  | Ast.NestExpr(starter,expr_dots,ender,None,multi) when generating ->
      nest_dots starter ender expression (function _ -> ()) expr_dots
  | Ast.NestExpr _ -> raise CantBeInPlus
  | Ast.Edots(dots,Some whencode) ->
      if generating
      then
	(mcode print_string dots;
	 print_text "   when != ";
	 expression whencode)
      else raise CantBeInPlus
  | Ast.Edots(dots,None) ->
      if generating
      then mcode print_string dots
      else raise CantBeInPlus

  | Ast.OptExp(exp) -> raise CantBeInPlus in
  loop e top

and arg_expression e =
  match Ast.unwrap e with
    Ast.EComma(cm) ->
      (* space is only used by add_newline, and only if not using SMPL
	 spacing.  pr_cspace uses a " " in unparse_c.ml.  Not so nice... *)
      mcode (print_string_with_hint (SpaceOrNewline (ref " "))) cm
  | _ -> expression e

and string_fragment e =
  match Ast.unwrap e with
    Ast.ConstantFragment(str) -> mcode print_string str
  | Ast.FormatFragment(pct,fmt) ->
      mcode print_string pct;
      string_format fmt
  | Ast.Strdots dots -> mcode print_string dots
  | Ast.MetaFormatList(pct,name,lenname,_,_,_) ->
      (*mcode print_string pct;*)
      handle_metavar name (function
	  Ast_c.MetaFragListVal(frags) ->
	    frags +> (List.iter pretty_print_c.Pretty_print_c.fragment)
	| _ -> error name e "format list value expected")

and string_format e =
  match Ast.unwrap e with
    Ast.ConstantFormat(str) -> mcode print_string str
  | Ast.MetaFormat(name,_,_,_) ->
      handle_metavar name (function
	  Ast_c.MetaFmtVal fmt ->
	    pretty_print_c.Pretty_print_c.format fmt
	| _ -> error name e "format value expected")

and unaryOp = function
    Ast.GetRef -> print_string "&"
  | Ast.GetRefLabel -> print_string "&&"
  | Ast.DeRef -> print_string "*"
  | Ast.UnPlus -> print_string "+"
  | Ast.UnMinus -> print_string "-"
  | Ast.Tilde -> print_string "~"
  | Ast.Not -> print_string "!"

and assignOp op =
  match Ast.unwrap op with
    Ast.SimpleAssign op -> mcode print_string op
  | Ast.OpAssign(aop) -> mcode (arithOp true) aop
  | Ast.MetaAssign(name,_,_,_) ->
      handle_metavar name (function
	  Ast_c.MetaAssignOpVal aop ->
	    pretty_print_c.Pretty_print_c.assignOp aop
	| _ -> error name op "assignment operator value expected")

and fixOp = function
    Ast.Dec -> print_string "--"
  | Ast.Inc -> print_string "++"

and binaryOp op =
  match Ast.unwrap op with
    Ast.Arith(aop) -> mcode (arithOp false) aop
  | Ast.Logical(lop) -> mcode logicalOp lop
  | Ast.MetaBinary(name,_,_,_) ->
      handle_metavar name (function
	  Ast_c.MetaBinaryOpVal bop ->
	    pretty_print_c.Pretty_print_c.binaryOp bop
	| _ -> error name op "binary operator value expected")

and arithOp eq op =
  let print_string s = if eq then print_string (s^"=") else print_string s in
  match op with
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

and logicalOp = function
    Ast.Inf -> print_string "<"
  | Ast.Sup -> print_string ">"
  | Ast.InfEq -> print_string "<="
  | Ast.SupEq -> print_string ">="
  | Ast.Eq -> print_string "=="
  | Ast.NotEq -> print_string "!="
  | Ast.AndLog -> print_string "&&"
  | Ast.OrLog -> print_string "||"

and constant = function
    Ast.String(s,sz) -> print_text (sz2c sz); print_string ("\""^s^"\"")
  | Ast.Char(s,sz) -> print_text (sz2c sz); print_string ("\'"^s^"\'")
  | Ast.Int(s) -> print_string s
  | Ast.Float(s) -> print_string s
  | Ast.DecimalConst(s,_l,_p) -> print_string s

and sz2c = function
    Ast.IsChar -> ""
  | Ast.IsUchar -> "U"
  | Ast.Isuchar -> "u"
  | Ast.Isu8char -> "u8"
  | Ast.IsWchar -> "L"

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
  | Ast.DisjType _ | Ast.ConjType _ -> raise CantBeInPlus
  | Ast.OptType(_) -> raise CantBeInPlus

and print_fninfo = function
    Ast.FStorage(stg) -> mcode storage stg
  | Ast.FType(ty) -> fullType ty
  | Ast.FInline(inline) -> mcode print_string inline; pr_space()
  | Ast.FAttr(attr) -> print_attribute attr; pr_space()

and print_attribute_list attrs =
  if not (attrs = []) then pr_space();
  print_between pr_space print_attribute attrs

and print_attribute attr =
  match Ast.unwrap attr with
    Ast.Attribute(a) -> print_attr_arg a

and print_attr_arg arg =
  match Ast.unwrap arg with
    Ast.AttrName(arg) -> mcode print_string arg
  | Ast.MetaAttr(name,_,_,_) ->
      handle_metavar name
	(function
	    Ast_c.MetaAttrArgVal a ->
              pretty_print_c.Pretty_print_c.attr_arg a
          | _ -> error name arg "attr_arg value expected")

and typeC ty =
  match Ast.unwrap ty with
    Ast.BaseType(ty,strings) ->
      print_between pr_space (mcode print_string) strings
  | Ast.SignedT(sgn,ty) -> mcode sign sgn; print_option_prespace typeC ty
  | Ast.Pointer(ty,star) ->
      fullType ty; ft_space ty; mcode print_string star; eatspace()
  | Ast.ParenType(lp,ty,rp) ->
      print_parentype (lp,ty,rp) (function _ -> ())
  | Ast.FunctionType(ty,lp,params,rp) ->
      fullType ty;
      mcode print_string lp;
      parameter_list params;
      mcode print_string rp
  | Ast.Array(ty,lb,size,rb) ->
      fullType ty; mcode print_string lb; print_option expression size;
      mcode print_string rb
  | Ast.Decimal(dec,lp,length,comma,precision_opt,rp) ->
      mcode print_string dec;
      mcode print_string lp;
      expression length;
      print_option (mcode print_string) comma;
      print_option expression precision_opt;
      mcode print_string rp
  | Ast.EnumName(kind,name) ->
      mcode print_string kind;
      print_option_prespace ident name
  | Ast.EnumDef(ty,lb,ids,rb) ->
      fullType ty; ft_space ty;
      mcode print_string lb;
      dots force_newline enum_decl ids;
      mcode print_string rb
  | Ast.StructUnionName(kind,name) ->
      mcode structUnion kind; print_option_prespace ident name
  | Ast.StructUnionDef(ty,lb,decls,rb) ->
      fullType ty; ft_space ty;
      mcode print_string lb;
      dots_before_and_after force_newline annotated_field decls;
      mcode print_string rb
  | Ast.TypeOfExpr(typeof,lp,exp,rp) ->
      mcode print_string typeof;
      mcode print_string_box lp;  expression exp; close_box();
      mcode print_string rp
  | Ast.TypeOfType(typeof,lp,ty,rp) ->
      mcode print_string typeof;
      mcode print_string_box lp; fullType ty; close_box();
      mcode print_string rp
  | Ast.TypeName(name)-> mcode print_string name
  | Ast.AutoType(auto) -> mcode print_string auto
  | Ast.MetaType(name,_,_,_) ->
      handle_metavar name  (function
          Ast_c.MetaTypeVal exp ->
            pretty_print_c.Pretty_print_c.ty exp
        | _ -> error name ty "type value expected")

and baseType ty = print_string (Ast.string_of_baseType ty ^ " ")

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
(* ParenType *)

and print_parentype (lp,ty,rp) fn =
  let function_pointer ty1 array_dec =
    match Ast.unwrap ty1 with
     Ast.Type(_,_,fty1) ->
      (match Ast.unwrap fty1 with
        Ast.Pointer(ty2,star) ->
         (match Ast.unwrap ty2 with
           Ast.Type(_,_,fty3) ->
            (match Ast.unwrap fty3 with
              Ast.FunctionType(ty3,lp3,params,rp3) ->
               fullType ty3;
               pr_space();
               mcode print_string lp;
               mcode print_string star;
               fn();
               let _ =
                 match array_dec with
                   Some(lb1,size,rb1) ->
                    mcode print_string lb1;
                    print_option expression size;
                    mcode print_string rb1
                 | None -> () in
               mcode print_string rp;
               mcode print_string lp3;
               parameter_list params;
               mcode print_string rp3
           | _ -> failwith "ParenType Unparse_cocci")
         | _ -> failwith "ParenType Unparse_cocci")
       | _ -> failwith "ParenType Unparse_cocci")
     | _ -> failwith "ParenType Unparse_cocci" in
  match Ast.unwrap ty with
    Ast.Type(_,_,fty1) ->
      (match Ast.unwrap fty1 with
        Ast.Array(ty1,lb1,size,rb1) ->
          function_pointer ty1 (Some(lb1,size,rb1))
      | Ast.Pointer(ty1,star) ->
          function_pointer ty None
      | _ -> failwith "ParenType Unparse_cocci")
   | _ -> failwith "ParenType Unparse_cocci"

(* --------------------------------------------------------------------- *)
(* Variable declaration *)

and print_named_type ty id =
  match Ast.unwrap ty with
    Ast.Type(_,None,ty1) ->
      (match Ast.unwrap ty1 with
        Ast.Array(_,_,_,_) ->
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
	    | _ -> typeC ty; ty_space ty; id(); k () in
	  loop ty1 (function _ -> ())
      | Ast.MetaType(name,_,_,_) ->
	  handle_metavar name  (function
              Ast_c.MetaTypeVal ty ->
		pretty_print_c.Pretty_print_c.type_with_ident ty
		  (function _ -> id())
            | _ -> error name ty "type value expected")
      | Ast.ParenType(lp,ty,rp) ->
          print_parentype (lp,ty,rp) (function _ -> id())
      | Ast.FunctionType(ty,lp,params,rp) ->
          fullType ty;
          id();
          mcode print_string lp;
          parameter_list params;
          mcode print_string rp
    (*| should have a case here for pointer to array or function type
        that would put ( * ) around the variable.  This makes one wonder
        why we really need a special case for function pointer *)
      | _ -> fullType ty; ft_space ty; id())
  | _ -> fullType ty; ft_space ty; id()

and ty_space ty =
  match Ast.unwrap ty with
    Ast.Pointer(_,_) -> ()
  | _ -> pr_space()

and ft_space ty =
  match Ast.unwrap ty with
    Ast.Type(_,cv,ty) ->
      let isptr =
	match Ast.unwrap ty with
	  Ast.Pointer(_,_) -> true
	| Ast.MetaType(name,_,_,_) ->
	    let (res,name_string,line,lcol,rcol) = lookup_metavar name in
	    (match res with
	      None ->
		failwith
		  (Printf.sprintf "variable %s not known on SP line %d\n"
		     name_string line)
	    | Some (Ast_c.MetaTypeVal (tq,ty)) ->
		(match Ast_c.unwrap ty with
		  Ast_c.Pointer(_,_) ->  true
		| _ -> false)
	    | _ -> false)
	| _ -> false in
      if isptr then () else pr_space()
  | _ -> pr_space()

and declaration d =
  match Ast.unwrap d with
    Ast.MetaDecl(name,_,_,_) ->
      handle_metavar name
	(function
	    Ast_c.MetaDeclVal (_,d) ->
              pretty_print_c.Pretty_print_c.decl d
          | _ -> error name d "declaration value expected")

  | Ast.AsDecl(decl,asdecl) -> declaration decl

  | Ast.Init(stg,ty,id,attr,eq,ini,sem) ->
      print_option (mcode storage) stg;
      print_option (function _ -> pr_space()) stg;
      print_named_type ty (fun _ -> ident id);
      print_attribute_list attr;
      pr_space(); mcode print_string eq;
      pr_space(); initialiser true ini; mcode print_string sem
  | Ast.UnInit(stg,ty,id,attr,sem) ->
      print_option (mcode storage) stg;
      print_option (function _ -> pr_space()) stg;
      print_named_type ty (fun _ -> ident id);
      print_attribute_list attr;
      mcode print_string sem
  | Ast.FunProto (fninfo,name,lp1,params,va,rp1,sem) ->
      List.iter print_fninfo fninfo;
      ident name; mcode print_string_box lp1;
      parameter_list params;
      begin match va with
        | None -> ()
        | Some (comma, ellipsis) ->
          mcode print_string comma;
          mcode print_string ellipsis
      end;
      close_box(); mcode print_string rp1;
      mcode print_string sem
  | Ast.MacroDecl(stg,name,lp,args,rp,attr,sem) ->
      print_option (mcode storage) stg;
      print_option (function _ -> pr_space()) stg;
      ident name; mcode print_string_box lp;
      dots (function _ -> ()) arg_expression args;
      close_box(); mcode print_string rp;
      print_attribute_list attr;
      mcode print_string sem
  | Ast.MacroDeclInit(stg,name,lp,args,rp,eq,ini,sem) ->
      print_option (mcode storage) stg;
      print_option (function _ -> pr_space()) stg;
      ident name; mcode print_string_box lp;
      dots (function _ -> ()) arg_expression args;
      close_box(); mcode print_string rp;
      pr_space(); mcode print_string eq;
      pr_space(); initialiser true ini; mcode print_string sem
  | Ast.TyDecl(ty,attr,sem) ->
      fullType ty;
      print_attribute_list attr;
      mcode print_string sem
  | Ast.Typedef(stg,ty,id,sem) ->
      mcode print_string stg; pr_space();
      print_named_type ty (fun _ -> typeC id);
      mcode print_string sem
  | Ast.DisjDecl(_) | Ast.ConjDecl(_) -> raise CantBeInPlus
  | Ast.OptDecl(decl) -> raise CantBeInPlus

and annotated_decl d =
  match Ast.unwrap d with
    Ast.DElem(_,_,decl) -> declaration decl

(* --------------------------------------------------------------------- *)
(* Field declaration *)

and field d =
  match Ast.unwrap d with
    Ast.MetaField(name,_,_,_) ->
      handle_metavar name
	(function
	    Ast_c.MetaFieldVal f ->
	      pretty_print_c.Pretty_print_c.field f
	  | _ -> error name d "field value expected")

  | Ast.MetaFieldList(name,_,_,_,_) ->
      handle_metavar name
	(function
	    Ast_c.MetaFieldListVal f ->
	      print_between force_newline pretty_print_c.Pretty_print_c.field f
	  | _ -> error name d "field list value expected")
  | Ast.Field(ty,id,bf,sem) ->
      begin
	match id with
	  None -> fullType ty
	| Some id -> print_named_type ty (fun _ -> ident id)
      end;
      let bitfield (c, e) =
	mcode print_string c;
	expression e in
      Common.do_option bitfield bf;
      mcode print_string sem

and annotated_field d =
  match Ast.unwrap d with
    Ast.FElem(_,_,decl) -> field decl
  | Ast.DisjField(_) | Ast.ConjField(_) -> raise CantBeInPlus
  | Ast.OptField(decl) -> raise CantBeInPlus
  | Ast.Fdots(_,_) -> raise CantBeInPlus

and enum_decl d =
  match Ast.unwrap d with
    Ast.Enum(name,enum_val) ->
      ident name;
      pr_space();
      print_option
        (function (eq,eval) ->
          mcode print_string eq; pr_space(); expression eval) enum_val
  | Ast.EnumComma(cm) ->
      mcode (print_string_with_hint (SpaceOrNewline (ref " "))) cm
  | Ast.EnumDots(dots,whencode) when generating ->
      mcode print_string dots;
      print_option
      (function w ->
        print_text "   when != ";
        enum_decl w) whencode
  | Ast.EnumDots(dots,whencode) -> raise CantBeInPlus

(* --------------------------------------------------------------------- *)
(* Initialiser *)

and initialiser nlcomma i =
  match Ast.unwrap i with
    Ast.MetaInit(name,_,_,_) ->
      handle_metavar name  (function
          Ast_c.MetaInitVal ini ->
            pretty_print_c.Pretty_print_c.init ini
        | _ -> error name i "initialiser value expected")
  | Ast.MetaInitList(name,_,_,_,_) ->
      handle_metavar name  (function
          Ast_c.MetaInitListVal ini ->
	    pretty_print_c.Pretty_print_c.init_list ini
        | _ -> error name i "initialiser list value expected")
  | Ast.AsInit(init,asinit) -> initialiser nlcomma init
  | Ast.InitExpr(exp) -> expression exp
  | Ast.ArInitList(lb,initlist,rb) ->
      (match Ast.unwrap initlist with
	[] -> mcode print_string lb; mcode print_string rb
      |	lst ->
	  mcode print_string lb; start_block();
	  initialiser_list nlcomma lst;
	  end_block lst; mcode print_string rb)
  | Ast.StrInitList(_,lb,[],rb,[]) ->
      mcode print_string lb; mcode print_string rb
  | Ast.StrInitList(_,lb,initlist,rb,[]) ->
      mcode (print_string_with_hint StartBox) lb; start_block();
      initialiser_list nlcomma initlist;
      end_block initlist; mcode (print_string_with_hint EndBox) rb
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
  | Ast.OptIni(ini) -> raise CantBeInPlus

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
    Ast.VoidParam(ty,attr) ->
      fullType ty;
      print_attribute_list attr;
  | Ast.Param(ty,Some id,attr) ->
      print_named_type ty (fun _ -> ident id);
      print_attribute_list attr;
  | Ast.Param(ty,None,attr) ->
      fullType ty;
      print_attribute_list attr;
  | Ast.MetaParam(name,_,_,_) ->
      handle_metavar name
	(function
	    Ast_c.MetaParamVal p ->
              pretty_print_c.Pretty_print_c.param p
          | _ -> error name p "parameter value expected")
  | Ast.MetaParamList(name,_,_,_,_) ->
      handle_metavar name
	(function
	    Ast_c.MetaParamListVal p ->
              pretty_print_c.Pretty_print_c.paramlist p
          | _ -> error name p "parameter list value expected")

  | Ast.AsParam(p,e) -> raise CantBeInPlus

  | Ast.PComma(cm) ->
      mcode (print_string_with_hint (SpaceOrNewline (ref " ")))  cm
  | Ast.Pdots(dots) when generating ->
      mcode print_string dots
  | Ast.Pdots(_) -> raise CantBeInPlus
  | Ast.OptParam(param) -> raise CantBeInPlus

and parameter_list l = dots (function _ -> ()) parameterTypeDef l
in


(* --------------------------------------------------------------------- *)
(* CPP code *)

let rec inc_file = function
    Ast.Local(elems) ->
      print_string ("\""^(String.concat "/" (List.map inc_elem elems))^"\"")
  | Ast.NonLocal(elems) ->
      print_string ("<"^(String.concat "/" (List.map inc_elem elems))^">")
  | Ast.AnyInc -> failwith "should not be in generated code"

and inc_elem = function
    Ast.IncPath s -> s
  | Ast.IncDots -> "..."

(* --------------------------------------------------------------------- *)
(* Top-level code *)

and rule_elem arity re =
  match Ast.unwrap re with
    Ast.FunHeader(_,_,fninfo,name,lp,params,va,rp) ->
      pr_arity arity; List.iter print_fninfo fninfo;
      ident name; mcode print_string_box lp;
      parameter_list params;
      begin match va with
        | None -> ()
        | Some (comma, ellipsis) ->
          mcode print_string comma;
          mcode print_string ellipsis
      end;
      close_box(); mcode print_string rp;
      force_newline()
  | Ast.Decl decl -> pr_arity arity; annotated_decl decl

  | Ast.SeqStart(brace) ->
      pr_arity arity; mcode print_string brace; start_block()
  | Ast.SeqEnd(brace) ->
      pr_arity arity; mcode print_string brace

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
      dots (function _ -> ()) arg_expression args; close_box();
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
  | Ast.Exec(exec,lang,code,sem) ->
      pr_arity arity; mcode print_string exec; pr_space();
      mcode print_string lang; pr_space();
      dots (function _ -> pr_space()) exec_code code;
      mcode print_string sem
  | Ast.Exp(exp) -> pr_arity arity; expression exp
  | Ast.TopExp(exp) -> pr_arity arity; expression exp
  | Ast.Ty(ty) -> pr_arity arity; fullType ty
  | Ast.TopId(id) -> pr_arity arity; ident id
  | Ast.TopInit(init) -> initialiser false init
  | Ast.Include(inc,s) ->
      mcode print_string inc; print_text " "; mcode inc_file s
  | Ast.MetaInclude(inc,s) ->
      mcode print_string inc; print_text " "; expression s
  | Ast.Undef(def,id) ->
      mcode print_string def; pr_space(); ident id
  | Ast.DefineHeader(def,id,params) ->
      mcode print_string def; pr_space(); ident id;
      print_define_parameters params
  | Ast.Pragma(prg,id,body) ->
      mcode print_string prg; print_text " "; ident id; print_text " ";
      pragmainfo body
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

  | Ast.MetaRuleElem(name,_,_,_) ->
      raise (Impossible 155)

  | Ast.MetaStmt(name,_,_,_,_) ->
      handle_metavar name (function
        | Ast_c.MetaStmtVal(_,stm,_) ->
            pretty_print_c.Pretty_print_c.statement stm
        | _ -> error name re "statement value expected")

  | Ast.MetaStmtList(name,_,_,_,_) ->
      handle_metavar name (function
        | Ast_c.MetaStmtListVal(statxs,_) ->
            pretty_print_c.Pretty_print_c.statement_seq_list statxs
        | _ -> error name re "statement list value expected")
  | Ast.AsRe(re,asre) -> rule_elem arity re

and pragmainfo pi =
  match Ast.unwrap pi with
      Ast.PragmaString(s) -> mcode print_string s
    | Ast.PragmaDots (dots) -> mcode print_string dots

and forinfo = function
    Ast.ForExp(e1,sem1) ->
      print_option expression e1; mcode print_string sem1
  | Ast.ForDecl decl -> annotated_decl decl

and print_define_parameters params =
  match Ast.unwrap params with
    Ast.NoParams -> ()
  | Ast.DParams(lp,params,rp) ->
      mcode print_string lp;
      dots (function _ -> ()) print_define_param params; mcode print_string rp

and print_define_param param =
  match Ast.unwrap param with
    Ast.DParam(id) -> ident id
  | Ast.MetaDParamList(name,_,_,_,_) ->
      handle_metavar name
	(function
	    Ast_c.MetaDParamListVal p ->
              pretty_print_c.Pretty_print_c.dparamlist p
          | _ -> error name param "#define parameter value expected")
  | Ast.DPComma(comma) -> mcode print_string comma
  | Ast.DPdots(dots) -> mcode print_string dots
  | Ast.OptDParam(dp) -> print_text "?"; print_define_param dp

and exec_code (e : Ast_cocci.exec_code) =
  match Ast.unwrap e with
    Ast.ExecEval(colon,id) -> mcode print_string colon; expression id
  | Ast.ExecToken(tok) -> mcode print_string tok
  | Ast.ExecDots(dots) -> mcode print_string dots in

let indent_if_needed s f =
  let isseq =
    match Ast.unwrap s with
      Ast.Seq(lbrace,body,rbrace) -> true
    | Ast.Atomic s ->
	(match Ast.unwrap s with
	| Ast.MetaStmt(name,_,_,_,_) ->
	    let (res,name_string,line,lcol,rcol) = lookup_metavar name in
	    (match res with
	      None ->
		failwith
		  (Printf.sprintf "variable %s not known on SP line %d\n"
		     name_string line)
	    | Some (Ast_c.MetaStmtVal(_,stm,_)) ->
		(match Ast_c.unwrap stm with
		  Ast_c.Compound _ -> true
		| _ -> false)
	    | _ -> failwith "bad metavariable value")
	| _ -> false)
    | _ -> false in
  if isseq
  then begin pr_space(); f() end
  else
    begin
      (*no newline at the end - someone else will do that*)
      indent(); start_block(); f(); unindent true
    end in

let rec statement arity s =
  match Ast.unwrap s with
    Ast.Seq(lbrace,body,rbrace) ->
      rule_elem arity lbrace;
      dots force_newline (statement arity) body;
      end_block (Ast.unwrap body);
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

  | Ast.FunDecl(header,lbrace,body,rbrace,_) ->
      rule_elem arity header; rule_elem arity lbrace;
      dots force_newline (statement arity) body;
      end_block (Ast.unwrap body);
      rule_elem arity rbrace

  | Ast.Define(header,body) ->
      rule_elem arity header; pr_space();
      dots force_newline (statement arity) body

  | Ast.AsStmt(stmt,asstmt) -> statement arity stmt

  | Ast.Disj([stmt_dots]) | Ast.Conj([stmt_dots]) ->
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
  | Ast.Conj(stmt_dots_list) -> (* ignores newline directive for readability *)
      if generating
      then
	(pr_arity arity; print_text "\n(\n";
	 print_between (function _ -> print_text "\n&\n")
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
  | Ast.Dots(d,whn,_,_) ->
      if generating
      then
	(pr_arity arity; mcode print_string d;
	 print_between force_newline
	   (whencode (dots force_newline (statement "")) (statement "")) whn;
	 force_newline())
      else raise CantBeInPlus

  | Ast.OptStm(s) -> raise CantBeInPlus

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

let if_open_brace  = function "{" | "else" -> true | _ -> false in

(* boolean result indicates whether an indent is needed *)
let pp_any = function
  (* assert: normally there is only CONTEXT NOTHING tokens in any *)
    Ast.FullTypeTag(x) -> fullType x; false
  | Ast.BaseTypeTag(x) -> baseType x unknown unknown; false
  | Ast.StructUnionTag(x) -> structUnion x unknown unknown; false
  | Ast.SignTag(x) -> sign x unknown unknown; false

  | Ast.IdentTag(x) -> ident x; false

  | Ast.ExpressionTag(x) -> expression x; false

  | Ast.ConstantTag(x) -> constant x unknown unknown; false
  | Ast.UnaryOpTag(x) -> unaryOp x unknown unknown; false
  | Ast.AssignOpTag(x) -> assignOp x; false
  | Ast.SimpleAssignOpTag(x) -> print_string x unknown unknown; false
  | Ast.OpAssignOpTag(x) -> arithOp true x unknown unknown; false
  | Ast.FixOpTag(x) -> fixOp x unknown unknown; false
  | Ast.BinaryOpTag(x) -> binaryOp x; false
  | Ast.ArithOpTag(x) -> arithOp false x unknown unknown; false
  | Ast.LogicalOpTag(x) -> logicalOp x unknown unknown; false

  | Ast.InitTag(x) -> initialiser false x; false
  | Ast.DeclarationTag(x) -> declaration x; false
  | Ast.FieldTag(x) -> field x; false
  | Ast.EnumDeclTag(x) -> enum_decl x; false

  | Ast.StorageTag(x) -> storage x unknown unknown; false
  | Ast.IncFileTag(x) -> inc_file x unknown unknown; false

  | Ast.Rule_elemTag(x) -> rule_elem "" x; false
  | Ast.StatementTag(x) -> statement "" x; false
  | Ast.ForInfoTag(x) -> forinfo x; false
  | Ast.CaseLineTag(x) -> case_line "" x; false
  | Ast.StringFragmentTag(x) -> string_fragment x; false
  | Ast.AttributeTag(x) -> print_attribute x; false
  | Ast.AttrArgTag(x) -> print_attr_arg x; false

  | Ast.ConstVolTag(x) -> const_vol x unknown unknown; false
  | Ast.Directive(xs) ->
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
      loop xs; true
  | Ast.Token(x,None) ->
      print_text x; if_open_brace x
  | Ast.Token(x,Some info) ->
      mcode
	(fun x line lcol ->
	  (* adds a newline before else, but not sure why; not correct after
	  brace in Linux, and normally should not be needed. *)
	  (*
	  (match x with
	    "else" -> force_newline()
	  | _ -> ());
	  *)
	  (match x with (* not sure if special case for comma is useful *)
	    "," -> print_string_with_hint (SpaceOrNewline(ref " ")) x line lcol
	  | _ -> print_string x line lcol))
	(let nomcodekind = Ast.CONTEXT(Ast.DontCarePos,Ast.NOTHING) in
	(x,info,nomcodekind,[]));
      if_open_brace x

  | Ast.Code(x) -> let _ = top_level x in false

  (* this is not '...', but a list of expr/statement/params, and
     normally there should be no '...' inside them *)
  | Ast.ExprDotsTag(x) -> dots (fun _ -> ()) expression x; false
  | Ast.ParamDotsTag(x) -> parameter_list x; false
  | Ast.StmtDotsTag(x) -> dots force_newline (statement "") x; false
  | Ast.AnnDeclDotsTag(x) -> dots force_newline annotated_decl x; false
  | Ast.AnnFieldDotsTag(x) -> dots force_newline annotated_field x; false
  | Ast.EnumDeclDotsTag(x) -> dots force_newline enum_decl x; false
  | Ast.DefParDotsTag(x) -> dots (fun _ -> ()) print_define_param x; false

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
      let prnl x = force_newline() in
      let newline_before _ =
	if before = After
	then
	  let hd = List.hd xxs in
	  match hd with
	    (Ast.Directive l::_)
	      when List.for_all (function Ast.Space x -> true | _ -> false) l ->
		()
	  | (Ast.StatementTag s::_) when isfn s ->
	      force_newline(); force_newline()
	  | (Ast.Directive _::_)
	  | (Ast.Rule_elemTag _::_) | (Ast.StatementTag _::_)
	  | (Ast.FieldTag _::_) | (Ast.EnumDeclTag _::_) | (Ast.InitTag _::_)
	  | (Ast.DeclarationTag _::_) | (Ast.Token ("}",_)::_) -> prnl hd
	  | _ -> () in
      let newline_after _ =
	if before = Before
	then
	  match List.rev(List.hd(List.rev xxs)) with
	    (Ast.StatementTag s::_) ->
	      (if isfn s then force_newline());
	      force_newline()
	  | (Ast.Directive _::_) | (Ast.StmtDotsTag _::_)
	  | (Ast.Rule_elemTag _::_) | (Ast.FieldTag _::_)
	  | (Ast.EnumDeclTag _::_)| (Ast.InitTag _::_)
	  | (Ast.DeclarationTag _::_) | (Ast.Token ("{",_)::_) ->
	      force_newline()
	  | _ -> () in
      (* print a newline at the beginning, if needed *)
      newline_before();
      (* print a newline before each of the rest *)
      let rec loop leading_newline indent_needed = function
	  [] -> ()
	| x::xs ->
	    (if leading_newline then force_newline());
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
	      |	Ast.UnaryOpTag(x) -> false
	      |	Ast.ParamTag(x) ->
		  (match Ast.unwrap x with
		    Ast.PComma _ -> false (* due to hint *)
		  | _ -> true)
	      |	Ast.ExpressionTag(x) ->
		  (match Ast.unwrap x with
		    Ast.EComma _ -> false (* due to hint *)
		  | _ -> true)
	      |	t -> true in
	    let indent_needed =
	      let rec loop space_after indent_needed = function
		  [] -> indent_needed
		| x::xs ->
		    (if indent_needed (* for open brace *)
		    then force_newline()
		    else if space_after && space_needed_before x
		    then pr_space());
		    let indent_needed = pp_any x in
		    let space_after = space_needed_after x in
		    loop space_after indent_needed xs in
	      loop false false x in
	    let newline_needed =
	      (* tokens don't group into multiline terms, so may need to avoid
		 adding newline *)
	      match List.hd(List.rev x) with
		Ast.Token(t,_) when List.mem t [";";"{";"}"] -> true
	      |	Ast.Token(t,_) -> false
	      | Ast.ParamDotsTag _ | Ast.ExprDotsTag _ ->
		  (* should never be followed by a newline *)
		  false
	      |	_ -> true in
	    loop newline_needed indent_needed xs in
      loop false false (x::xs);
      (* print a newline at the end, if needed *)
      newline_after()

let pp_list_list_any (envs, pr, pr_celem, pr_cspace, pr_space, pr_arity,
			  pr_barrier, indent, unindent, eatspace)
    generating xxs before =
  match envs with
    [] -> ()
  | first::rest ->
      do_all (first, pr, pr_celem, pr_cspace, pr_space, pr_arity, pr_barrier,
	      indent, unindent, eatspace)
	generating xxs before;
      let before =
	match before with
	  InPlace -> After
	| _ -> before in
      List.iter
	(function env ->
	  do_all (env, pr, pr_celem, pr_cspace, pr_space, pr_arity, pr_barrier,
		  indent, unindent, eatspace)
	    generating xxs before)
	rest
      
