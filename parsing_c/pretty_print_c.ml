(* Yoann Padioleau, Julia Lawall
 *
 * Copyright (C) 2010, University of Copenhagen DIKU and INRIA.
 * Copyright (C) 2006, 2007, 2008, 2009 Ecole des Mines de Nantes and DIKU
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)
open Common

open Ast_c

module F = Control_flow_c

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2, pr2_once = Common.mk_pr2_wrappers Flag_parsing_c.verbose_unparsing

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type type_with_ident = Ast_c.fullType -> (unit -> unit) -> unit
type type_with_ident_rest = Ast_c.fullType -> (unit -> unit) -> unit

type 'a printer = 'a -> unit

type pretty_printers = {
  expression      : Ast_c.expression printer;
  assignOp        : Ast_c.assignOp printer;
  binaryOp        : Ast_c.binaryOp printer;
  arg_list        : (Ast_c.argument Ast_c.wrap2 list) printer;
  arg             : Ast_c.argument printer;
  statement       : Ast_c.statement printer;
  statement_seq_list : Ast_c.statement_sequencable list printer;
  decl            : Ast_c.declaration printer;
  field           : Ast_c.field printer;
  field_list      : Ast_c.field list printer;
  init            : Ast_c.initialiser printer;
  init_list       : (Ast_c.newlines * Ast_c.initialiser wrap2 list) printer;
  param           : Ast_c.parameterType printer;
  paramlist       : (Ast_c.parameterType Ast_c.wrap2 list) printer;
  template_param  : Ast_c.templateParameterType printer;
  template_paramlist : (Ast_c.templateParameterType Ast_c.wrap2 list) printer;
  dparamlist      : ((string Ast_c.wrap) Ast_c.wrap2 list) printer;
  ty              : Ast_c.fullType printer;
  type_with_ident : type_with_ident;
  base_type       : Ast_c.fullType printer;
  type_with_ident_rest : type_with_ident_rest;
  toplevel        : Ast_c.toplevel printer;
  fragment        : Ast_c.string_fragment printer;
  fragment_list   : (Ast_c.string_fragment list) printer;
  format          : Ast_c.string_format printer;
  attribute       : Ast_c.attribute printer;
  attr_arg        : Ast_c.attr_arg printer;
  flow            : Control_flow_c.node printer;
  name            : Ast_c.name printer
}



(*****************************************************************************)

(* This module is used by unparse_c, but because unparse_c have also
 * the list of tokens, pretty_print_c could be useless in the future
 * (except that the ast_c have some fake tokens not present in the list
 * of tokens so it's still useful). But this module is also useful to
 * unparse C when you don't have the ordered list of tokens separately,
 * or tokens without position information, for instance when you want
 * to pretty print some piece of C that was generated, or some
 * abstract-lined piece of code, etc. *)

let mk_pretty_printers
    ~pr_elem ~pr_space
    ~pr_nl ~pr_indent ~pr_outdent ~pr_unindent
 =
  let rec redo pr_elem =
  let start_block () = pr_nl(); pr_indent() in
  let end_block   () = pr_unindent(); pr_nl() in
  let pr_text s = pr_elem (Ast_c.fakeInfo Ast_c.After +> Ast_c.rewrap_str s) in
(*
  let pr_nl_slash _ = (* multiline macro *)
    pr_text " \\"; pr_nl() in
*)
  let indent_if_needed st f =
    match Ast_c.unwrap_st st with
      Compound _ -> pr_space(); f()
    | _ ->
        (*no newline at the end - someone else will do that*)
	start_block(); f(); pr_unindent() in


  let pp_list printer l =
    l +> List.iter (fun (e, opt) ->
      assert (List.length opt <= 1); (* opt must be a comma? *)
      opt +> List.iter (function x -> pr_elem x; pr_space());
      printer e) in

  let pp_nl_list printer l =
    l +> List.iter (fun (e, opt) ->
      assert (List.length opt <= 1); (* opt must be a comma? *)
      opt +> List.iter (function x -> pr_elem x; pr_nl());
      printer e) in

  let pp_list2 printer l = (* no comma case *)
    l +> List.iter printer in

  let rec pp_expression = fun ((exp, typ), ii) ->
    (match exp, ii with
    | Ident (ident),         []     -> pp_name ident
    (* only a MultiString can have multiple ii *)
    | Constant (MultiString _), is     ->
	is +> Common.print_between pr_space pr_elem
    | Constant (c),         [i]     -> pr_elem i
    | StringConstant(s,os,w),  [i1;i2] ->
	pr_elem i1;
	s +> (List.iter pp_string_fragment);
	pr_elem i2
    | FunCall  (e, es),     [i1;i2] ->
        pp_expression e; pr_elem i1;
	pp_arg_list es;
        pr_elem i2

    | CondExpr (e1, e2, e3),    [i1;i2]    ->
        pp_expression e1; pr_space(); pr_elem i1; pr_space();
	do_option (function x -> pp_expression x; pr_space()) e2; pr_elem i2;
        pr_space(); pp_expression e3
    | Sequence (e1, e2),          [i]  ->
        pp_expression e1; pr_elem i; pr_space(); pp_expression e2
    | Assignment (e1, op, e2),    []  ->
        pp_expression e1; pr_space(); pr_assignOp op; pr_space(); pp_expression e2

    | Postfix  (e, op),    [i] -> pp_expression e; pr_elem i;
    | Infix    (e, op),    [i] -> pr_elem i; pp_expression e;
    | Unary    (e, op),    [i] -> pr_elem i; pp_expression e
    | Binary   (e1, op, e2),    [] ->
        pp_expression e1; pr_space(); pr_binaryOp op; pr_space(); pp_expression e2

    | ArrayAccess (e, es), [i1;i2] ->
        pp_expression e; pr_elem i1;
        pp_arg_list es;
        pr_elem i2

    | RecordAccess   (e, name),     [i1] ->
        pp_expression e; pr_elem i1; pp_name name;
    | RecordPtAccess (e, name),     [i1] ->
        pp_expression e; pr_elem i1; pp_name name;
    | QualifiedAccess(typ, name),   [i1] ->
        do_option pp_type typ; pr_elem i1; pp_name name
            
    | SizeOfExpr  (e),     [i] ->
	pr_elem i;
	(match Ast_c.unwrap e with
	  ParenExpr (e), _ -> ()
	| _ -> pr_space());
	pp_expression e
    | SizeOfType  (t),     [i1;i2;i3] ->
        pr_elem i1; pr_elem i2; pp_type t; pr_elem i3
    | Cast    (t, e),   [i1;i2] ->
        pr_elem i1; pp_type t;
        pr_elem i2; pp_expression e;

    | StatementExpr (statxs, [ii1;ii2]),  [i1;i2] ->
        pr_elem i1;
        pr_elem ii1;
        statxs +> List.iter pp_statement_seq;
        pr_elem ii2;
        pr_elem i2;
    | Constructor (t, init), [lp;rp] ->
        pr_elem lp;
        pp_type t;
        pr_elem rp;
	pp_init init

    | ParenExpr (e), [i1;i2] -> pr_elem i1; pp_expression e; pr_elem i2;

    | New   (pp, t, init),    i1::rest ->
	pr_elem i1; pr_space();
	let rest =
	  default rest
	    (function x ->
	      match rest with
		i2::i3::rest -> 
		  pr_arg_list_with_par (i2,x,i3);
		  rest
              | _ -> failwith "impossible")
	    pp in
	(match init with
	  None ->
	    (match rest with
	      [i4;i5] -> pr_elem i4; pp_type t; pr_elem i5
	    | [] -> pp_type t
	    | _ -> failwith "impossible")
	| Some init ->
	    (match rest with
	      [i4;i5;i6;i7] ->
		pr_elem i4; pp_type t; pr_elem i5;
		pr_arg_list_with_par (i6, init, i7)
	    | [i6;i7] -> pp_type t; pr_arg_list_with_par (i6,init,i7)
	    | _ -> failwith "impossible"))
    | Delete(false,t), [i1] -> pr_elem i1; pr_space(); pp_expression t
    | Delete(true,t), [i1;i2;i3] ->
	pr_elem i1; pr_space(); pr_elem i2; pr_elem i3; pr_space();
	pp_expression t
    | TemplateInst(name,es),[lab;rab] ->
        pp_expression name; pr_elem lab;
	pp_arg_list es;
        pr_elem rab
    | TupleExpr(init), [] ->
	pp_init init
    | Defined name, [i1] ->
        pr_elem i1; (* defined *) pr_space();
        pp_name name;
    | Defined name, [i1;i2;i3] ->
        pr_elem i1; (* defined *) pr_space();
        pr_elem i2; (* ( *)
        pp_name name;
        pr_elem i3; (* ) *)

    | (Ident (_) | Constant _ | StringConstant _ | FunCall (_,_)
    | CondExpr (_,_,_) | Sequence (_,_) | Assignment (_,_,_)
    | Postfix (_,_) | Infix (_,_) | Unary (_,_) | Binary (_,_,_)
    | ArrayAccess (_,_) | RecordAccess (_,_) | RecordPtAccess (_,_) | QualifiedAccess(_,_)
    | SizeOfExpr (_) | SizeOfType (_) | Cast (_,_)
    | StatementExpr (_) | Constructor _
    | ParenExpr (_) | New (_) | Delete (_,_) | TemplateInst(_,_) | TupleExpr(_)
    | Defined (_)),_ -> raise (Impossible 95)
    );

    if !Flag_parsing_c.pretty_print_type_info
    then begin
      pr_text "/*";
      let alt =
	redo (fun x -> pr_text (Ast_c.str_of_info x)) in
      !typ +>
      (fun (ty,_test) -> ty +>
	Common.do_option
	  (fun (x,l) -> alt.ty x;
	    let s = match l with
	      Ast_c.LocalVar _ -> ", local"
	    | _ -> "" in
	    pr_text s));
      pr_text "*/"
    end

  and pr_assignOp (_,ii) =
    let i = Common.tuple_of_list1 ii in
    pr_elem i

  and pr_arg_list_with_par (i1, args, i2) =
    pr_elem i1;
    pp_arg_list args;
    pr_elem i2;
    pr_space()

  and pr_binaryOp (_,ii) =
    let i = Common.tuple_of_list1 ii in
    pr_elem i

  and pp_arg_list es = pp_list pp_argument es

  and pp_argument argument =
    let pp_action (ActMisc ii) = ii +> List.iter pr_elem in
    match argument with
    | Left e -> pp_expression e
    | Right weird ->
	(match weird with
	| ArgType param -> pp_param param
	| ArgAction action -> pp_action action)

(* ---------------------- *)
  and pp_name = function
    | RegularName (s, ii) ->
        let (i1) = Common.tuple_of_list1 ii in
        pr_elem i1

    | Operator(space_needed,op::ii) ->
	pr_elem op;
	(if space_needed then pr_space());
	List.iter pr_elem ii
    | Operator(space_needed,_) ->
	failwith "pretty print: bad operator"

    | QualName xs ->
        xs +> List.iter (fun (nm, ii2) ->
          ii2 +> List.iter pr_elem;
          pp_name nm)

    | CppConcatenatedName xs ->
        xs +> List.iter (fun ((x,ii1), ii2) ->
          ii2 +> List.iter pr_elem;
          ii1 +> List.iter pr_elem;
        )
    | CppVariadicName (s, ii) ->
        ii +> List.iter pr_elem
    | CppIdentBuilder ((s,iis), xs) ->
        let (iis, iop, icp) = Common.tuple_of_list3 iis in
        pr_elem iis;
        pr_elem iop;
        xs +> List.iter (fun ((x,iix), iicomma) ->
          iicomma +> List.iter pr_elem;
          iix +> List.iter pr_elem;
        );
        pr_elem icp

and pp_string_fragment (e,ii) =
  match (e,ii) with
    ConstantFragment(str), ii ->
      let (i) = Common.tuple_of_list1 ii in
      pr_elem i
  | FormatFragment(fmt), ii ->
      let (i) = Common.tuple_of_list1 ii in
      pr_elem i;
      pp_string_format fmt

and pp_string_fragment_list sfl = pp_list2 pp_string_fragment sfl

and pp_string_format (e,ii) =
  match (e,ii) with
    ConstantFormat(str), ii ->
      let (i) = Common.tuple_of_list1 ii in
      pr_elem i

(* ---------------------- *)

  and pp_statement_seq_list statxs =
    statxs +> Common.print_between pr_nl pp_statement_seq

  and pp_statement = fun st ->
    match Ast_c.get_st_and_ii st with
    | Labeled (Label (name, st)), ii ->
        let (i2) = Common.tuple_of_list1 ii in
	pr_outdent(); pp_name name; pr_elem i2; pr_nl(); pp_statement st
    | Labeled (Case  (e, st)), [i1;i2] ->
	pr_unindent();
        pr_elem i1; pr_space(); pp_expression e; pr_elem i2; pr_nl();
	pr_indent(); pp_statement st
    | Labeled (CaseRange  (e, e2, st)), [i1;i2;i3] ->
	pr_unindent();
        pr_elem i1; pr_space(); pp_expression e; pr_elem i2;
	pp_expression e2; pr_elem i3; pr_nl(); pr_indent();
        pp_statement st
    | Labeled (Default st), [i1;i2] ->
	pr_unindent(); pr_elem i1; pr_elem i2; pr_nl(); pr_indent();
	pp_statement st
    | Compound statxs, [i1;i2] ->
        pr_elem i1; start_block(); pp_statement_seq_list statxs;
        end_block(); pr_elem i2;

    | ExprStatement (None), [i] -> pr_elem i;
    | ExprStatement (None), [] -> ()
    | ExprStatement (Some e), [i] -> pp_expression e; pr_elem i
        (* the last ExprStatement of a for does not have a trailing
           ';' hence the [] for ii *)
    | ExprStatement (Some e), [] -> pp_expression e;
    | Selection  (If (e, st1, st2)), i1::i2::i3::is ->
        pp_ifthen e st1 i1 i2 i3;
        pp_else st2 is
    | Selection  (Ifdef_Ite (e, st1, st2)), i1::i2::i3::i4::is ->
        pr_elem i1;
        pp_ifthen e st1 i2 i3 i4;
        pp_else st2 is
    | Selection (Ifdef_Ite2 (e, st1, st2, st3)), i1::i2::i3::i4::is ->
        pr_elem i1;
        pp_ifthen e st1 i2 i3 i4;
        (* else #else S #endif *)
	(match is with
          [i4;i5;i6;iifakend] ->
            pr_elem i4; (* else *)
            pr_elem i5; (* #else *)
            indent_if_needed st2 (function _ -> pp_statement st2);
            pr_elem i6; (* #endif *)
            indent_if_needed st3 (function _ -> pp_statement st3);
            pr_elem iifakend
	| _ -> raise (Impossible 90))
    | Selection (TryCatch (st, cal)), [it;iifakend] ->
        pr_elem it;
        pp_statement st;
        cal +> List.iter (fun ((param,st), ii) ->
          let (ic,lp,rp) = Common.tuple_of_list3 ii in
          pr_elem ic;
          pr_elem lp;
          pp_param param;
          pr_elem rp;
          pp_statement st
        );
        pr_elem iifakend
    | Selection  (Switch (e, st)), [i1;i2;i3;iifakend] ->
        pr_elem i1; pr_space(); pr_elem i2; pp_expression e; pr_elem i3;
	indent_if_needed st (function _-> pp_statement st); pr_elem iifakend
    | Iteration  (While (WhileExp (e), st)), [i1;i2;i3;iifakend] ->
	pr_elem i1; pr_space(); pr_elem i2; pp_expression e; pr_elem i3;
	indent_if_needed st (function _-> pp_statement st); pr_elem iifakend
    | Iteration  (While (WhileDecl (d), st)), [i1;i2;i3;iifakend] ->
	pr_elem i1; pr_space(); pr_elem i2; pp_decl d; pr_elem i3;
	indent_if_needed st (function _-> pp_statement st); pr_elem iifakend
    | Iteration  (ScopedGuard (es, st)), [i1;i2;i3;iifakend] ->
	pr_elem i1; pr_space(); pr_elem i2; pp_arg_list es; pr_elem i3;
	indent_if_needed st (function _-> pp_statement st); pr_elem iifakend
    | Iteration  (DoWhile (st, e)), [i1;i2;i3;i4;i5;iifakend] ->
	pr_elem i1;
	indent_if_needed st (function _ -> pp_statement st);
	pr_elem i2; pr_elem i3; pp_expression e;
        pr_elem i4; pr_elem i5;
        pr_elem iifakend


    | Iteration  (For (first,st)),
        [i1;i2;i3;iifakend] ->

          pr_elem i1; pr_space();
          pr_elem i2;
	  (match first with
	    ForExp ((e1opt,il1),(e2opt,il2),(e3opt, il3)) ->
              pp_statement (Ast_c.mk_st (ExprStatement e1opt) il1);
	      pr_space();
              pp_statement (Ast_c.mk_st (ExprStatement e2opt) il2);
              assert (il3 = []);
	      pr_space();
              pp_statement (Ast_c.mk_st (ExprStatement e3opt) il3)
	  | ForDecl (decl,(e2opt,il2),(e3opt, il3)) ->
	      pp_decl decl;
	      pr_space();
              pp_statement (Ast_c.mk_st (ExprStatement e2opt) il2);
              assert (il3 = []);
	      pr_space();
              pp_statement (Ast_c.mk_st (ExprStatement e3opt) il3)
	  | ForRange(decl,ini) ->
	      pp_decl decl;
	      pr_space();
	      pp_init ini);
          pr_elem i3;
          indent_if_needed st (function _ -> pp_statement st);
          pr_elem iifakend

    | Iteration  (MacroIteration (s,es,st)), [i1;i2;i3;iifakend] ->
        pr_elem i1; pr_space();
        pr_elem i2;

	pp_arg_list es;

        pr_elem i3;
        indent_if_needed st (function _ -> pp_statement st);
        pr_elem iifakend

    | Jump (Goto name), ii               ->
        let (i1, i3) = Common.tuple_of_list2 ii in
        pr_elem i1; pr_space(); pp_name name; pr_elem i3;
    | Jump ((Continue|Break|Return)), [i1;i2] -> pr_elem i1; pr_elem i2;
    | Jump (ReturnExpr e), [i1;i2] ->
	pr_elem i1; pr_space(); pp_expression e; pr_elem i2
    | Jump (GotoComputed e), [i1;i2;i3] ->
        pr_elem i1; pr_elem i2; pp_expression e; pr_elem i3

    | Decl decl, [] -> pp_decl decl
    | Asm asmbody, ii ->
        (match ii with
        | [iasm;iopar;icpar;iptvirg] ->
            pr_elem iasm; pr_elem iopar;
            pp_asmbody asmbody;
            pr_elem icpar; pr_elem iptvirg
        | [iasm;ivolatile;iopar;icpar;iptvirg] ->
            pr_elem iasm; pr_elem ivolatile; pr_elem iopar;
            pp_asmbody asmbody;
            pr_elem icpar; pr_elem iptvirg
        | _ -> raise (Impossible 97)
        )

    | NestedFunc def, ii ->
        assert (ii = []);
        pp_def def
    | MacroStmt, ii ->
        ii +> List.iter pr_elem ;

    | Exec(code), [exec;lang;sem] ->
	pr_elem exec; pr_space(); pr_elem lang; pr_space();
	pp_list2 pp_exec_code code; pr_elem sem

    | IfdefStmt1 (ifdefs, xs), [] ->
	pp_statement_seq
	  (IfdefStmt2 (ifdefs,List.map (fun x -> [StmtElem x]) xs))

    | (Labeled (Case  (_,_))
    | Labeled (CaseRange  (_,_,_)) | Labeled (Default _)
    | Compound _ | ExprStatement _
    | Selection  (If (_, _, _)) | Selection  (Switch (_, _))
    | Selection (Ifdef_Ite _) | Selection (Ifdef_Ite2 _)
    | Selection (TryCatch (_, _))
    | Iteration  (While (_, _)) | Iteration  (DoWhile (_, _))
    | Iteration  (For (_, _))
    | Iteration  (MacroIteration (_,_,_))
    | Iteration  (ScopedGuard (_, _))
    | Jump ((Continue|Break|Return)) | Jump (ReturnExpr _)
    | Jump (GotoComputed _)
    | Decl _ | Exec _ | IfdefStmt1 _
	), _ -> raise (Impossible 98)

  and pp_statement_seq = function
    | StmtElem st -> pp_statement st
    | IfdefStmt ifdef -> pp_ifdef ifdef
    | CppDirectiveStmt cpp -> pp_directive cpp
    | IfdefStmt2 (ifdef, xxs) -> pp_ifdef_tree_sequence ifdef xxs

  and pp_ifthen e st1 i1 i2 i3 =
        (* if (<e>) *)
        pr_elem i1; pr_space(); pr_elem i2; pp_expression e; pr_elem i3;
        indent_if_needed st1 (function _ -> pp_statement st1);
  and pp_else st2 is =
        match (Ast_c.get_st_and_ii st2, is) with
          | ((ExprStatement None, []), [])  -> ()
          | ((ExprStatement None, []), [iifakend])  -> pr_elem iifakend
          | _st2, [i4;iifakend] ->
              pr_elem i4;
              indent_if_needed st2 (function _ -> pp_statement st2);
              pr_elem iifakend
          | _st2, [i4;i5;iifakend] -> (* else #endif S *)
              pr_elem i4;
              pr_elem i5;
              indent_if_needed st2 (function _ -> pp_statement st2);
              pr_elem iifakend
          | x -> raise (Impossible 96)


(* ifdef XXX elsif YYY elsif ZZZ endif *)
  and pp_ifdef_tree_sequence ifdef xxs =
    match ifdef with
    | if1::ifxs ->
        pp_ifdef if1;
        pp_ifdef_tree_sequence_aux  ifxs xxs
    | _ -> raise (Impossible 99)

(* XXX elsif YYY elsif ZZZ endif *)
  and pp_ifdef_tree_sequence_aux ifdefs xxs =
    Common.zip ifdefs xxs +> List.iter (fun (ifdef, xs) ->
      xs +> List.iter pp_statement_seq;
      pp_ifdef ifdef
    )





(* ---------------------- *)
  and pp_asmbody (string_list, colon_list) =
    string_list +> List.iter pr_elem ;
    colon_list +> List.iter (fun (Colon xs, ii) ->
      ii +> List.iter pr_elem;
      xs +> List.iter (fun (x,iicomma) ->
	assert ((List.length iicomma) <= 1);
	iicomma +> List.iter (function x -> pr_elem x; pr_space());
	(match x with
	| ColonMisc, ii -> ii +> List.iter pr_elem;
	| ColonExpr e, [istring;iopar;icpar] ->
            pr_elem istring;
            pr_elem iopar;
            pp_expression e;
            pr_elem icpar
	  (* the following case used to be just raise Impossible, but
	     the code __asm__ __volatile__ ("dcbz 0, %[input]"
                                ::[input]"r"(&coherence_data[i]));
	     in linux-2.6.34/drivers/video/fsl-diu-fb.c matches this case *)
	| (ColonExpr e), ii ->
	    (match List.rev ii with
	      icpar::iopar::istring::rest ->
		List.iter pr_elem (List.rev rest);
		pr_elem istring;
		pr_elem iopar;
		pp_expression e;
		pr_elem icpar
	    | _ -> raise (Impossible 100)))
        ))

  and pp_exec_code = function
    ExecEval name, [colon] -> pr_elem colon; pp_expression name
  | ExecToken, [tok] -> pr_elem tok
  | _ -> raise (Impossible 101)


(* ---------------------- *)

(*
   pp_type_with_ident
   pp_base_type
   pp_type_with_ident_rest
   pp_type_left
   pp_type_right
   pp_type

   pp_decl
*)
  and (pp_type_with_ident:
	 (unit -> unit) option -> (storage * il) option ->
	   fullType -> attribute list ->  unit) =
    fun ident sto ft endattrs ->
      pp_base_type ft sto;
      let hastype =
	match (Ast_c.unwrap_typeC ft,sto) with
	  (NoType,(None|Some((NoSto,false,NoAlign),[]))) ->
	    false
	| (FunctionType((_,_,(NoType,_)),_),
	   (None|Some((NoSto,false,NoAlign),[]))) ->
	    false
	| (FunctionType((_,_,(_,[ity])),_),
	   (None|Some((NoSto,false,NoAlign),[]))) ->
	    str_of_info ity <> ""
	| _ -> true in
      (if hastype
      then
	if ident <> None
	then pr_space()
	else
	  let rec ptrfront ft =
	    match Ast_c.unwrap_typeC ft with
	      Pointer _ -> pr_space()
	    | Array(_,t) -> ptrfront t
	    | _ -> () (* doesn't start with * *) in
	  ptrfront ft);
      pp_type_with_ident_rest ident ft [] endattrs


  and (pp_base_type: fullType -> (storage * il) option -> unit) =
    fun (qu, attr, (ty, iity)) sto ->
      let ii_is_before =
	if iity = []
	then
	  let iis =
	    Lib_parsing_c.ii_of_type(nQ,[],(ty, iity)) in
	  if iis = []
	  then (fun ii -> true)
	  else
	    let ii0 = List.hd iis in
	    (fun ii -> Ast_c.compare_pos ii ii0 < 0)
	else
	  let ii0 = List.hd iity in
	  (fun ii -> Ast_c.compare_pos ii ii0 < 0) in
      let get_aux_positions sto (qu, iiqu) attrs =
	let sto_info =
	  match sto with
	    None -> []
	  | Some (s,iis) ->
	      List.map (fun i -> (i,fun _ -> pr_elem i)) iis in
	let qu_info =
	  List.map (fun i -> (i,fun _ -> pr_elem i)) iiqu in
	let attr_info =
	  List.map
	    (function attr ->
	      let iis = Lib_parsing_c.ii_of_attr attr in
	      (List.hd iis, fun _ -> pp_attribute attr))
	    attrs in
	(* separate before type and after type *)
	List.partition (fun (ii',fn) -> ii_is_before ii')
	  (sto_info @ qu_info @ attr_info) in

      let print_ii_list info =
	List.sort (fun (i1,_) (i2,_) -> Ast_c.compare_pos i1 i2) info +>
	Common.print_between pr_space (fun (_,pr) -> pr()) in

      let print_sto_qu print_type =
	let (beforeii,afterii) = get_aux_positions sto qu attr in
	print_ii_list beforeii; print_type();
	(if afterii <> [] then pr_space());
	print_ii_list afterii in

      let print_order_ty ii =
	let ty = List.map (fun i -> (i,fun _ -> pr_elem i)) ii in
	print_ii_list ty in

      match ty, iity with
      |	(NoType,_) -> ()
      | (Pointer t, [i])                           -> pp_base_type t sto
      | (ParenType t, _)                           -> pp_base_type t sto
      | (Array (eopt, t), [i1;i2])                 -> pp_base_type t sto
      | (FunctionType (returnt, paramst), [i1;i2]) ->
          pp_base_type returnt sto

      | (StructUnion (su, sopt, optfinal, base_classes, fields),iis) ->
          print_sto_qu
	    (fun _ ->
              (match sopt,iis with
              | Some s , [su;id;dotdot;lb;rb] ->
		  pr_elem su; pr_space(); pr_elem id; pr_space();
		  do_option pr_elem optfinal; pr_elem dotdot;
		  pp_list pp_base_class base_classes; pr_space();
		  pr_elem lb
              | Some s , [su;id;lb;rb] ->
		  pr_elem su; pr_space(); pr_elem id; pr_space();
		  do_option pr_elem optfinal; pr_elem lb;
              | None , [su;dotdot;lb;rb] ->
		  pr_elem su; pr_space(); pr_elem dotdot; pr_space();
		  do_option pr_elem optfinal; pr_elem lb;
              | None, [su;lb;rb] ->
		  pr_elem su; pr_space(); do_option pr_elem optfinal; pr_elem lb;
              | x -> raise (Impossible 101));

              fields +> List.iter (fun x -> pr_nl(); pr_indent(); pp_field x);
	      pr_nl();

              (match sopt,iis with
              | Some s , [i1;i2;i3;i4;i5] -> pr_elem i5
              | Some s , [i1;i2;i3;i4] ->    pr_elem i4
              | None, [i1;i2;i3;i4] ->       pr_elem i4
              | None, [i1;i2;i3] ->          pr_elem i3
              | x -> raise (Impossible 102)))

      | (EnumDef  ((_qu,_attr,enident), base, enumt), iis) ->
	  pp_base_type (qu, attr, enident) sto;

	  let rest = 
	    match (base, iis) with
	      (Some base, i4::ii) -> pr_elem i4; pp_base_type base sto; ii
	    | (None, ii) -> ii
	    | (Some _, []) -> raise (Impossible 1030) in

	  (match rest with
	    lb::rb::ii ->
	      pr_space();
	      pr_elem lb;
	      enumt +> List.iter (fun ((name, eopt), iicomma) ->
		assert (List.length iicomma <= 1);
		iicomma +> List.iter (function x -> pr_elem x; pr_space());
		pp_name name;
		eopt +> Common.do_option (fun (ieq, e) ->
		  pr_elem ieq;
		  pp_expression e;
		));
	      (match ii with
		[co] -> pr_elem co
	      | _ -> ());
	      pr_elem rb
	  |  _ -> raise (Impossible 1031))

      | (BaseType _, iis) ->
          print_sto_qu (fun _ -> print_order_ty iis)

      | (StructUnionName (s, structunion), iis) ->
          assert (List.length iis = 2);
          print_sto_qu (fun _ -> print_order_ty iis)

      | (EnumName  (key, s), iis) ->
          print_sto_qu (fun _ -> print_order_ty iis)
  
      | (TypeName (name), iis) ->
          assert (List.length iis = 2);
          print_sto_qu (fun _ -> print_order_ty iis)

      | (Decimal(l,p), [dec;lp;cm;rp]) ->
          print_sto_qu (fun _ -> print_order_ty [dec]);
	  pr_elem lp; pp_expression l; pr_elem cm;
	  do_option pp_expression p; pr_elem rp
      
      | (QualifiedType (typ,name), [iis]) ->
         Common.do_option  pp_type typ; pr_elem iis; pp_name name
      | (QualifiedType (typ,name), _) ->
         Common.do_option  pp_type typ; pp_name name

      | (NamedType (name,typ), noii) ->
          assert (noii = []);
          let (_s, iis) = get_s_and_info_of_name name in
          print_sto_qu
	    (fun _ ->
	      print_order_ty [iis];

              if !Flag_parsing_c.pretty_print_typedef_value
              then begin
		pr_text "{*";
		typ +> Common.do_option (fun typ ->
                  pp_type typ;
		  );
		pr_text "*}"
              end)

      | (FieldType (t, _, _), iis) ->
	  pp_base_type t sto

      | (TypeOfExpr (e), iis) ->
          print_sto_qu
	    (fun _ ->
              match iis with
              | [itypeof;iopar;icpar] ->
		  pr_elem itypeof; pr_elem iopar;
		  pp_expression e;
		  pr_elem icpar;
              | _ -> raise (Impossible 105))

      | (TypeOfType (t), iis) ->
          print_sto_qu
	    (fun _ ->
              match iis with
              | [itypeof;iopar;icpar] ->
		  pr_elem itypeof; pr_elem iopar;
		  pp_type t;
		  pr_elem icpar;
              | _ -> raise (Impossible 106))
      | (AutoType, iis) ->
          print_sto_qu (fun _ -> print_order_ty iis)

      | (TemplateType(name,es),ii) ->
	  let (i1,i2) = Common.tuple_of_list2 ii in
          pp_type name; pr_elem i1;
	  pp_arg_list es;
          pr_elem i2

      | (Pointer _ | Array _ | FunctionType _ | Decimal _), _ -> raise (Impossible 107)

  and pp_field_list fields = fields +>  Common.print_between pr_nl pp_field
  (* common to fields and top-level variables *)
  and firstvar attrs storage var returnType endattrs iisto =
    match var with
    | Some (name, iniopt) -> 
	assert (attrs = []); (* can only be non-null for later variables *)
	pp_type_with_ident
	  (Some (function _ -> pp_name name)) (Some (storage, iisto))
	  returnType endattrs;
	(match iniopt with
	  Ast_c.NoInit -> ()
	| Ast_c.ValInit(init,[iini]) ->
	    pr_space(); pr_elem iini; pr_space(); pp_init init
	| Ast_c.ValInit(init,[]) -> pr_space(); pp_init init
	| Ast_c.ValInit _ -> raise (Impossible 112))
    | None -> pp_type returnType
  and restvar iivirg var returnType attrs endattrs =
    match var with
      Some (name,iniopt) ->
	iivirg +> List.iter pr_elem; pr_space();
	pp_type_with_ident_rest (Some (function _ -> pp_name name))
	  returnType attrs endattrs;
	(match iniopt with
	  Ast_c.NoInit -> ()
	| Ast_c.ValInit(init,[iini]) ->
	    pr_space(); pr_elem iini; pr_space(); pp_init init
	| Ast_c.ValInit(init,[]) -> pr_space(); pp_init init
	| Ast_c.ValInit _ -> raise (Impossible 113))
    | None -> raise (Impossible 113)
  and pp_field = function
      DeclarationField
	(FieldDeclList(onefield_multivars,iiptvirg::ifakestart::iisto)) ->
	pr_elem ifakestart;
        (match onefield_multivars with
          x::xs ->
	    (* handling the first var. Special case, with the
               first var, we print the whole type *)

	    (match x with
	      (Simple (storage, attrs, nameopt, typ, endattrs)), iivirg ->
              (* first var cannot have a preceding ',' *)
		assert (List.length iivirg = 0);
		firstvar attrs storage nameopt typ endattrs iisto

	    | (BitField (nameopt, typ, iidot, expr)), iivirg ->
                      (* first var cannot have a preceding ',' *)
		assert (List.length iivirg = 0);
		(match nameopt with
		| None ->
		    pp_type typ;
		| Some name ->
		    pp_type_with_ident (Some (function _ -> pp_name name))
		      None typ Ast_c.noattr;
		    );
                pr_elem iidot;
		pp_expression expr

                  ); (* match x, first onefield_multivars *)

                      (* for other vars *)
	    xs +> List.iter (function
	      | (Simple (storage, attrs, nameopt, typ, endattrs)), iivirg ->
		  restvar iivirg nameopt typ attrs endattrs

	      | (BitField (nameopt, typ, iidot, expr)), iivirg ->
		  iivirg +> List.iter pr_elem;
		  (match nameopt with
		  | Some name ->
		      pp_type_with_ident_rest
			(Some (function _ -> pp_name name))
			typ Ast_c.noattr Ast_c.noattr;
		      pr_elem iidot;
		      pp_expression expr
		  | None ->
		      (* was raise Impossible, but have no idea why because
			 nameless bit fields are accepted by the parser and
			 nothing seems to be done to give them names *)
		      pr_elem iidot;
		      pp_expression expr
			)); (* iter other vars *)

	| [] -> raise (Impossible 108)
	      ); (* onefield_multivars *)
	pr_elem iiptvirg

    | DeclarationField(FieldDeclList(onefield_multivars,_)) ->
	failwith "wrong number of tokens"

    | MacroDeclField ((s, es, attrs), ii) ->
        let (iis, lp, rp, iiend, ifakestart) =
          Common.tuple_of_list5 ii in
                 (* iis::lp::rp::iiend::ifakestart::iisto
	            iisto +> List.iter pr_elem; (* static and const *)
                 *)
	pr_elem ifakestart;
	pr_elem iis;
	pr_elem lp;
	pp_arg_list es;
	pr_elem rp;
	pp_attributes attrs;
	pr_elem iiend;

    | MacroDeclFieldInit ((s, es, attrs, ini), ii) ->
        let (iis, lp, rp, eq, iiend, ifakestart) =
          Common.tuple_of_list6 ii in
                 (* iis::lp::rp::eq::iiend::ifakestart::iisto
	            iisto +> List.iter pr_elem; (* static and const *)
                 *)
	pr_elem ifakestart;
	pr_elem iis;
	pr_elem lp;
	pp_arg_list es;
	pr_elem rp;
	pp_attributes attrs;
	pr_space(); pr_elem eq; pr_space();
	pp_init ini;
	pr_elem iiend;

    | MacroDeclFieldMarker (s, ii) ->
        let iis = Common.tuple_of_list1 ii in
	pr_elem iis

    | EmptyField iipttvirg_when_emptyfield ->
        pr_elem iipttvirg_when_emptyfield

    | CppDirectiveStruct cpp -> pp_directive cpp
    | IfdefStruct ifdef -> pp_ifdef ifdef

    (* C++ *)
    | FunctionField def -> pp_def def
    | AccSpec ii ->
	let (kwd,dotdot) = Common.tuple_of_list2 ii in
	pr_elem kwd; pr_elem dotdot
    | ConstructDestructField cd -> pp_construct_destruct cd

(* used because of DeclList, in    int i,*j[23];  we don't print anymore the
   int before *j *)
  and (pp_type_with_ident_rest: (unit -> unit) option ->
    fullType -> attribute list -> attribute list -> unit) =

    fun ident (((qu, iiqu), attrs, (ty, iity)) as fullt) extra_attrs endattrs ->

      pp_attributes extra_attrs;

      let print_qu_attr _ =
	let qu_info =
	  List.map (fun i -> (i,fun _ -> pr_elem i)) iiqu in
	let attr_info =
	  List.map
	    (function attr ->
	      let iis = Lib_parsing_c.ii_of_attr attr in
	      (List.hd iis, fun _ -> pp_attribute attr))
	    attrs in
	let info = qu_info @ attr_info in
	List.sort (fun (i1,_) (i2,_) -> Ast_c.compare_pos i1 i2) info +>
	List.iter (function (_,x) -> pr_space(); x()) in

      let print_ident ident = Common.do_option (fun f ->
        (* XXX attrs +> pp_attributes pr_elem pr_space; *)
        f()
	) ident
      in

      (match ty, iity with
      (* the work is to do in base_type !! *)
      | (NoType, iis)                           -> ()
      | (BaseType _, iis)                       -> print_ident ident
      | (EnumDef  (sopt, base, enumt), iis)     -> print_ident ident
      | (StructUnion (_, sopt, optfinal, base_classes, fields),iis) -> print_ident ident
      | (StructUnionName (s, structunion), iis) -> print_ident ident
      | (EnumName  (key, s), iis)               -> print_ident ident
      | (TypeName (name), iis)                   -> print_ident ident
      | (Decimal _, iis)                        -> print_ident ident
      | (QualifiedType(_typ,_name), iis)        -> print_ident ident 
      | (NamedType (_name,_typ), iis)            -> print_ident ident
      | (FieldType (_typ,_,_), iis)             -> print_ident ident
      | (TypeOfExpr (e), iis)                   -> print_ident ident
      | (TypeOfType (e), iis)                   -> print_ident ident
      | (AutoType, _)                           -> print_ident ident
      | (TemplateType _, _)                     -> print_ident ident

      | (Pointer t, [i]) ->
          (* subtil:  void ( *done)(int i)   is a Pointer
             (FunctionType (return=void, params=int i) *)
          (*WRONG I THINK, use left & right function *)
          (* bug: pp_type_with_ident_rest None t;      print_ident ident *)
          pp_type_left t;
          pr_elem i;
	  print_qu_attr();
          if iiqu <> [] || attrs <> [] || get_comments_after i <> []
          then pr_space();
          print_ident ident;
	  pp_type_right t

      (* ugly special case ... todo? maybe sufficient in practice *)
      | (ParenType ttop, [i1;i2]) ->
          (match Ast_c.get_ty_and_ii ttop with
          | (Pointer t2, [ipointer]) ->
              (match t2 with
              | (q2, attrs, (FunctionType t, ii3)) ->

		  pp_type_left (q2, attrs, mk_tybis (FunctionType t) ii3);
		  pr_elem i1;
		  pr_elem ipointer;
		  print_ident ident;
		  pr_elem i2;
		  pp_type_right (q2, attrs, mk_tybis (FunctionType t) ii3);
              | _ ->
                  pr2 "PB PARENTYPE ZARB, I forget about the ()";
                  pp_type_with_ident_rest ident ttop Ast_c.noattr Ast_c.noattr;
              )
          (* another ugly special case *)
          | (Array (eopt,t2 ), [iarray1;iarray2]) ->
              (match Ast_c.get_ty_and_ii t2 with
              | (Pointer t3, [ipointer]) ->
                  (match t3 with
                  | (q3, attrs, (FunctionType t, iifunc)) ->

		      pp_type_left (q3, attrs, mk_tybis (FunctionType t) iifunc);
		      pr_elem i1;
		      pr_elem ipointer;
		      print_ident ident;
		      pr_elem iarray1;
		      do_option pp_expression eopt;
		      pr_elem iarray2;
		      pr_elem i2;
		      pp_type_right (q3, attrs, mk_tybis (FunctionType t) iifunc)
                  | _ ->
                      pr2 "PB PARENTYPE ZARB, I forget about the ()";
                      pp_type_with_ident_rest ident ttop Ast_c.noattr Ast_c.noattr;
                  )
              | _ ->
                  pr2 "PB PARENTYPE ZARB, I forget about the ()";
                  pp_type_with_ident_rest ident ttop Ast_c.noattr Ast_c.noattr;
              )
          | _t ->

              pr2 "PB PARENTYPE ZARB, I forget about the ()";
              pp_type_with_ident_rest ident ttop Ast_c.noattr Ast_c.noattr;
          )


      | (Array (eopt, t), [i1;i2]) ->
          pp_type_left fullt;

          print_qu_attr();
          print_ident ident;

          pp_type_right fullt;


      | (FunctionType (returnt, paramst), [i1;i2]) ->
          pp_type_left fullt;

          print_qu_attr();
          print_ident ident;

          pp_type_right fullt;


      | (FunctionType _ | Array _ | ParenType _ | Pointer _), _ ->
	  raise (Impossible 109));
      pp_attributes endattrs

  and (pp_type_left: fullType -> unit) =
    fun ((qu, iiqu), attr, (ty, iity)) ->
      let print_qu_attr _ =
	let qu_info =
	  List.map (fun i -> (i,fun _ -> pr_elem i)) iiqu in
	let attr_info =
	  List.map
	    (function attr ->
	      let iis = Lib_parsing_c.ii_of_attr attr in
	      (List.hd iis, fun _ -> pp_attribute attr))
	    attr in
	let info = qu_info @ attr_info in
	List.sort (fun (i1,_) (i2,_) -> Ast_c.compare_pos i1 i2) info +>
	List.iter (function (_,x) -> pr_space(); x()) in

      match ty, iity with
	(NoType,_) -> failwith "pp_type_left: unexpected NoType"
      | (Pointer t, [i]) ->
          pp_type_left t;
          pr_elem i;
	  print_qu_attr();
          if iiqu <> [] || attr <> [] || get_comments_after i <> []
          then pr_space()

      | (Array (eopt, t), [i1;i2]) -> pp_type_left t
      | (FunctionType (returnt, paramst), [i1;i2]) -> pp_type_left returnt

      | (ParenType t, _) ->  failwith "pp_type_left: unexpected parenType"


      | (BaseType _, iis)    -> ()
      | (EnumDef  (sopt, base, enumt), iis) -> ()
      | (StructUnion (_, sopt, _, _, fields),iis)  -> ()
      | (StructUnionName (s, structunion), iis) -> ()
      | (EnumName  (key, s), iis) -> ()
      | (TypeName (name), iis) -> ()
      | (Decimal(l,p), iis) -> ()
      | (NamedType (_name,_typ), iis) -> ()
      | (QualifiedType(_typ,_name), iis) -> ()
      | FieldType (_, _, _), _ -> ()
      | TypeOfType _, _ -> ()
      | TypeOfExpr _, _ -> ()
      | AutoType, _ -> ()
      | TemplateType _, _ -> ()

      | (FunctionType _ | Array _ | Pointer _), _ -> raise (Impossible 110)


  and pp_param param =
    let {p_namei = nameopt;
	  p_register = (b,iib);
	  p_type=t;
	  p_endattr=endattr} = param in

    iib +> List.iter pr_elem;

    match nameopt with
    | None ->
        pp_type t
    | Some name ->
	pp_type_with_ident (Some (function _ -> pp_name name))
	  None t endattr

  and pp_params (ts, (b, iib)) =
    pp_param_list ts;
    iib +> List.iter pr_elem

  and pp_type_right (((qu, iiqu), attrs, (ty, iity)) : fullType) =
    match ty, iity with
      (NoType,_) -> failwith "pp_type_right: unexpected NoType"
    | (Pointer t, [i]) ->  pp_type_right t

    | (Array (eopt, t), [i1;i2]) ->
        pr_elem i1;
        eopt +> do_option pp_expression;
        pr_elem i2;
        pp_type_right t

    | (ParenType t, _) ->  failwith "parenType"
    | (FunctionType (returnt, paramst), [i1;i2]) ->
        pr_elem i1; pp_params paramst;
        pr_elem i2

    | (BaseType _, iis)        -> ()
    | (EnumDef  (sopt, base, enumt), iis) -> ()
    | (StructUnion (_, sopt, _, _, fields),iis)-> ()
    | (StructUnionName (s, structunion), iis) -> ()
    | (EnumName  (key, s), iis) -> ()
    | (TypeName (name), iis) -> ()
    | (Decimal(l,p), iis) -> ()
    | (NamedType (name,_typ), iis) -> ()
    | (QualifiedType(_typ,name), iis) -> ()
    | (FieldType (_, _, _), _) -> ()

    | TypeOfType _, _ -> ()
    | TypeOfExpr _, _ -> ()
    | AutoType, _ -> ()
    | TemplateType _, _ -> ()

    | (FunctionType _ | Array _ | Pointer _), _ -> raise (Impossible 111)

  and pp_type t =
    pp_type_with_ident None None t Ast_c.noattr
  and pp_type_ident t i =
    pp_type_with_ident (Some i) None t Ast_c.noattr
  and pp_type_ident_rest t i =
    pp_type_with_ident_rest (Some i) t Ast_c.noattr Ast_c.noattr
  and pp_base_type2 t =
    pp_base_type t None

(* ---------------------- *)
  and pp_decl = function
    | DeclList ((({v_namei = var;
                   v_type = returnType;
                   v_storage = storage;
                   v_attr = attrs;
                   v_endattr = endattrs;
                  },[])::xs, has_ender),
	       vfs) ->

        let (iivirg,ifakestart,iisto) =
          match vfs with
            iivirg::ifakestart::iisto when has_ender -> (Some iivirg,ifakestart,iisto)
          |         ifakestart::iisto -> (None,ifakestart,iisto)
	  | _ -> failwith "UsingTypename: wrong number of elements" in
        pr_elem ifakestart;
        (* old: iisto +> List.iter pr_elem; *)

        (* handling the first var. Special case, we print the whole type *)
	firstvar attrs storage var returnType endattrs iisto;

      (* for other vars, we just call pp_type_with_ident_rest. *)
	xs +> List.iter (function
	    ({v_namei = var;
	       v_type = returnType;
	       v_storage = storage2;
	       v_attr = attrs;
	       v_endattr = endattrs;
	     }, iivirg) ->
	       assert (storage2 = storage);
	       restvar iivirg var returnType attrs endattrs);

	do_option pr_elem iivirg;

    | MacroDecl
      ((sto, preattrs, s, es, attrs, true), iis::lp::rp::iiend::ifakestart::iisto) ->
	pr_elem ifakestart;
	iisto +> List.iter pr_elem; (* static and const *)
	pp_attributes preattrs;
	pr_elem iis;
	pr_elem lp;
	pp_arg_list es;
	pr_elem rp;
	pp_attributes attrs;
	pr_elem iiend;

    | MacroDecl
      ((sto, preattrs, s, es, attrs, false), iis::lp::rp::ifakestart::iisto) ->
	pr_elem ifakestart;
	iisto +> List.iter pr_elem; (* static and const *)
	pp_attributes preattrs;
	pr_elem iis;
	pr_elem lp;
	pp_arg_list es;
	pr_elem rp;
	pp_attributes attrs;

    | MacroDeclInit
	((sto, preattrs, s, es, attrs, ini), iis::lp::rp::eq::iiend::ifakestart::iisto) ->
	pr_elem ifakestart;
	iisto +> List.iter pr_elem; (* static and const *)
	pp_attributes preattrs;
	pr_elem iis;
	pr_elem lp;
	pp_arg_list es;
	pr_elem rp;
	pp_attributes attrs;
	pr_space(); pr_elem eq; pr_space();
	pp_init ini;
	pr_elem iiend;

    | (DeclList (_, _) | (MacroDecl _) | (MacroDeclInit _)) ->
	raise (Impossible 115)

(* ---------------------- *)
and pp_init (init, iinit) =
  match init, iinit with
      | InitExpr e, [] -> pp_expression e;
      | InitList xs, i1::i2::iicommaopt ->
          pr_elem i1; start_block();
          xs +> List.iter (fun (x, ii) ->
            assert (List.length ii <= 1);
            ii +> List.iter (function e -> pr_elem e; pr_nl());
            pp_init x
          );
          iicommaopt +> List.iter pr_elem;
	  end_block();
          pr_elem i2;
      | InitListNoBrace xs, iicommaopt ->
          start_block();
          xs +> List.iter (fun (x, ii) ->
            assert (List.length ii <= 1);
            ii +> List.iter (function e -> pr_elem e; pr_nl());
            pp_init x
          );
          iicommaopt +> List.iter pr_elem;
	  end_block()

      | InitDesignators (xs, initialiser), [i1] -> (* : *)
          xs +> List.iter pp_designator;
          pr_space(); pr_elem i1; pr_space();
          pp_init initialiser

    (* no use of '=' in the "Old" style *)
      | InitFieldOld (string, initialiser), [i1;i2] -> (* label:   in oldgcc *)
          pr_elem i1; pr_elem i2; pp_init initialiser
      | InitIndexOld (expression, initialiser), [i1;i2] -> (* [1] in oldgcc *)
          pr_elem i1; pp_expression expression; pr_elem i2;
          pp_init initialiser

      | (InitIndexOld _ | InitFieldOld _ | InitDesignators _
      | InitList _ | InitExpr _
	  ), _ -> raise (Impossible 116)

  and pp_init_list (newlines, ini) =
    match newlines with
      Ast_c.Keep -> pp_nl_list pp_init ini
    | Ast_c.Compress -> pp_list pp_init ini

  and pp_designator = function
    | DesignatorField (s), [i1; i2] ->
	pr_elem i1; pr_elem i2;
    | DesignatorIndex (expression), [i1;i2] ->
	pr_elem i1; pp_expression expression; pr_elem i2;

    | DesignatorRange (e1, e2), [iocro;iellipsis;iccro] ->
	pr_elem iocro; pp_expression e1; pr_elem iellipsis;
	pp_expression e2; pr_elem iccro;

    | (DesignatorField _ | DesignatorIndex _ | DesignatorRange _
	), _ -> raise (Impossible 117)


(* ---------------------- *)
  and pp_attributes attrs =
    if not (attrs = []) then pr_space();
    Common.print_between pr_space pp_attribute attrs;

  and pp_attribute (e,ii) =
    match (e,ii) with
      Attribute(a), ii  ->
        pp_attr_arg a
    | GccAttribute(args), ii ->
        let (i1,i2,i3,i4,i5) = Common.tuple_of_list5 ii in
        pr_elem i1; pr_elem i2; pr_elem i3;
        pp_arg_list args; pr_elem i4; pr_elem i5
    | CxxAttribute(args), ii ->
        let (i1,i2,i3) = Common.tuple_of_list3 ii in
        pr_elem i1; pp_arg_list args; pr_elem i2; pr_elem i3
    | CxxAttributeUsing(atnm, args), ii ->
        let (i1,i2,i3,i4,i5) = Common.tuple_of_list5 ii in
        pr_elem i1; pr_elem i2; pp_name atnm; pr_elem i3; pp_arg_list args;
	pr_elem i4; pr_elem i5

  and pp_attr_arg (e,ii) =
    match (e,ii) with
      MacroAttr(a), ii ->
        let (i) = Common.tuple_of_list1 ii in
        pr_elem i
    | MacroAttrArgs(attr, args), ii ->
        let (i1,i2,i3) = Common.tuple_of_list3 ii in
        pr_elem i1; pr_elem i2;
	pp_arg_list args;
        pr_elem i3;

(* ---------------------- *)
  and pp_def_start defbis iifunc1 iifunc2 ifakestart isto =
    let {f_name = name;
          f_type = (returnt, (paramst, (b, iib)));
          f_storage = sto;
	  f_constr_inherited = constr_inh;
          f_body = statxs;
	} = defbis in
    let (idotdot,isto) =
      if !Flag.c_plus_plus = Flag.Off
      then ([],isto)
      else
	match constr_inh with
	  [] -> ([],isto)
	| _ -> ([List.hd isto],List.tl isto) in
    pr_elem ifakestart;

    pp_type_with_ident None (Some (sto, isto)) returnt Ast_c.noattr;

    pp_name name;

    pr_elem iifunc1;
    pp_param_list paramst;
    iib +> List.iter pr_elem;
    pr_elem iifunc2;

    if !Flag.c_plus_plus = Flag.Off && constr_inh <> []
    then
      begin
	pr_elem (List.hd idotdot);
	pp_list pp_expression constr_inh
      end

  and pp_def def =
    let defbis, ii = def in
    match ii with
    | iifunc1::iifunc2::i1::i2::ifakestart::ifakeend::isto ->
	pp_def_start defbis iifunc1 iifunc2 ifakestart isto;
	pr_space();
        pr_elem i1;
	pp_statement_seq_list defbis.f_body;
        pr_elem i2;
        pr_elem ifakeend
    | _ -> raise (Impossible 118)

  and pp_fun_header def =
    let defbis, ii = def in
    match ii with
    | iifunc1::iifunc2::ifakestart::isto ->
	pp_def_start defbis iifunc1 iifunc2 ifakestart isto
    | _ -> raise (Impossible 1180)

  and pp_param_list paramst = pp_list pp_param paramst

  and pp_construct_destruct (cd,ii) =
    let constructor_start vrtl iis lp paramst rp constr_init final =
      pr_elem iis;
      pr_elem lp; pp_params paramst; pr_elem rp;
      (match constr_init with
	(inits,[i1]) ->
	  let pp_init ((name,args),parens) =
	    let (lp,rp) = Common.tuple_of_list2 parens in
	    pp_name name; pr_elem lp; pp_arg_list args; pr_elem rp in
	  pr_space(); pr_elem i1; pr_space();
	  pp_list pp_init inits
      | _ -> ());
      snd vrtl +> List.iter pr_elem;
      snd final +> List.iter pr_elem in
    match cd with
    | ConstructorDecl (vrtl, s, paramst, final)  ->
	let (iis, lp, rp, iiend, ifakestart) = Common.tuple_of_list5 ii in
                 (* iis::lp::rp::iiend::ifakestart::iisto
	            iisto +> List.iter pr_elem; (* static and const *)
                 *)
	pr_elem ifakestart;
	constructor_start vrtl iis lp paramst rp ([],[]) final;
	pr_elem iiend
    | DestructorDecl (vrtl, s, paramst, final)  ->
	let (itil, iis, lp, rp, iiend, ifakestart) = Common.tuple_of_list6 ii in
                 (* itil::iis::lp::rp::iiend::ifakestart::iisto
	            iisto +> List.iter pr_elem; (* static and const *)
                 *)
	pr_elem ifakestart;
	pr_elem itil;
	constructor_start vrtl iis lp paramst rp ([],[]) final;
	pr_elem iiend
    | ConstructorDef (vrtl, s, paramst, constr_init, final, body)  ->
	let (iis, lp, rp, iilb, iirb, ifakestart) = Common.tuple_of_list6 ii in
	pr_elem ifakestart;
	constructor_start vrtl iis lp paramst rp constr_init final;
	pr_elem iilb; start_block(); pp_statement_seq_list body;
        end_block(); pr_elem iirb
    | DestructorDef (vrtl, s, paramst, final, body)  ->
	let (itil, iis, lp, rp, iilb, iirb, ifakestart) = Common.tuple_of_list7 ii in
	pr_elem ifakestart;
	pr_elem itil;
	constructor_start vrtl iis lp paramst rp ([],[]) final;
	pr_elem iilb; start_block(); pp_statement_seq_list body;
        end_block(); pr_elem iirb

(* ---------------------- *)

  and pp_ifdef ifdef =
    match ifdef with
    | IfdefDirective (ifdef, ii) ->
	List.iter pr_elem ii


  and pp_directive = function
    | Include {i_include = (s, ii);} ->
	let (i1,i2) = Common.tuple_of_list2 ii in
	pr_elem i1; pr_space(); pr_elem i2
    | Define ((s,ii), (defkind, defval)) ->
	let (idefine,iident,ieol) = Common.tuple_of_list3 ii in
	pr_elem idefine; pr_space();
	pr_elem iident; pr_space();

	let define_val = function
          | DefineExpr e -> pp_expression e
          | DefineStmt st -> pp_statement st
          | DefineDoWhileZero ((st,e), ii) ->
              (match ii with
              | [ido;iwhile;iopar;icpar] ->
                  pr_elem ido;
                  pp_statement st;
                  pr_elem iwhile; pr_elem iopar;
                  pp_expression e;
                  pr_elem icpar
              | _ -> raise (Impossible 119)
	      )
          | DefineFunction def -> pp_def def

          | DefineType ty -> pp_type ty
          | DefineAttr a -> pp_attributes a
          | DefineText (s, ii) -> List.iter pr_elem ii
          | DefineEmpty -> ()
          | DefineInit ini -> pp_init ini
	  | DefineMulti ss ->
	      ss +> List.iter pp_statement
          | DefineTodo -> pr2 "DefineTodo"
	in
	(match defkind with
	| DefineVar | Undef -> ()
	| DefineFunc (params, ii) ->
            let (i1,i2) = tuple_of_list2 ii in
            pr_elem i1;
	    pp_define_param_list params;
            pr_elem i2;
	);
	define_val defval;
	pr_elem ieol

    | Pragma((name,rest), ii) ->
	let (ipragma,ieol) = Common.tuple_of_list2 ii in
	pr_elem ipragma; pr_space();
	pp_name name; pr_space(); pr_elem rest; pr_elem ieol

    | OtherDirective (ii) ->
	List.iter pr_elem ii

    | UsingTypename((name,def),ii) ->
	let (iusing,ieq,itypename,iptvirg) =
	  match ii with
	    [iusing;ieq;itypename;iptvirg] ->
	      let itypename _ = pr_elem itypename; pr_space() in
	      (iusing,ieq,itypename,iptvirg)
	  | [iusing;ieq;iptvirg] -> (iusing,ieq,(fun _ -> ()),iptvirg)
	  | _ -> failwith "UsingTypename: wrong number of elements" in
	pr_elem iusing; pr_space(); pp_name name; pr_space();
	pr_elem ieq;  pr_space(); itypename();
	pp_type def; pr_elem iptvirg

    | UsingMember(name,ii) ->
	let (iusing,iptvirg) = Common.tuple_of_list2 ii in
	pr_elem iusing; pr_space(); pp_name name;
	pr_elem iptvirg
    | UsingNamespace(name,ii) ->
	let (iusing,nmspc,iptvirg) = Common.tuple_of_list3 ii in
	pr_elem iusing; pr_space(); pp_name name;
	pr_elem iptvirg

  and pp_define_param_list dparams =
    pp_list (fun (s,iis) -> iis +> List.iter pr_elem) dparams

  and pp_base_class (bc,ii) =
    match bc with
      ClassName name -> pp_name name
    | CPublic name | CProtected name | CPrivate name ->
	let tag = Common.tuple_of_list1 ii in
	pr_elem tag; pr_space(); pp_name name in

  let rec pp_toplevel = function
    | Declaration decl -> pp_decl decl
    | Definition def -> pp_def def

    | CppTop directive -> pp_directive directive

    | MacroTop (s, es,   [i1;i2;i3;i4]) ->
	pr_elem i1;
	pr_elem i2;
	pp_arg_list es;
	pr_elem i3;
	pr_elem i4;

    | EmptyDef ii -> ii +> List.iter pr_elem
    | NotParsedCorrectly ii ->
	assert (List.length ii >= 1);
	ii +> List.iter pr_elem
    | FinalDef info -> pr_elem (Ast_c.rewrap_str "" info)

    | IfdefTop ifdefdir -> pp_ifdef ifdefdir

    | Namespace (tls, [i1; i2; i3; i4]) ->
	pr_elem i1; pr_elem i2; pr_elem i3;
	List.iter pp_toplevel tls;
	pr_elem i4
    | Namespace (tls, [i1; i2; i3]) ->
	pr_elem i1; pr_elem i2;
	List.iter pp_toplevel tls;
	pr_elem i3

    | TemplateDefinition(params,defn,ii) ->
	let (i1,i2,i3) = Common.tuple_of_list3 ii in
	pr_elem i1; pr_space(); pr_elem i2;
	pp_template_param_list params; pr_elem i3; pr_nl();
	pp_toplevel defn
    | (MacroTop _) | (Namespace _) -> raise (Impossible 120)

  and pp_template_param_list paramst = pp_list pp_template_param paramst

  and pp_template_param = function
      TypenameOrClassParam((nm,tyopt),ii) ->
	pr_elem (List.hd ii); pp_name nm;
	(match tyopt with
	  None -> ()
	| Some ty -> pr_elem (List.nth ii 1); pp_type ty)
    | VarNameParam((ty,nm,expopt),ii) ->
	pp_type_with_ident
	  (Some
	     (function _ ->
	       pp_name nm;
	       match expopt with
		 None -> ()
	       | Some ini ->
		   let i1 = Common.tuple_of_list1 ii in
		   pr_space(); pr_elem i1; pr_space();
		   pp_init ini))
	  None ty []
    | TemplateParam((params,tmp),ii) ->
	let (i1,i2,i3) = Common.tuple_of_list3 ii in
	pr_elem i1; pr_space(); pr_elem i2;
	pp_template_param_list params; pr_elem i3; pr_space();
	pp_template_param tmp in

  let pp_flow n =
    match F.unwrap n  with
    | F.FunHeader (({f_name =idb;
                      f_type = (rett, (paramst,(isvaargs,iidotsb)));
                      f_storage = stob;
                      f_body = body},ii) as def) ->

			assert (body = []);
			pp_fun_header def


    | F.Decl decl -> pp_decl decl

    | F.ExprStatement (st, (eopt, ii)) ->
	pp_statement (Ast_c.mk_st (ExprStatement eopt) ii)

    | F.IfHeader (_, (e,ii)) ->
	let (i1,i2,i3) = tuple_of_list3 ii in
	pr_elem i1; pr_space(); pr_elem i2; pp_expression e; pr_elem i3
    | F.SwitchHeader (_, (e,ii)) ->
	let (i1,i2,i3) = tuple_of_list3 ii in
	pr_elem i1; pr_space(); pr_elem i2; pp_expression e; pr_elem i3
    | F.WhileHeader (_, (WhileExp e,ii)) ->
	let (i1,i2,i3) = tuple_of_list3 ii in
	pr_elem i1; pr_space(); pr_elem i2; pp_expression e; pr_elem i3
    | F.WhileHeader (_, (WhileDecl d,ii)) ->
	let (i1,i2,i3) = tuple_of_list3 ii in
	pr_elem i1; pr_space(); pr_elem i2; pp_decl d; pr_elem i3
    | F.DoWhileTail (e,ii) ->
	let (i1,i2,i3,i4) = tuple_of_list4 ii in
	pr_elem i1; pr_elem i2; pp_expression e;
	pr_elem i3; pr_elem i4
    | F.ScopedGuardHeader (_, (es,ii)) ->
	let (i1,i2,i3) = tuple_of_list3 ii in
	pr_elem i1; pr_space(); pr_elem i2; pp_arg_list es; pr_elem i3

    | F.ForHeader (_st, ((first), ii)) ->
	let (i1,i2,i3) = tuple_of_list3 ii in
	pr_elem i1; pr_space();
	pr_elem i2;
	(match first with
	  ForExp ((e1opt,il1), (e2opt,il2), (e3opt,il3)) ->
	    pp_statement (Ast_c.mk_st (ExprStatement e1opt) il1);
	    pr_space();
	    pp_statement (Ast_c.mk_st (ExprStatement e2opt) il2);
	    assert (il3 = []);
	    pr_space();
	    pp_statement (Ast_c.mk_st (ExprStatement e3opt) il3)
	| ForDecl (decl, (e2opt,il2), (e3opt,il3)) ->
	    pp_decl decl;
	    pr_space();
	    pp_statement (Ast_c.mk_st (ExprStatement e2opt) il2);
	    assert (il3 = []);
	    pr_space();
	    pp_statement (Ast_c.mk_st (ExprStatement e3opt) il3)
	| ForRange (decl, ini) ->
	    pp_decl decl; pr_space(); pp_init ini);
	pr_elem i3

    | F.MacroIterHeader (_s, ((s,es), ii)) ->
	let (i1,i2,i3) = tuple_of_list3 ii in
	pr_elem i1; pr_space();
	pr_elem i2;
	pp_arg_list es;
	pr_elem i3

    | F.ReturnExpr (_st, (e,ii)) ->
	let (i1,i2) = tuple_of_list2 ii in
	pr_elem i1; pr_space(); pp_expression e; pr_elem i2

    | F.Case (_st, (e,ii)) ->
	let (i1,i2) = tuple_of_list2 ii in
	pr_elem i1; pr_space(); pp_expression e; pr_elem i2

    | F.CaseRange (_st, ((e1, e2),ii)) ->
	let (i1,i2,i3) = tuple_of_list3 ii in
	pr_elem i1; pr_space(); pp_expression e1; pr_elem i2;
	pp_expression e2; pr_elem i3

    | F.CaseNode i -> ()

    | F.DefineExpr e  -> pp_expression e

    | F.DefineType ft  -> pp_type ft

    | F.DefineAttr a  -> pp_attributes a

    | F.DefineHeader ((s,ii), (defkind))  ->
        let (idefine,iident,ieol) = Common.tuple_of_list3 ii in
	pr_elem idefine; pr_space();
	pr_elem iident; pr_space();
	(match defkind with
	| DefineVar | Undef -> ()
	| DefineFunc (params, ii) ->
	    let (i1,i2) = tuple_of_list2 ii in
	    pr_elem i1;
	    pp_define_param_list params;
	    pr_elem i2)

    | F.DefineDoWhileZeroHeader (((),ii)) ->
	(* not sure what it is, ignore *)
        (* iif ii *)
	pr2 "DefineDoWhileZeroHeader"

    | F.DefineInit ini -> pp_init ini
    | F.CppTop(di) -> pp_directive di
    | F.TemplateHeader(params,ii) ->
	let (itmp,ileft,iright) = Common.tuple_of_list3 ii in
	pr_elem itmp; pr_elem ileft;
	pp_template_param_list params; pr_elem iright; pr_nl()

    | F.Include ({i_include = (s, ii);} as a) ->
	pp_directive (Include a)

    | F.MacroTop (s, args, ii) ->
	pp_toplevel(MacroTop (s, args, ii))

    | F.Break    (st,((),ii),fromswitch) ->
	let (i1,i2) = tuple_of_list2 ii in
	pr_elem i1; pr_elem i2
    | F.Continue (st,((),ii)) ->
	let (i1,i2) = tuple_of_list2 ii in
	pr_elem i1; pr_elem i2
    | F.Default  (st,((),ii)) ->
	let (i1,i2) = tuple_of_list2 ii in
	pr_elem i1; pr_elem i2
    | F.Return   (st,((),ii)) ->
	let (i1,i2) = tuple_of_list2 ii in
	pr_elem i1; pr_elem i2
    | F.Goto  (st, name, ((),ii)) ->
	let (i1, i3) = Common.tuple_of_list2 ii in
	pr_elem i1; pr_space(); pp_name name; pr_elem i3
    | F.Label (st, name, ((),ii)) ->
	let (i2) = Common.tuple_of_list1 ii in
	pp_name name; pr_elem i2
    | F.TryHeader (_,i1) ->
	pr_elem i1
    | F.CatchHeader (param,ii) ->
	let (i1, i2, i3) = Common.tuple_of_list3 ii in
	pr_elem i1; pr_space();
	pr_elem i2; pp_param param; pr_elem i3
    | F.EndStatement iopt ->
        (* do_option infof iopt *)
	pr2 "EndStatement"
    | F.DoHeader (st, info) ->
	pr_elem info
    | F.Else info ->
	pr_elem info
    | F.SeqEnd (i, info) ->
	pr_elem info
    | F.SeqStart (st, i, info) ->
	pr_elem info

    | F.MacroStmt (st, ((),ii)) ->
	pp_statement (MacroStmt,ii)
    | F.Asm (st, (asmbody,ii)) ->
	pp_statement (Asm asmbody, ii)
    | F.NestedFunc (st, (def,ii)) ->
	pp_statement (NestedFunc def, ii)

    | F.Exec(st,(code,ii)) ->
	pp_statement (Exec code, ii)

    | F.IfdefHeader (info) ->
	pp_ifdef info
    | F.IfdefElse (info) ->
	pp_ifdef info
    | F.IfdefEndif (info) ->
	pp_ifdef info

    | F.IfdefIteHeader _ii ->
        pr2 "IfdefIteHeader"

    | F.DefineTodo ->
	pr2 "DefineTodo"


    | F.TopNode -> pr2 "TopNode"
    | F.EndNode -> pr2 "EndNode"
    | F.ErrorExit -> pr2 "ErrorExit"
    | F.CatchExit -> pr2 "CatchExit"
    | F.PreExit _ -> pr2 "PreExit"
    | F.Exit -> pr2 "Exit"
    | F.Enter -> pr2 "Enter"
    | F.LoopFallThroughNode -> pr2 "LoopFallThroughNode"
    | F.FallThroughNode -> pr2 "FallThroughNode"
    | F.AfterNode _ -> pr2 "AfterNode"
    | F.FalseNode -> pr2 "FalseNode"
    | F.TrueNode _ -> pr2 "TrueNode"
    | F.InLoopNode -> pr2 "InLoopNode"
    | F.Fake -> pr2 "Fake" in


  { expression = pp_expression;
    assignOp   = pr_assignOp;
    binaryOp   = pr_binaryOp;
    arg_list   = pp_arg_list;
    arg        = pp_argument;
    statement  = pp_statement;
    statement_seq_list = pp_statement_seq_list;
    decl       = pp_decl;
    field      = pp_field;
    field_list = pp_field_list;
    init       = pp_init;
    init_list  = pp_init_list;
    param      = pp_param;
    paramlist  = pp_param_list;
    template_param     = pp_template_param;
    template_paramlist = pp_template_param_list;
    dparamlist = pp_define_param_list;
    ty         = pp_type;
    type_with_ident = pp_type_ident;
    type_with_ident_rest = pp_type_ident_rest;
    base_type  = pp_base_type2;
    toplevel   = pp_toplevel;
    fragment   = pp_string_fragment;
    fragment_list = pp_string_fragment_list;
    attribute  = pp_attribute;
    attr_arg   = pp_attr_arg;
    format     = pp_string_format;
    flow       = pp_flow;
    name       = pp_name;
  } in
  redo pr_elem

(*****************************************************************************)

(* Here we do not use (mcode, env). It is a simple C pretty printer. *)
let pr_elem info =
  let s = Ast_c.str_of_info info in
  if !Flag_parsing_c.pretty_print_comment_info
  then
    (match get_comments_before info with
      [] -> ()
    | before ->
	pp "-->";
	before +> List.iter (fun (comment_like, pinfo) ->
          let s = pinfo.Common.str in
          pp s
	    );
	pp "<--");
  pp s;
  if !Flag_parsing_c.pretty_print_comment_info
  then
    (match get_comments_after info with
      [] -> ()
    | before ->
	pp "==>";
	before +> List.iter (fun (comment_like, pinfo) ->
          let s = pinfo.Common.str in
          pp s
	    );
	pp "<==")

let pr_space _ = Format.print_space()

let pr_nl _ = ()
let pr_indent _ = ()
let pr_outdent _ = ()
let pr_unindent _ = ()


let ppc =
  mk_pretty_printers
    ~pr_elem ~pr_space ~pr_nl ~pr_outdent ~pr_indent ~pr_unindent

let pp_expression_simple = ppc.expression
let pp_arg_list_simple   = ppc.arg_list
let pp_assignOp_simple   = ppc.assignOp
let pp_binaryOp_simple   = ppc.binaryOp
let pp_decl_simple       = ppc.decl
let pp_field_simple      = ppc.field
let pp_statement_simple  = ppc.statement
let pp_statement_seq_list_simple  = ppc.statement_seq_list
let pp_type_simple       = ppc.ty
let pp_init_simple       = ppc.init
let pp_toplevel_simple   = ppc.toplevel
let pp_string_fragment_simple = ppc.fragment
let pp_string_format_simple = ppc.format
let pp_attribute_simple  = ppc.attribute
let pp_attr_arg_simple   = ppc.attr_arg
let pp_flow_simple       = ppc.flow
let pp_name              = ppc.name


let pp_elem_sp ~pr_elem ~pr_space =
  mk_pretty_printers
    ~pr_elem ~pr_space
    ~pr_nl ~pr_outdent ~pr_indent ~pr_unindent

let pp_elem_sp_nl ~pr_elem ~pr_space ~pr_nl =
  mk_pretty_printers
    ~pr_elem ~pr_space
    ~pr_nl ~pr_outdent ~pr_indent ~pr_unindent

let pp_expression_gen ~pr_elem ~pr_space =
  (pp_elem_sp ~pr_elem:pr_elem ~pr_space:pr_space).expression

let pp_assignOp_gen ~pr_elem ~pr_space =
  (pp_elem_sp ~pr_elem:pr_elem ~pr_space:pr_space).assignOp

let pp_binaryOp_gen ~pr_elem ~pr_space =
  (pp_elem_sp ~pr_elem:pr_elem ~pr_space:pr_space).binaryOp

let pp_arg_list_gen ~pr_elem ~pr_space =
  (pp_elem_sp ~pr_elem:pr_elem ~pr_space:pr_space).arg_list

let pp_arg_gen ~pr_elem ~pr_space =
  (pp_elem_sp ~pr_elem:pr_elem ~pr_space:pr_space).arg

let pp_statement_gen ~pr_elem ~pr_space =
  (pp_elem_sp ~pr_elem:pr_elem ~pr_space:pr_space).statement

let pp_statement_seq_list_gen ~pr_elem ~pr_space ~pr_nl =
  (pp_elem_sp_nl ~pr_elem:pr_elem ~pr_space:pr_space ~pr_nl:pr_nl).statement_seq_list

let pp_decl_gen ~pr_elem ~pr_space =
  (pp_elem_sp ~pr_elem:pr_elem ~pr_space:pr_space).decl

let pp_field_gen ~pr_elem ~pr_space =
  (pp_elem_sp ~pr_elem:pr_elem ~pr_space:pr_space).field

let pp_field_list_gen ~pr_elem ~pr_space =
  (pp_elem_sp ~pr_elem:pr_elem ~pr_space:pr_space).field_list

let pp_init_gen ~pr_elem ~pr_space =
  (pp_elem_sp ~pr_elem:pr_elem ~pr_space:pr_space).init

let pp_init_list_gen ~pr_elem ~pr_space =
  (pp_elem_sp ~pr_elem:pr_elem ~pr_space:pr_space).init_list

let pp_param_gen ~pr_elem ~pr_space =
  (pp_elem_sp ~pr_elem:pr_elem ~pr_space:pr_space).param

let pp_param_list_gen ~pr_elem ~pr_space =
  (pp_elem_sp ~pr_elem:pr_elem ~pr_space:pr_space).paramlist

let pp_template_param_gen ~pr_elem ~pr_space =
  (pp_elem_sp ~pr_elem:pr_elem ~pr_space:pr_space).template_param

let pp_template_param_list_gen ~pr_elem ~pr_space =
  (pp_elem_sp ~pr_elem:pr_elem ~pr_space:pr_space).template_paramlist

let pp_define_param_list_gen ~pr_elem ~pr_space =
  (pp_elem_sp ~pr_elem:pr_elem ~pr_space:pr_space).dparamlist

let pp_type_gen ~pr_elem ~pr_space =
  (pp_elem_sp ~pr_elem:pr_elem ~pr_space:pr_space).ty

let pp_string_fragment_list_gen ~pr_elem ~pr_space =
  (pp_elem_sp ~pr_elem:pr_elem ~pr_space:pr_space).fragment_list

let pp_string_format_gen ~pr_elem ~pr_space =
  (pp_elem_sp ~pr_elem:pr_elem ~pr_space:pr_space).format

let pp_attr_arg_gen ~pr_elem ~pr_space =
  (pp_elem_sp ~pr_elem:pr_elem ~pr_space:pr_space).attr_arg

let pp_program_gen ~pr_elem ~pr_space =
  (pp_elem_sp ~pr_elem:pr_elem ~pr_space:pr_space).toplevel


let string_of_expression e =
  Common.format_to_string_nonl (fun () ->
    pp_expression_simple e
  )

let string_of_ifdef_guard = function
  | Gifdef s  -> "defined(" ^ s ^ ")"
  | Gifndef s -> "!defined(" ^ s ^ ")"
  | Gif_str (_,s) -> s
  | Gif e     -> string_of_expression e
  | Gnone     -> "0"

let string_of_flow n =
  Common.format_to_string (fun () ->
    pp_flow_simple n
  )

let string_of_fullType t =
  Common.format_to_string (fun () ->
    pp_type_simple t
  )

let (debug_info_of_node:
       Control_flow_c.G.key -> Control_flow_c.cflow -> string) =
  fun nodei flow ->
    let node = Control_flow_c.KeyMap.find nodei flow#nodes in
    let s = Common.format_to_string (fun () ->
      pp_flow_simple node
    ) in
    let pos = Lib_parsing_c.min_pinfo_of_node node in
    (spf "%s(n%d)--> %s" (Common.string_of_parse_info_bis pos) nodei s)
