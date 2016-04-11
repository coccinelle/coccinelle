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

type type_with_ident =
    (string * Ast_c.info) option ->
    (Ast_c.storage * Ast_c.il) option ->
    Ast_c.fullType ->
    Ast_c.attribute list -> unit

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
  init_list       : (Ast_c.initialiser wrap2 list) printer;
  param           : Ast_c.parameterType printer;
  paramlist       : (Ast_c.parameterType Ast_c.wrap2 list) printer;
  ty              : Ast_c.fullType printer;
  type_with_ident : type_with_ident;
  toplevel        : Ast_c.toplevel printer;
  fragment        : Ast_c.string_fragment printer;
  fragment_list   : (Ast_c.string_fragment list) printer;
  format          : Ast_c.string_format printer;
  flow            : Control_flow_c.node printer
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
  let start_block () = pr_nl(); pr_indent() in
  let end_block   () = pr_unindent(); pr_nl() in
(*
  let pr_nl_slash _ = (* multiline macro *)
    let slash = (Ast_c.fakeInfo() +> Ast_c.rewrap_str " \\") in
    pr_elem slash; pr_nl() in
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
        pr_elem i2;

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

    | ArrayAccess    (e1, e2),   [i1;i2] ->
        pp_expression e1; pr_elem i1; pp_expression e2; pr_elem i2
    | RecordAccess   (e, name),     [i1] ->
        pp_expression e; pr_elem i1; pp_name name;
    | RecordPtAccess (e, name),     [i1] ->
        pp_expression e; pr_elem i1; pp_name name;

    | SizeOfExpr  (e),     [i] ->
	pr_elem i;
	(match Ast_c.unwrap e with
	  ParenExpr (e), _ -> ()
	| _ -> pr_space());
	pp_expression e
    | SizeOfType  (t),     [i1;i2;i3] ->
        pr_elem i1; pr_elem i2; pp_type t; pr_elem i3
    | Cast    (t, e),      [i1;i2] ->
        pr_elem i1; pp_type t; pr_elem i2; pp_expression e

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

    | New   (None, t),     [i1] -> pr_elem i1; pp_argument t
    | New   (Some ts, t),     [i1; i2; i3] ->
	pr_elem i1; pr_elem i2; pp_arg_list ts; pr_elem i3; pp_argument t
    | Delete(t),     [i1] -> pr_elem i1; pp_expression t

    | Defined name, [i1] ->
        pr_elem i1; (* defined *)
        pp_name name;
    | Defined name, [i1;i2;i3] ->
        pr_elem i1; (* defined *)
        pr_elem i2; (* ( *)
        pp_name name;
        pr_elem i3; (* ) *)

    | (Ident (_) | Constant _ | StringConstant _ | FunCall (_,_)
    | CondExpr (_,_,_) | Sequence (_,_) | Assignment (_,_,_)
    | Postfix (_,_) | Infix (_,_) | Unary (_,_) | Binary (_,_,_)
    | ArrayAccess (_,_) | RecordAccess (_,_) | RecordPtAccess (_,_)
    | SizeOfExpr (_) | SizeOfType (_) | Cast (_,_)
    | StatementExpr (_) | Constructor _
    | ParenExpr (_) | New (_) | Delete (_)
    | Defined (_)),_ -> raise (Impossible 95)
    );

    if !Flag_parsing_c.pretty_print_type_info
    then begin
      pr_elem (Ast_c.fakeInfo() +> Ast_c.rewrap_str "/*");
      !typ +>
      (fun (ty,_test) -> ty +>
	Common.do_option
	  (fun (x,l) -> pp_type x;
	    let s = match l with
	      Ast_c.LocalVar _ -> ", local"
	    | _ -> "" in
	    pr_elem (Ast_c.fakeInfo() +> Ast_c.rewrap_str s)));
      pr_elem (Ast_c.fakeInfo() +> Ast_c.rewrap_str "*/");
    end

  and pr_assignOp (_,ii) =
    let i = Common.tuple_of_list1 ii in
    pr_elem i

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
        pr_elem i1; pp_expression e; pr_elem i2; pr_nl(); pr_indent();
	pp_statement st
    | Labeled (CaseRange  (e, e2, st)), [i1;i2;i3] ->
	pr_unindent();
        pr_elem i1; pp_expression e; pr_elem i2; pp_expression e2; pr_elem i3;
	pr_nl(); pr_indent();
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
    | Selection  (Switch (e, st)), [i1;i2;i3;iifakend] ->
        pr_elem i1; pr_space(); pr_elem i2; pp_expression e; pr_elem i3;
	indent_if_needed st (function _-> pp_statement st); pr_elem iifakend
    | Iteration  (While (e, st)), [i1;i2;i3;iifakend] ->
        pr_elem i1; pr_space(); pr_elem i2; pp_expression e; pr_elem i3;
	indent_if_needed st (function _-> pp_statement st); pr_elem iifakend
    | Iteration  (DoWhile (st, e)), [i1;i2;i3;i4;i5;iifakend] ->
        pr_elem i1;
	indent_if_needed st (function _ -> pp_statement st);
	pr_elem i2; pr_elem i3; pp_expression e;
        pr_elem i4; pr_elem i5;
        pr_elem iifakend


    | Iteration  (For (first,(e2opt,il2),(e3opt, il3),st)),
        [i1;i2;i3;iifakend] ->

          pr_elem i1; pr_space();
          pr_elem i2;
	  (match first with
	    ForExp (e1opt,il1) ->
              pp_statement (Ast_c.mk_st (ExprStatement e1opt) il1)
	  | ForDecl decl -> pp_decl decl);
          pp_statement (Ast_c.mk_st (ExprStatement e2opt) il2);
          assert (il3 = []);
          pp_statement (Ast_c.mk_st (ExprStatement e3opt) il3);
          pr_elem i3;
          indent_if_needed st (function _ -> pp_statement st);
          pr_elem iifakend

    | Iteration  (MacroIteration (s,es,st)), [i1;i2;i3;iifakend] ->
        pr_elem i1; pr_space();
        pr_elem i2;

        es +> List.iter (fun (e, opt) ->
          assert (List.length opt <= 1);
          opt +> List.iter pr_elem;
          pp_argument e;
        );

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

    | (Labeled (Case  (_,_))
    | Labeled (CaseRange  (_,_,_)) | Labeled (Default _)
    | Compound _ | ExprStatement _
    | Selection  (If (_, _, _)) | Selection  (Switch (_, _))
    | Selection (Ifdef_Ite _) | Selection (Ifdef_Ite2 _)
    | Iteration  (While (_, _)) | Iteration  (DoWhile (_, _))
    | Iteration  (For (_, (_,_), (_, _), _))
    | Iteration  (MacroIteration (_,_,_))
    | Jump ((Continue|Break|Return)) | Jump (ReturnExpr _)
    | Jump (GotoComputed _)
    | Decl _ | Exec _
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
	 (string * info) option -> (storage * il) option ->
	   fullType -> attribute list ->
	     unit) =
    fun ident sto ft attrs ->
      pp_base_type ft  sto;
      (match (ident, Ast_c.unwrap_typeC ft) with
	(Some _,_) | (_,Pointer _) -> pr_space()
      |	_ -> ());
      pp_type_with_ident_rest ident ft attrs


  and (pp_base_type: fullType -> (storage * il) option -> unit) =
    fun (qu, (ty, iity)) sto ->
      let get_sto sto =
        match sto with
        | None -> [] | Some (s, iis) -> (*assert (List.length iis = 1);*) iis
      in
      let print_sto_qu (sto, (qu, iiqu)) =
        let all_ii = get_sto sto @ iiqu in
        all_ii
          +> List.sort Ast_c.compare_pos
          +> Common.print_between pr_space pr_elem

      in
      let print_sto_qu_ty (sto, (qu, iiqu), iity) =
        let all_ii = get_sto sto @ iiqu @ iity in
        let all_ii2 = all_ii +> List.sort Ast_c.compare_pos in

        if all_ii <> all_ii2
        then begin
            (* TODO in fact for pointer, the qualifier is after the type
             * cf -test strangeorder
             *)
          pr2 "STRANGEORDER";
          all_ii2 +> Common.print_between pr_space pr_elem
        end
        else all_ii2 +> Common.print_between pr_space pr_elem
      in

      match ty, iity with
      |	(NoType,_) -> ()
      | (Pointer t, [i])                           -> pp_base_type t sto
      | (ParenType t, _)                           -> pp_base_type t sto
      | (Array (eopt, t), [i1;i2])                 -> pp_base_type t sto
      | (FunctionType (returnt, paramst), [i1;i2]) ->
          pp_base_type returnt sto;


      | (StructUnion (su, sopt, fields),iis) ->
          print_sto_qu (sto, qu);

          (match sopt,iis with
          | Some s , [i1;i2;i3;i4] ->
              pr_elem i1; pr_elem i2; pr_elem i3;
          | None, [i1;i2;i3] ->
              pr_elem i1; pr_elem i2;
          | x -> raise (Impossible 101)
	  );

          fields +> List.iter pp_field;

          (match sopt,iis with
          | Some s , [i1;i2;i3;i4] -> pr_elem i4
          | None, [i1;i2;i3] ->       pr_elem i3;
          | x -> raise (Impossible 102)
	  );



      | (Enum  (sopt, enumt), iis) ->
          print_sto_qu (sto, qu);

          (match sopt, iis with
          | (Some s, ([i1;i2;i3;i4]|[i1;i2;i3;i4;_])) ->
              pr_elem i1; pr_elem i2; pr_elem i3;
          | (None, ([i1;i2;i3]|[i1;i2;i3;_])) ->
              pr_elem i1; pr_elem i2
          | x -> raise (Impossible 103)
	  );

          enumt +> List.iter (fun ((name, eopt), iicomma) ->
            assert (List.length iicomma <= 1);
            iicomma +> List.iter (function x -> pr_elem x; pr_space());
            pp_name name;
            eopt +> Common.do_option (fun (ieq, e) ->
              pr_elem ieq;
              pp_expression e;
	  ));

          (match sopt, iis with
          | (Some s, [i1;i2;i3;i4]) ->    pr_elem i4
          | (Some s, [i1;i2;i3;i4;i5]) ->
              pr_elem i5; pr_elem i4 (* trailing comma *)
          | (None, [i1;i2;i3]) ->         pr_elem i3
          | (None, [i1;i2;i3;i4]) ->
              pr_elem i4; pr_elem i3 (* trailing comma *)


          | x -> raise (Impossible 104)
	  );


      | (BaseType _, iis) ->
          print_sto_qu_ty (sto, qu, iis);

      | (StructUnionName (s, structunion), iis) ->
          assert (List.length iis = 2);
          print_sto_qu_ty (sto, qu, iis);

      | (EnumName  s, iis) ->
          assert (List.length iis = 2);
          print_sto_qu_ty (sto, qu, iis);

      | (Decimal(l,p), [dec;lp;cm;rp]) ->
	  (* hope that sto before qu is right... cf print_sto_qu_ty *)
	  let stoqulp = get_sto sto @ (snd qu) @ [dec] in
	  Common.print_between pr_space pr_elem stoqulp;
	  pr_elem lp; pp_expression l; pr_elem cm;
	  do_option pp_expression p; pr_elem rp

      | (TypeName (name,typ), noii) ->
          assert (noii = []);
          let (_s, iis) = get_s_and_info_of_name name in
          print_sto_qu_ty (sto, qu, [iis]);

          if !Flag_parsing_c.pretty_print_typedef_value
          then begin
            pr_elem (Ast_c.fakeInfo() +> Ast_c.rewrap_str "{*");
            typ +> Common.do_option (fun typ ->
                pp_type typ;
            );
            pr_elem (Ast_c.fakeInfo() +> Ast_c.rewrap_str "*}");
          end;

      | (TypeOfExpr (e), iis) ->
          print_sto_qu (sto, qu);
          (match iis with
          | [itypeof;iopar;icpar] ->
              pr_elem itypeof; pr_elem iopar;
              pp_expression e;
              pr_elem icpar;
          | _ -> raise (Impossible 105)
          )

      | (TypeOfType (t), iis) ->
          print_sto_qu (sto, qu);
          (match iis with
          | [itypeof;iopar;icpar] ->
              pr_elem itypeof; pr_elem iopar;
              pp_type t;
              pr_elem icpar;
          | _ -> raise (Impossible 106)
	  )

      | (Pointer _ | (*ParenType _ |*) Array _ | FunctionType _ | Decimal _
            (* | StructUnion _ | Enum _ | BaseType _ *)
            (* | StructUnionName _ | EnumName _ | TypeName _  *)
            (* | TypeOfExpr _ | TypeOfType _ *)
         ), _ -> raise (Impossible 107)

  and pp_field_list fields = fields +>  Common.print_between pr_nl pp_field
  and pp_field = function
      DeclarationField
	(FieldDeclList(onefield_multivars,[iiptvirg;ifakestart])) ->
	pr_elem ifakestart;
        (match onefield_multivars with
          x::xs ->
	    (* handling the first var. Special case, with the
               first var, we print the whole type *)

	    (match x with
	      (Simple (nameopt, typ)), iivirg ->
              (* first var cannot have a preceding ',' *)
		assert (List.length iivirg = 0);
		let identinfo =
                  match nameopt with
		  | None -> None
                  | Some name -> Some (get_s_and_info_of_name name)
                in
		pp_type_with_ident identinfo None typ Ast_c.noattr;

	    | (BitField (nameopt, typ, iidot, expr)), iivirg ->
                      (* first var cannot have a preceding ',' *)
		assert (List.length iivirg = 0);
		(match nameopt with
		| None ->
		    pp_type typ;
		| Some name ->
                    let (s, is) = get_s_and_info_of_name name in
		    pp_type_with_ident
		      (Some (s, is)) None typ Ast_c.noattr;
		    );
                pr_elem iidot;
		pp_expression expr

                  ); (* match x, first onefield_multivars *)

                      (* for other vars *)
	    xs +> List.iter (function
	      | (Simple (nameopt, typ)), iivirg ->
		  iivirg +> List.iter pr_elem;
		  let identinfo =
		    match nameopt with
		    | None -> None
		    | Some name -> Some (get_s_and_info_of_name name)
		  in
		  pp_type_with_ident_rest identinfo typ Ast_c.noattr

	      | (BitField (nameopt, typ, iidot, expr)), iivirg ->
		  iivirg +> List.iter pr_elem;
		  (match nameopt with
		  | Some name ->
                      let (s,is) = get_s_and_info_of_name name in
		      pp_type_with_ident_rest
			(Some (s, is)) typ Ast_c.noattr;
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

    | MacroDeclField ((s, es), ii)  ->
        let (iis, lp, rp, iiend, ifakestart) =
          Common.tuple_of_list5 ii in
                 (* iis::lp::rp::iiend::ifakestart::iisto
	            iisto +> List.iter pr_elem; (* static and const *)
                 *)
	pr_elem ifakestart;
	pr_elem iis;
	pr_elem lp;
	es +> List.iter (fun (e, opt) ->
          assert (List.length opt <= 1);
          opt +> List.iter pr_elem;
          pp_argument e;
	  );

	pr_elem rp;
	pr_elem iiend;



    | EmptyField iipttvirg_when_emptyfield ->
        pr_elem iipttvirg_when_emptyfield

    | CppDirectiveStruct cpp -> pp_directive cpp
    | IfdefStruct ifdef -> pp_ifdef ifdef

(* used because of DeclList, in    int i,*j[23];  we don't print anymore the
   int before *j *)
  and (pp_type_with_ident_rest: (string * info) option ->
    fullType -> attribute list -> unit) =

    fun ident (((qu, iiqu), (ty, iity)) as fullt) attrs ->

      let print_ident ident = Common.do_option (fun (s, iis) ->
        (* XXX attrs +> pp_attributes pr_elem pr_space; *)
        pr_elem iis
	) ident
      in

      match ty, iity with
      (* the work is to do in base_type !! *)
      | (NoType, iis)                           -> ()
      | (BaseType _, iis)                       -> print_ident ident
      | (Enum  (sopt, enumt), iis)              -> print_ident ident
      | (StructUnion (_, sopt, fields),iis)     -> print_ident ident
      | (StructUnionName (s, structunion), iis) -> print_ident ident
      | (EnumName  s, iis)                      -> print_ident ident
      | (Decimal _, iis)                        -> print_ident ident
      | (TypeName (_name,_typ), iis)            -> print_ident ident
      | (TypeOfExpr (e), iis)                   -> print_ident ident
      | (TypeOfType (e), iis)                   -> print_ident ident



      | (Pointer t, [i]) ->
          (* subtil:  void ( *done)(int i)   is a Pointer
             (FunctionType (return=void, params=int i) *)
          (*WRONG I THINK, use left & right function *)
          (* bug: pp_type_with_ident_rest None t;      print_ident ident *)
          pr_elem i;
          iiqu +> List.iter pr_elem; (* le const est forcement apres le '*' *)
          pp_type_with_ident_rest ident t attrs;

      (* ugly special case ... todo? maybe sufficient in practice *)
      | (ParenType ttop, [i1;i2]) ->
          (match Ast_c.get_ty_and_ii ttop with
          | (_q1, (Pointer t2, [ipointer])) ->
              (match Ast_c.get_ty_and_ii t2 with
              | (q2, (FunctionType t, ii3)) ->

		  pp_type_left (q2, mk_tybis (FunctionType t) ii3);
		  pr_elem i1;
		  pr_elem ipointer;
		  print_ident ident;
		  pr_elem i2;
		  pp_type_right (q2, mk_tybis (FunctionType t) ii3);
              | _ ->
                  pr2 "PB PARENTYPE ZARB, I forget about the ()";
                  pp_type_with_ident_rest ident ttop attrs;
              )
          (* another ugly special case *)
          | _q1, (Array (eopt,t2 ), [iarray1;iarray2]) ->
              (match Ast_c.get_ty_and_ii t2 with
              | (_q2, (Pointer t3, [ipointer])) ->
                  (match Ast_c.get_ty_and_ii t3 with
                  | (q3, (FunctionType t, iifunc)) ->

		      pp_type_left (q3, mk_tybis (FunctionType t) iifunc);
		      pr_elem i1;
		      pr_elem ipointer;
		      print_ident ident;
		      pr_elem iarray1;
		      do_option pp_expression eopt;
		      pr_elem iarray2;
		      pr_elem i2;
		      pp_type_right (q3, mk_tybis (FunctionType t) iifunc)
                  | _ ->
                      pr2 "PB PARENTYPE ZARB, I forget about the ()";
                      pp_type_with_ident_rest ident ttop attrs;
                  )
              | _ ->
                  pr2 "PB PARENTYPE ZARB, I forget about the ()";
                  pp_type_with_ident_rest ident ttop attrs;
              )
          | _t ->

              pr2 "PB PARENTYPE ZARB, I forget about the ()";
              pp_type_with_ident_rest ident ttop attrs;
          )


      | (Array (eopt, t), [i1;i2]) ->
          pp_type_left fullt;

          iiqu +> List.iter pr_elem;
          print_ident ident;

          pp_type_right fullt;


      | (FunctionType (returnt, paramst), [i1;i2]) ->
          pp_type_left fullt;

          iiqu +> List.iter pr_elem;
          print_ident ident;

          pp_type_right fullt;


      | (FunctionType _ | Array _ | ParenType _ | Pointer _), _ ->
	  raise (Impossible 109)


  and (pp_type_left: fullType -> unit) =
    fun ((qu, iiqu), (ty, iity)) ->
      match ty, iity with
	(NoType,_) -> failwith "pp_type_left: unexpected NoType"
      | (Pointer t, [i]) ->
          pr_elem i;
          iiqu +> List.iter pr_elem; (* le const est forcement apres le '*' *)
          pp_type_left t

      | (Array (eopt, t), [i1;i2]) -> pp_type_left t
      | (FunctionType (returnt, paramst), [i1;i2]) -> pp_type_left returnt

      | (ParenType t, _) ->  failwith "parenType"


      | (BaseType _, iis)    -> ()
      | (Enum  (sopt, enumt), iis) -> ()
      | (StructUnion (_, sopt, fields),iis)  -> ()
      | (StructUnionName (s, structunion), iis) -> ()
      | (EnumName  s, iis) -> ()
      | (Decimal(l,p), iis) -> ()
      | (TypeName (_name,_typ), iis) -> ()

      | TypeOfType _, _ -> ()
      | TypeOfExpr _, _ -> ()

      | (FunctionType _ | Array _ | Pointer _), _ -> raise (Impossible 110)


  and pp_param param =
    let {p_namei = nameopt;
         p_register = (b,iib);
         p_type=t;} = param in

    iib +> List.iter pr_elem;

    match nameopt with
    | None ->
        pp_type t
    | Some name ->
        let (s,i1) = get_s_and_info_of_name name in
	pp_type_with_ident
          (Some (s, i1)) None t Ast_c.noattr




  and pp_type_right (((qu, iiqu), (ty, iity)) : fullType) =
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
        pr_elem i1;
        (match paramst with
        | (ts, (b, iib)) ->
            ts +> List.iter (fun (param,iicomma) ->
              assert ((List.length iicomma) <= 1);
              iicomma +> List.iter (function x -> pr_elem x; pr_space());

              pp_param param;
	    );
            iib +> List.iter pr_elem;
        );
        pr_elem i2

    | (BaseType _, iis)        -> ()
    | (Enum  (sopt, enumt), iis) -> ()
    | (StructUnion (_, sopt, fields),iis)-> ()
    | (StructUnionName (s, structunion), iis) -> ()
    | (EnumName  s, iis) -> ()
    | (Decimal(l,p), iis) -> ()
    | (TypeName (name,_typ), iis) -> ()

    | TypeOfType _, _ -> ()
    | TypeOfExpr _, _ -> ()

    | (FunctionType _ | Array _ | Pointer _), _ -> raise (Impossible 111)

  and pp_type t =
    pp_type_with_ident None None t Ast_c.noattr

(* ---------------------- *)
  and pp_decl = function
    | DeclList ((({v_namei = var;
                   v_type = returnType;
                   v_storage = storage;
                   v_attr = attrs;
                  },[])::xs),
	       iivirg::ifakestart::iisto) ->

	pr_elem ifakestart;

        (* old: iisto +> List.iter pr_elem; *)


        (* handling the first var. Special case, we print the whole type *)
	(match var with
	| Some (name, iniopt) ->
            let (s,iis) = get_s_and_info_of_name name in
	    pp_type_with_ident
	      (Some (s, iis)) (Some (storage, iisto))
	      returnType attrs;
	    (match iniopt with
	      Ast_c.NoInit -> ()
	    | Ast_c.ValInit(iini,init) -> pr_elem iini; pp_init init
	    | Ast_c.ConstrInit((init,[lp;rp])) ->
		pr_elem lp; pp_arg_list init; pr_elem rp
	    | Ast_c.ConstrInit _ -> raise (Impossible 112))
	| None -> pp_type returnType
	);

      (* for other vars, we just call pp_type_with_ident_rest. *)
	xs +> List.iter (function
	| ({v_namei = Some (name, iniopt);
	    v_type = returnType;
	    v_storage = storage2;
	    v_attr = attrs;
	  }, iivirg) ->

            let (s,iis) = get_s_and_info_of_name name in
	    assert (storage2 = storage);
	    iivirg +> List.iter pr_elem;
	    pp_type_with_ident_rest
	      (Some (s, iis)) returnType attrs;
	    (match iniopt with
	      Ast_c.NoInit -> ()
	    | Ast_c.ValInit(iini,init) -> pr_elem iini; pp_init init
	    | Ast_c.ConstrInit((init,[lp;rp])) ->
		pr_elem lp; pp_arg_list init; pr_elem rp
	    | Ast_c.ConstrInit _ -> raise (Impossible 113));


	| x -> raise (Impossible 114)
	);

	pr_elem iivirg;

    | MacroDecl ((sto, s, es, true), iis::lp::rp::iiend::ifakestart::iisto) ->
	pr_elem ifakestart;
	iisto +> List.iter pr_elem; (* static and const *)
	pr_elem iis;
	pr_elem lp;
	es +> List.iter (fun (e, opt) ->
          assert (List.length opt <= 1);
          opt +> List.iter pr_elem;
          pp_argument e;
	);

	pr_elem rp;
	pr_elem iiend;

    | MacroDecl ((sto, s, es, false), iis::lp::rp::ifakestart::iisto) ->
	pr_elem ifakestart;
	iisto +> List.iter pr_elem; (* static and const *)
	pr_elem iis;
	pr_elem lp;
	es +> List.iter (fun (e, opt) ->
          assert (List.length opt <= 1);
          opt +> List.iter pr_elem;
          pp_argument e;
	);

	pr_elem rp;

    | MacroDeclInit
	((sto, s, es, ini), iis::lp::rp::eq::iiend::ifakestart::iisto) ->
	pr_elem ifakestart;
	iisto +> List.iter pr_elem; (* static and const *)
	pr_elem iis;
	pr_elem lp;
	es +> List.iter (fun (e, opt) ->
          assert (List.length opt <= 1);
          opt +> List.iter pr_elem;
          pp_argument e;
	);

	pr_elem rp;
	pr_elem eq;
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

      | InitDesignators (xs, initialiser), [i1] -> (* : *)
          xs +> List.iter pp_designator;
          pr_elem i1;
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

  and pp_init_list ini = pp_list pp_init ini

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
  and pp_attributes pr_elem pr_space attrs =
    attrs +> List.iter (fun (attr, ii) ->
      ii +> List.iter pr_elem;
    );

(* ---------------------- *)
  and pp_def def =
    let defbis, ii = def in
    match ii with
    | iifunc1::iifunc2::i1::i2::ifakestart::isto ->
	let {f_name = name;
             f_type = (returnt, (paramst, (b, iib)));
             f_storage = sto;
             f_body = statxs;
             f_attr = attrs;
	} = defbis
	in
        pr_elem ifakestart;

        pp_type_with_ident None (Some (sto, isto))
          returnt Ast_c.noattr;

        pp_attributes pr_elem pr_space attrs;
	pr_space();
        pp_name name;

        pr_elem iifunc1;

        (* not anymore, cf tests/optional_name_parameter and
           macro_parameter_shortcut.c
           (match paramst with
           | [(((bool, None, t), ii_b_s), iicomma)] ->
               assert
		 (match t with
		 | qu, (BaseType Void, ii) -> true
		 | _ -> true
	       );
               assert (iicomma = []);
               assert (ii_b_s = []);
               pp_type_with_ident None None t

           | paramst ->
               paramst +> List.iter (fun (((bool, s, t), ii_b_s), iicomma) ->
	       iicomma +> List.iter pr_elem;

	       (match b, s, ii_b_s with
               | false, Some s, [i1] ->
		   pp_type_with_ident (Some (s, i1)) None t;
               | true, Some s, [i1;i2] ->
		   pr_elem i1;
		   pp_type_with_ident (Some (s, i2)) None t;

            (* in definition we have name for params, except when f(void) *)
               | _, None, _ -> raise Impossible
               | false, None, [] ->

               | _ -> raise Impossible
           )));

         (* normally ii represent the ",..." but it is also abused
            with the f(void) case *)
         (* assert (List.length iib <= 2);*)
           iib +> List.iter pr_elem;

        *)
	pp_param_list paramst;
        iib +> List.iter pr_elem;


        pr_elem iifunc2; pr_space();
        pr_elem i1;
	pp_statement_seq_list statxs;
        pr_elem i2;
    | _ -> raise (Impossible 118)

  and pp_param_list paramst = pp_list pp_param paramst

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
	pr_elem idefine;
	pr_elem iident;

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
            params +> List.iter (fun ((s,iis), iicomma) ->
              assert (List.length iicomma <= 1);
              iicomma +> List.iter pr_elem;
              iis +> List.iter pr_elem;
            );
            pr_elem i2;
	);
	define_val defval;
	pr_elem ieol

    | Pragma ((s,ii), pragmainfo) ->
	let (ipragma,iident,ieol) = Common.tuple_of_list3 ii in
	pr_elem ipragma;
	pr_elem iident;
	pp_pragmainfo pragmainfo;
	pr_elem ieol

    | OtherDirective (ii) ->
	List.iter pr_elem ii

  and pp_pragmainfo = function
      PragmaTuple(args,ii) ->
	let (ilp,irp) = Common.tuple_of_list2 ii in
	pr_elem ilp;
	pp_arg_list args;
        pr_elem irp
    | PragmaIdList(ids) ->
	let loop = function
	    [] -> ()
	  | [id,_] -> pp_name id
	  | (id,_)::rest -> pp_name id; pr_space() in
	loop ids in

  let rec pp_toplevel = function
    | Declaration decl -> pp_decl decl
    | Definition def -> pp_def def

    | CppTop directive -> pp_directive directive


    | MacroTop (s, es,   [i1;i2;i3;i4]) ->
	pr_elem i1;
	pr_elem i2;
	es +> List.iter (fun (e, opt) ->
          assert (List.length opt <= 1);
          opt +> List.iter pr_elem;
          pp_argument e;
	);
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
	pr_elem i4;
    | (MacroTop _) | (Namespace _) -> raise (Impossible 120) in




  let pp_flow n =
    match F.unwrap n  with
    | F.FunHeader ({f_name =idb;
                     f_type = (rett, (paramst,(isvaargs,iidotsb)));
                     f_storage = stob;
                     f_body = body;
                     f_attr = attrs},ii) ->

		       assert (body = []);
      (*
	 iif ii;
	 iif iidotsb;
	 attrs +> List.iter (vk_attribute bigf);
	 vk_type bigf rett;
	 paramst +> List.iter (fun (param, iicomma) ->
         vk_param bigf param;
         iif iicomma;
	 );
      *)
		       pr2 "Def";


    | F.Decl decl ->
        (* vk_decl bigf decl *)
	pr2 "Decl"

    | F.ExprStatement (st, (eopt, ii)) ->
	pp_statement (Ast_c.mk_st (ExprStatement eopt) ii)

    | F.IfHeader (_, (e,ii))
    | F.SwitchHeader (_, (e,ii))
    | F.WhileHeader (_, (e,ii))
    | F.DoWhileTail (e,ii) ->
        (*
           iif ii;
           vk_expr bigf e
        *)
	pr2 "XXX";


    | F.ForHeader (_st, ((first, (e2opt,i2), (e3opt,i3)), ii)) ->
        (*
           iif i1; iif i2; iif i3;
           iif ii;
           e1opt +> do_option (vk_expr bigf);
           e2opt +> do_option (vk_expr bigf);
           e3opt +> do_option (vk_expr bigf);
        *)
	pr2 "XXX"

    | F.MacroIterHeader (_s, ((s,es), ii)) ->
        (*
           iif ii;
           vk_argument_list bigf es;
        *)
	pr2 "XXX"


    | F.ReturnExpr (_st, (e,ii)) ->
        (* iif ii; vk_expr bigf e*)
	pr2 "XXX"


    | F.Case  (_st, (e,ii)) ->
      (* iif ii; vk_expr bigf e *)
	pr2 "XXX"

    | F.CaseRange (_st, ((e1, e2),ii)) ->
        (* iif ii; vk_expr bigf e1; vk_expr bigf e2 *)
	pr2 "XXX"



    | F.CaseNode i -> ()

    | F.DefineExpr e  ->
        (* vk_expr bigf e *)
	pr2 "XXX"

    | F.DefineType ft  ->
        (* vk_type bigf ft *)
	pr2 "XXX"

    | F.DefineHeader ((s,ii), (defkind))  ->
        (*
           iif ii;
           vk_define_kind bigf defkind;
        *)
	pr2 "XXX"


    | F.DefineDoWhileZeroHeader (((),ii)) ->
        (* iif ii *)
	pr2 "XXX"

    | F.PragmaHeader((s,ii), pragmainfo) ->
	let (ipragma,iident,ieol) = Common.tuple_of_list3 ii in
	pr_elem ipragma;
	pr_elem iident;
	pp_pragmainfo pragmainfo

    | F.Include {i_include = (s, ii);} ->
        (* iif ii; *)
	pr2 "XXX"


    | F.MacroTop (s, args, ii) ->
        (* iif ii;
           vk_argument_list bigf args *)
	pr2 "XXX"


    | F.Break    (st,((),ii),fromswitch) ->
        (* iif ii *)
	pr2 "XXX"
    | F.Continue (st,((),ii)) ->
        (* iif ii *)
	pr2 "XXX"
    | F.Default  (st,((),ii)) ->
        (* iif ii *)
	pr2 "XXX"
    | F.Return   (st,((),ii)) ->
        (* iif ii *)
	pr2 "XXX"
    | F.Goto  (st, name, ((),ii)) ->
        (* iif ii *)
	pr2 "XXX"
    | F.Label (st, name, ((),ii)) ->
        (* iif ii *)
	pr2 "XXX"
    | F.EndStatement iopt ->
        (* do_option infof iopt *)
	pr2 "XXX"
    | F.DoHeader (st, info) ->
        (* infof info *)
	pr2 "XXX"
    | F.Else info ->
        (* infof info *)
	pr2 "XXX"
    | F.SeqEnd (i, info) ->
        (* infof info *)
	pr2 "XXX"
    | F.SeqStart (st, i, info) ->
        (* infof info *)
	pr2 "XXX"

    | F.MacroStmt (st, ((),ii)) ->
        (* iif ii *)
	pr2 "XXX"
    | F.Asm (st, (asmbody,ii)) ->
        (*
           iif ii;
           vk_asmbody bigf asmbody
        *)
	pr2 "XXX"

    | F.Exec(st,(code,ii)) -> pr2 "XXX"

    | F.IfdefHeader (info) ->
	pp_ifdef info
    | F.IfdefElse (info) ->
	pp_ifdef info
    | F.IfdefEndif (info) ->
	pp_ifdef info

    | F.IfdefIteHeader _ii ->
        pr2 "XXX"

    | F.DefineTodo ->
	pr2 "XXX"


    | (F.TopNode|F.EndNode|
      F.ErrorExit|F.Exit|F.Enter|F.LoopFallThroughNode|F.FallThroughNode|
      F.AfterNode _|F.FalseNode|F.TrueNode _|F.InLoopNode|
      F.Fake) ->
        pr2 "YYY" in


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
    ty         = pp_type;
    type_with_ident = pp_type_with_ident;
    toplevel   = pp_toplevel;
    fragment   = pp_string_fragment;
    fragment_list = pp_string_fragment_list;
    format     = pp_string_format;
    flow       = pp_flow;
  }

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
let pp_flow_simple       = ppc.flow


let pp_elem_sp ~pr_elem ~pr_space =
  mk_pretty_printers
    ~pr_elem ~pr_space
    ~pr_nl ~pr_outdent ~pr_indent ~pr_unindent

let pp_expression_gen ~pr_elem ~pr_space =
  (pp_elem_sp pr_elem pr_space).expression

let pp_assignOp_gen ~pr_elem ~pr_space =
  (pp_elem_sp pr_elem pr_space).assignOp

let pp_binaryOp_gen ~pr_elem ~pr_space =
  (pp_elem_sp pr_elem pr_space).binaryOp

let pp_arg_list_gen ~pr_elem ~pr_space =
  (pp_elem_sp pr_elem pr_space).arg_list

let pp_arg_gen ~pr_elem ~pr_space =
  (pp_elem_sp pr_elem pr_space).arg

let pp_statement_gen ~pr_elem ~pr_space =
  (pp_elem_sp pr_elem pr_space).statement

let pp_statement_seq_list_gen ~pr_elem ~pr_space =
  (pp_elem_sp pr_elem pr_space).statement_seq_list

let pp_decl_gen ~pr_elem ~pr_space =
  (pp_elem_sp pr_elem pr_space).decl

let pp_field_gen ~pr_elem ~pr_space =
  (pp_elem_sp pr_elem pr_space).field

let pp_field_list_gen ~pr_elem ~pr_space =
  (pp_elem_sp pr_elem pr_space).field_list

let pp_init_gen ~pr_elem ~pr_space =
  (pp_elem_sp pr_elem pr_space).init

let pp_init_list_gen ~pr_elem ~pr_space =
  (pp_elem_sp pr_elem pr_space).init_list

let pp_param_gen ~pr_elem ~pr_space =
  (pp_elem_sp pr_elem pr_space).param

let pp_param_list_gen ~pr_elem ~pr_space =
  (pp_elem_sp pr_elem pr_space).paramlist

let pp_type_gen ~pr_elem ~pr_space =
  (pp_elem_sp pr_elem pr_space).ty

let pp_type_with_ident_gen pr_elem pr_space =
  (pp_elem_sp pr_elem pr_space).type_with_ident

let pp_string_fragment_list_gen ~pr_elem ~pr_space =
  (pp_elem_sp pr_elem pr_space).fragment_list

let pp_string_format_gen ~pr_elem ~pr_space =
  (pp_elem_sp pr_elem pr_space).format

let pp_program_gen ~pr_elem ~pr_space =
  (pp_elem_sp pr_elem pr_space).toplevel


let string_of_expression e =
  Common.format_to_string (fun () ->
    pp_expression_simple e
  )

let string_of_ifdef_guard = function
  | Gifdef s  -> "defined(" ^ s ^ ")"
  | Gifndef s -> "!defined(" ^ s ^ ")"
  | Gif_str s -> s
  | Gif e     -> string_of_expression e
  | Gnone     -> "0"

let string_of_toplevel top =
  Common.format_to_string (fun () ->
    pp_toplevel_simple top
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
