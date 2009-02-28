(* Yoann Padioleau, Julia Lawall
 * 
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


type pr_elem_func = Ast_c.info -> unit
type pr_space_func = unit -> unit
type pr_nl_func = unit -> unit
type pr_indent_func = unit -> unit
type pr_outdent_func = unit -> unit
type pr_unindent_func = unit -> unit

type expression_printer = Ast_c.expression -> unit
type arg_list_printer = Ast_c.argument Ast_c.wrap2 list -> unit
type statement_printer = Ast_c.statement -> unit
type declaration_printer = Ast_c.declaration -> unit
type initialiser_printer = Ast_c.initialiser -> unit
type param_printer = Ast_c.parameterType -> unit
type type_printer = Ast_c.fullType -> unit
type type_with_ident_printer =
    (string * Ast_c.info) option ->
      (Ast_c.storage * Ast_c.il) option -> Ast_c.fullType ->
	Ast_c.attribute list -> unit
type toplevel_printer = Ast_c.toplevel -> unit
type flow_printer = Control_flow_c.node -> unit

(* result type *)
type pretty_printers =
    {expression : expression_printer;
      arg_list : arg_list_printer;
      statement : statement_printer;
      decl : declaration_printer;
      init : initialiser_printer;
      param : param_printer;
      ty : type_printer;
      type_with_ident : type_with_ident_printer;
      toplevel : toplevel_printer;
      flow : flow_printer}

module F = Control_flow_c

(*****************************************************************************)

(* This module is used by unparse_c, but because unparse_c have also
 * the list of tokens, pretty_print_c could be useless in the future
 * (except that the ast_c have some fake tokens not present in the list
 * of tokens so it's still useful). But this module is also useful to
 * unparse C when you don't have the ordered list of tokens separately,
 * or tokens without position information, for instance when you want
 * to pretty print some piece of C that was generated, or some
 * abstract-lined piece of code, etc. *)

let pretty_print_c pr_elem pr_space pr_nl pr_indent pr_outdent pr_unindent =
  let start_block () = pr_nl(); pr_indent() in
  let end_block   () = pr_unindent(); pr_nl() in
  
  let indent_if_needed (s,_) f =
    match s with
      Compound _ -> pr_space(); f()
    | _ ->
        (*no newline at the end - someone else will do that*)
	start_block(); f(); pr_unindent() in
  
  let rec pp_expression = fun ((exp, typ), ii) -> 
    (match exp, ii with
    | Ident (c),         [i]     -> pr_elem i
    (* only a MultiString can have multiple ii *)
    | Constant (MultiString _), is     -> is +> List.iter pr_elem
    | Constant (c),         [i]     -> pr_elem i 
    | FunCall  (e, es),     [i1;i2] -> 
        pp_expression e; pr_elem i1; 
	pp_arg_list es;
        pr_elem i2;
        
    | CondExpr (e1, e2, e3),    [i1;i2]    -> 
        pp_expression e1; pr_space(); pr_elem i1; pr_space();
	do_option (function x -> pp_expression x; pr_space()) e2; pr_elem i2; 
        pp_expression e3
    | Sequence (e1, e2),          [i]  -> 
        pp_expression e1; pr_elem i; pr_space(); pp_expression e2
    | Assignment (e1, op, e2),    [i]  -> 
        pp_expression e1; pr_space(); pr_elem i; pr_space(); pp_expression e2
          
    | Postfix  (e, op),    [i] -> pp_expression e; pr_elem i;
    | Infix    (e, op),    [i] -> pr_elem i; pp_expression e;
    | Unary    (e, op),    [i] -> pr_elem i; pp_expression e
    | Binary   (e1, op, e2),    [i] -> 
        pp_expression e1; pr_space(); pr_elem i; pr_space(); pp_expression e2
          
    | ArrayAccess    (e1, e2),   [i1;i2] -> 
        pp_expression e1; pr_elem i1; pp_expression e2; pr_elem i2
    | RecordAccess   (e, s),     [i1;i2] -> 
        pp_expression e; pr_elem i1; pr_elem i2
    | RecordPtAccess (e, s),     [i1;i2] -> 
        pp_expression e; pr_elem i1; pr_elem i2
	  
    | SizeOfExpr  (e),     [i] -> pr_elem i; pp_expression e
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
    | Constructor (t, xs), lp::rp::i1::i2::iicommaopt -> 
        pr_elem lp;
        pp_type t;
        pr_elem rp;
        pr_elem i1;
        xs +> List.iter (fun (x, ii) -> 
          assert (List.length ii <= 1);
          ii +> List.iter (function x -> pr_elem x; pr_space());
          pp_init x
        );
        iicommaopt +> List.iter pr_elem;
        pr_elem i2;
	
    | ParenExpr (e), [i1;i2] -> pr_elem i1; pp_expression e; pr_elem i2;
	
    | (Ident (_) | Constant _ | FunCall (_,_) | CondExpr (_,_,_) 
    | Sequence (_,_)
    | Assignment (_,_,_) 
    | Postfix (_,_) | Infix (_,_) | Unary (_,_) | Binary (_,_,_)
    | ArrayAccess (_,_) | RecordAccess (_,_) | RecordPtAccess (_,_)
    | SizeOfExpr (_) | SizeOfType (_) | Cast (_,_) 
    | StatementExpr (_) | Constructor _
    | ParenExpr (_)),_ -> raise Impossible
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
	
  and pp_arg_list es =
    es +> List.iter (fun (e, opt) -> 
      assert (List.length opt <= 1); (* opt must be a comma? *)
      opt +> List.iter (function x -> pr_elem x; pr_space());
      pp_argument e)
      
  and pp_argument argument = 
    let rec pp_action (ActMisc ii) = ii +> List.iter pr_elem in
    match argument with
    | Left e -> pp_expression e
    | Right wierd -> 
	(match wierd with
	| ArgType param -> pp_param param
	| ArgAction action -> pp_action action)
	  
(* ---------------------- *)
  and pp_statement = function
    | Labeled (Label (s, st)), [i1;i2] ->
	pr_outdent(); pr_elem i1; pr_elem i2; pr_nl(); pp_statement st
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
        pr_elem i1; start_block();
        statxs +> Common.print_between pr_nl pp_statement_seq;
        end_block(); pr_elem i2;
        
    | ExprStatement (None), [i] -> pr_elem i;
    | ExprStatement (None), [] -> ()
    | ExprStatement (Some e), [i] -> pp_expression e; pr_elem i
        (* the last ExprStatement of a for does not have a trailing
           ';' hence the [] for ii *)
    | ExprStatement (Some e), [] -> pp_expression e; 
    | Selection  (If (e, st1, st2)), i1::i2::i3::is -> 
        pr_elem i1; pr_space(); pr_elem i2; pp_expression e; pr_elem i3;
	indent_if_needed st1 (function _ -> pp_statement st1);
        (match (st2, is) with
        | ((ExprStatement None, []), [])  -> ()
        | ((ExprStatement None, []), [iifakend])  -> pr_elem iifakend
        | st2, [i4;iifakend] -> pr_elem i4;
	    indent_if_needed st2 (function _ -> pp_statement st2);
	    pr_elem iifakend
        | x -> raise Impossible
        )
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
          
          
    | Iteration  (For ((e1opt,il1),(e2opt,il2),(e3opt, il3),st)),
        [i1;i2;i3;iifakend] ->
	  
          pr_elem i1; pr_space();
          pr_elem i2;
          pp_statement (ExprStatement e1opt, il1);
          pp_statement (ExprStatement e2opt, il2);
          assert (null il3);
          pp_statement (ExprStatement e3opt, il3);
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
          
    | Jump (Goto s), [i1;i2;i3]               -> 
        pr_elem i1; pr_space(); pr_elem i2; pr_elem i3;
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
        | _ -> raise Impossible
        )
	  
    | NestedFunc def, ii -> 
        assert (null ii);
        pp_def def
    | MacroStmt, ii -> 
        ii +> List.iter pr_elem ;
	
    | ( Labeled (Label (_,_)) | Labeled (Case  (_,_)) 
    | Labeled (CaseRange  (_,_,_)) | Labeled (Default _)
    | Compound _ | ExprStatement _ 
    | Selection  (If (_, _, _)) | Selection  (Switch (_, _))
    | Iteration  (While (_, _)) | Iteration  (DoWhile (_, _)) 
    | Iteration  (For ((_,_), (_,_), (_, _), _))
    | Iteration  (MacroIteration (_,_,_))
    | Jump (Goto _) | Jump ((Continue|Break|Return)) | Jump (ReturnExpr _)
    | Jump (GotoComputed _)
    | Decl _ 
	), _ -> raise Impossible
	    
  and pp_statement_seq = function
    | StmtElem st -> pp_statement st
    | IfdefStmt ifdef -> pp_ifdef ifdef
    | CppDirectiveStmt cpp -> pp_directive cpp
    | IfdefStmt2 (ifdef, xxs) -> pp_ifdef_tree_sequence ifdef xxs
	  
(* ifdef XXX elsif YYY elsif ZZZ endif *)
  and pp_ifdef_tree_sequence ifdef xxs = 
    match ifdef with
    | if1::ifxs -> 
	pp_ifdef if1;
	pp_ifdef_tree_sequence_aux  ifxs xxs
    | _ -> raise Impossible
	  
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
	| (ColonExpr _), _ -> raise Impossible)
        ))
      
      
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
    fun ident sto ((qu, iiqu), (ty, iity)) attrs ->
      pp_base_type ((qu, iiqu), (ty, iity))  sto;
      (match (ident,ty) with
	(Some _,_) | (_,Pointer _) -> pr_space()
      |	_ -> ());
      pp_type_with_ident_rest ident ((qu, iiqu), (ty, iity)) attrs
	
	
  and (pp_base_type: fullType -> (storage * il) option -> unit) = 
    fun (qu, (ty, iity)) sto -> 
      let get_sto sto = 
        match sto with 
        | None -> [] | Some (s, iis) -> (*assert (List.length iis = 1);*) iis
      in
      let print_sto_qu (sto, (qu, iiqu)) = 
        let all_ii = get_sto sto ++ iiqu in
        all_ii 
          +> List.sort Ast_c.compare_pos
          +> Common.print_between pr_space pr_elem
          
      in
      let print_sto_qu_ty (sto, (qu, iiqu), iity) = 
        let all_ii = get_sto sto ++ iiqu ++ iity in
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
      | (Pointer t, [i])                           -> pp_base_type t sto
      | (ParenType t, _)                           -> pp_base_type t sto
      | (Array (eopt, t), [i1;i2])                 -> pp_base_type t sto
      | (FunctionType (returnt, paramst), [i1;i2]) -> 
          pp_base_type returnt sto
	      
	      
      | (StructUnion (su, sopt, fields),iis) -> 
          print_sto_qu (sto, qu);
	    
          (match sopt,iis with
          | Some s , [i1;i2;i3;i4] -> 
              pr_elem i1; pr_elem i2; pr_elem i3; 
          | None, [i1;i2;i3] -> 
              pr_elem i1; pr_elem i2; 
          | x -> raise Impossible
	  );
	    
          fields +> List.iter 
            (fun (xfield, iipttvirg_when_emptyfield) -> 
		
              match xfield with 
              | DeclarationField(FieldDeclList(onefield_multivars,iiptvirg))->
                  (match onefield_multivars with
                  | x::xs -> 
                    (* handling the first var. Special case, with the
                       first var, we print the whole type *)
			
		      (match x with
		      | (Simple (sopt, typ), iis), iivirg -> 
                        (* first var cant have a preceding ',' *)
			  assert (List.length iivirg = 0); 
			  let identinfo = 
                            (match sopt, iis with 
			      None,_ -> None 
                            | (Some s, [iis]) -> Some (s, iis) 
                            | x -> raise Impossible) in
			  pp_type_with_ident identinfo None typ Ast_c.noattr;
			    
		      | (BitField (sopt, typ, expr), ii), iivirg -> 
                      (* first var cant have a preceding ',' *)
			  assert (List.length iivirg = 0); 
			  (match sopt, ii with
			  | (None , [idot]) -> 
			      pp_type typ;
			      pr_elem idot;
			      pp_expression expr
			  | (Some s, [is;idot]) -> 
			      pp_type_with_ident
				(Some (s, is)) None typ Ast_c.noattr;
			      pr_elem idot;
			      pp_expression expr
			  | x -> raise Impossible
			)); (* match x, first onefield_multivars *)
			
                  (* for other vars *)
		      xs +> List.iter (function
			| (Simple (sopt, typ), iis), iivirg -> 
			    iivirg +> List.iter pr_elem;
			    let identinfo = 
			      (match sopt, iis with 
			      | None,_ -> None 
			      | (Some s, [iis]) -> Some (s, iis) 
			      | x -> raise Impossible) 
			    in
			    pp_type_with_ident_rest identinfo typ Ast_c.noattr

			| (BitField (sopt, typ, expr), ii), iivirg -> 
			    iivirg +> List.iter pr_elem;
			    (match sopt, ii with
			    | (Some s, [is;idot]) -> 
				pp_type_with_ident_rest
				  (Some (s, is)) typ Ast_c.noattr;
				pr_elem idot;
				pp_expression expr
			    | x -> raise Impossible
			    )); (* iter other vars *)
			
		  | [] -> raise Impossible
		  ); (* onefield_multivars *)
		  assert (List.length iiptvirg = 1);
		  iiptvirg +> List.iter pr_elem;
		    
		    
	      | MacroStructDeclTodo -> pr2 "MacroTodo"
		      
		      
	      | EmptyField -> iipttvirg_when_emptyfield +> List.iter pr_elem
		      
	      | CppDirectiveStruct cpp -> pp_directive cpp
	      | IfdefStruct ifdef -> pp_ifdef ifdef
	  );
	    
          (match sopt,iis with
          | Some s , [i1;i2;i3;i4] -> pr_elem i4
          | None, [i1;i2;i3] ->       pr_elem i3; 
          | x -> raise Impossible
	  );
	    
	    
	    
      | (Enum  (sopt, enumt), iis) -> 
          print_sto_qu (sto, qu);
	    
          (match sopt, iis with
          | (Some s, ([i1;i2;i3;i4]|[i1;i2;i3;i4;_])) -> 
              pr_elem i1; pr_elem i2; pr_elem i3;
          | (None, ([i1;i2;i3]|[i1;i2;i3;_])) -> 
              pr_elem i1; pr_elem i2
          | x -> raise Impossible
	  );
	    
          enumt +> List.iter (fun (((s, eopt),ii_s_eq), iicomma) -> 
            assert (List.length iicomma <= 1);
            iicomma +> List.iter (function x -> pr_elem x; pr_space());
            (match eopt, ii_s_eq with
            | None, [is] -> pr_elem is;
            | Some e, [is;ieq] -> pr_elem is; pr_elem ieq; pp_expression e
            | _ -> raise Impossible
	  ));
	    
          (match sopt, iis with
          | (Some s, [i1;i2;i3;i4]) ->    pr_elem i4
          | (Some s, [i1;i2;i3;i4;i5]) -> 
              pr_elem i5; pr_elem i4 (* trailing comma *)
          | (None, [i1;i2;i3]) ->         pr_elem i3
          | (None, [i1;i2;i3;i4]) ->      
              pr_elem i4; pr_elem i3 (* trailing comma *)
		  
		  
          | x -> raise Impossible
	  );
	    
	    
      | (BaseType _, iis) -> 
          print_sto_qu_ty (sto, qu, iis);
	    
      | (StructUnionName (s, structunion), iis) -> 
          assert (List.length iis = 2);
          print_sto_qu_ty (sto, qu, iis);
	    
      | (EnumName  s, iis) -> 
          assert (List.length iis = 2);
          print_sto_qu_ty (sto, qu, iis);
	    
      | (TypeName (s,_typ), iis) -> 
          assert (List.length iis = 1);  
          print_sto_qu_ty (sto, qu, iis);
	    
      | (TypeOfExpr (e), iis) -> 
          print_sto_qu (sto, qu);
          (match iis with
          | [itypeof;iopar;icpar] -> 
              pr_elem itypeof; pr_elem iopar;
              pp_expression e;
              pr_elem icpar;
          | _ -> raise Impossible
          )
	      
      | (TypeOfType (t), iis) -> 
          print_sto_qu (sto, qu);
          (match iis with
          | [itypeof;iopar;icpar] -> 
              pr_elem itypeof; pr_elem iopar;
              pp_type t; 
              pr_elem icpar;
          | _ -> raise Impossible
	  )
	      
      | (Pointer _ | (*ParenType _ |*) Array _ | FunctionType _ 
            (* | StructUnion _ | Enum _ | BaseType _ *)
            (* | StructUnionName _ | EnumName _ | TypeName _  *)
            (* | TypeOfExpr _ | TypeOfType _ *)
         ), _ -> raise Impossible
		
      
      
(* used because of DeclList, in    int i,*j[23];  we dont print anymore the 
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
      | (BaseType _, iis)                       -> print_ident ident
      | (Enum  (sopt, enumt), iis)              -> print_ident ident
      | (StructUnion (_, sopt, fields),iis)     -> print_ident ident
      | (StructUnionName (s, structunion), iis) -> print_ident ident
      | (EnumName  s, iis)                      -> print_ident ident
      | (TypeName (s,_typ), iis)                -> print_ident ident
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
      | (ParenType (q1, (Pointer (q2, (FunctionType t, ii3))   , 
                         [ipointer])  ), [i1;i2]) ->  
			   pp_type_left (q2, (FunctionType t, ii3));
			   pr_elem i1;
			   pr_elem ipointer;
			   print_ident ident;
			   pr_elem i2;
			   pp_type_right (q2, (FunctionType t, ii3));
			   
      (* another ugly special case *)
      | (ParenType 
           (q1, (Array (eopt,
			(q2, (Pointer 
				(q3, (FunctionType t, iifunc)), 
			      [ipointer]))),
		 [iarray1;iarray2])), [i1;i2]) -> 
		   pp_type_left (q3, (FunctionType t, iifunc));
		   pr_elem i1;
		   pr_elem ipointer;
		   print_ident ident;
		   pr_elem iarray1;
		   do_option pp_expression eopt;
		   pr_elem iarray2;
		   pr_elem i2;
		   pp_type_right (q3, (FunctionType t, iifunc))
		     
		     
		     
      | (ParenType t, [i1;i2]) ->  
          pr2 "PB PARENTYPE ZARB, I forget about the ()";
          pp_type_with_ident_rest ident t attrs;
          
	  
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
	  raise Impossible
              
	      
  and (pp_type_left: fullType -> unit) = 
    fun ((qu, iiqu), (ty, iity)) -> 
      match ty, iity with
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
      | (TypeName (s,_typ), iis) -> ()
	    
      | TypeOfType _, _ -> ()
      | TypeOfExpr _, _ -> ()
	    
      | (FunctionType _ | Array _ | Pointer _), _ -> raise Impossible

      
  and pp_param ((b, sopt, t), ii_b_s) =
    match b, sopt, ii_b_s with
    | false, None, [] -> 
	pp_type t
    | true, None, [i1] -> 
	pr_elem i1;
	pp_type t
	  
    | false, Some s, [i1] -> 
	pp_type_with_ident
          (Some (s, i1)) None t Ast_c.noattr;
    | true, Some s, [i1;i2] -> 
	pr_elem i1;
	pp_type_with_ident
          (Some (s, i2)) None t Ast_c.noattr;
    | _ -> raise Impossible                
	  
	  
  and pp_type_right (((qu, iiqu), (ty, iity)) : fullType) = 
    match ty, iity with
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
    | (TypeName (s,_typ), iis) -> ()
	    
    | TypeOfType _, _ -> ()
    | TypeOfExpr _, _ -> ()
	    
    | (FunctionType _ | Array _ | Pointer _), _ -> raise Impossible
      
  and pp_type t =
    pp_type_with_ident None None t Ast_c.noattr
      
(* ---------------------- *)
  and pp_decl = function
    | DeclList ((({v_namei = var; v_type = returnType;
                    v_storage = storage; v_attr = attrs;
                  },[])::xs), 
		iivirg::ifakestart::iisto) -> 
		  
		  pr_elem ifakestart;
		  
      (* old: iisto +> List.iter pr_elem; *)
		  
		  
      (* handling the first var. Special case, we print the whole type *)
		  (match var with
		  | Some ((s, ini),  iis::iini) -> 
		      pp_type_with_ident
			(Some (s, iis)) (Some (storage, iisto))
			returnType attrs;
		      ini +> do_option (fun init -> 
			List.iter pr_elem iini; pp_init init);
		  | None -> pp_type returnType
		  | _ -> raise Impossible
			);
		  
      (* for other vars, we just call pp_type_with_ident_rest. *)
		  xs +> List.iter (function
		    | ({v_namei = Some ((s, ini), iis::iini);
			 v_type = returnType;
			 v_storage = storage2;
			 v_attr = attrs;
		       }, iivirg) ->
			 
			 assert (storage2 = storage);
			 iivirg +> List.iter pr_elem;
			 pp_type_with_ident_rest
			   (Some (s, iis)) returnType attrs;
			 ini +> do_option (fun (init) -> 
			   List.iter pr_elem iini; pp_init init);
			 
			 
		    | x -> raise Impossible
		  );
		  
		  pr_elem iivirg;
		  
    | MacroDecl ((s, es), iis::lp::rp::iiend::ifakestart::iisto) -> 
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
	
    | (DeclList (_, _) | (MacroDecl _)) -> raise Impossible
	  
	  
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
	  ), _ -> raise Impossible
      
      
      
  and pp_designator = function
    | DesignatorField (s), [i1; i2] -> 
	pr_elem i1; pr_elem i2; 
    | DesignatorIndex (expression), [i1;i2] -> 
	pr_elem i1; pp_expression expression; pr_elem i2; 
	
    | DesignatorRange (e1, e2), [iocro;iellipsis;iccro] -> 
	pr_elem iocro; pp_expression e1; pr_elem iellipsis;
	pp_expression e2; pr_elem iccro; 
	
    | (DesignatorField _ | DesignatorIndex _ | DesignatorRange _
	), _ -> raise Impossible
	    
	    
(* ---------------------- *)
  and pp_attributes pr_elem pr_space attrs =
    attrs +> List.iter (fun (attr, ii) -> 
      ii +> List.iter pr_elem;
    );
    
(* ---------------------- *)
  and pp_def def = 
    let defbis, ii = def in
    match ii with 
    | is::iifunc1::iifunc2::i1::i2::ifakestart::isto -> 
	
	let {f_name = s;
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
        pr_elem is;
	
	
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
               assert (null iicomma);
               assert (null ii_b_s);
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
        paramst +> List.iter (fun (param,iicomma) -> 
          assert ((List.length iicomma) <= 1);
          iicomma +> List.iter (function x -> pr_elem x; pr_space());
          
          pp_param param;
        );
        iib +> List.iter pr_elem;
        
	
        pr_elem iifunc2;
        pr_elem i1; 
        statxs +> List.iter pp_statement_seq;
        pr_elem i2;
    | _ -> raise Impossible
	  
	  
	  
(* ---------------------- *)
	  
  and pp_ifdef ifdef = 
    match ifdef with
    | IfdefDirective (ifdef, ii) -> 
	List.iter pr_elem ii
	  
	  
  and pp_directive = function
    | Include {i_include = (s, ii);} -> 
	let (i1,i2) = Common.tuple_of_list2 ii in
	pr_elem i1; pr_elem i2
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
              | _ -> raise Impossible
	      )
          | DefineFunction def -> pp_def def
		
          | DefineType ty -> pp_type ty
          | DefineText (s, ii) -> List.iter pr_elem ii
          | DefineEmpty -> ()
          | DefineInit ini -> pp_init ini
		
          | DefineTodo -> pr2 "DefineTodo"
	in
	(match defkind with
	| DefineVar -> ()
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
          
    | Undef (s, ii) -> 
	List.iter pr_elem ii
    | PragmaAndCo (ii) -> 
	List.iter pr_elem ii in
	  
	  
	  
	  
  let pp_toplevel = function
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
	
    | (MacroTop _) -> raise Impossible in




  let pp_flow n =
    match F.unwrap n  with
    | F.FunHeader ({f_name =idb;
                     f_type = (rett, (paramst,(isvaargs,iidotsb)));
                     f_storage = stob;
                     f_body = body;
                     f_attr = attrs},ii) ->
		     
		       assert(null body);
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
	pp_statement (ExprStatement eopt, ii) 
	
    | F.IfHeader (_, (e,ii)) 
    | F.SwitchHeader (_, (e,ii))
    | F.WhileHeader (_, (e,ii))
    | F.DoWhileTail (e,ii) -> 
        (*
           iif ii;
           vk_expr bigf e
        *)
	pr2 "XXX";
      
      
    | F.ForHeader (_st, (((e1opt,i1), (e2opt,i2), (e3opt,i3)), ii)) -> 
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
      
      
    | F.Include {i_include = (s, ii);} -> 
        (* iif ii; *)
	pr2 "XXX"
      
      
    | F.MacroTop (s, args, ii) -> 
        (* iif ii;
           vk_argument_list bigf args *)
	pr2 "XXX"
      
      
    | F.Break    (st,((),ii)) -> 
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
    | F.Goto  (st, (s,ii)) -> 
        (* iif ii *)
	pr2 "XXX"
    | F.Label (st, (s,ii)) -> 
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
      
      
    | F.IfdefHeader (info) -> 
	pp_ifdef info
    | F.IfdefElse (info) -> 
	pp_ifdef info
    | F.IfdefEndif (info) -> 
	pp_ifdef info
	
    | F.DefineTodo -> 
	pr2 "XXX"
      
      
    | (F.TopNode|F.EndNode|
      F.ErrorExit|F.Exit|F.Enter|
      F.FallThroughNode|F.AfterNode|F.FalseNode|F.TrueNode|F.InLoopNode|
      F.Fake) ->
        pr2 "YYY" in


  {expression = pp_expression;
    arg_list = pp_arg_list;
    statement = pp_statement;
    decl = pp_decl;
    init = pp_init;
    param = pp_param;
    ty = pp_type;
    type_with_ident = pp_type_with_ident;
    toplevel = pp_toplevel;
    flow = pp_flow}
    
(*****************************************************************************)
    
(* Here we do not use (mcode, env). It is a simple C pretty printer. *)
let pr_elem info =
  let s = Ast_c.str_of_info info in
  if !Flag_parsing_c.pretty_print_comment_info then begin
    let before = !(info.comments_tag).mbefore in
    if not (null before) then begin
      pp "-->";
      before +> List.iter (fun (comment_like, pinfo) -> 
        let s = pinfo.Common.str in
        pp s
      );
      pp "<--";
    end;
  end;
  pp s
    
let pr_space _ = Format.print_space()

let pr_nl _ = ()
let pr_indent _ = ()
let pr_outdent _ = ()
let pr_unindent _ = ()

let ppc =
  pretty_print_c pr_elem pr_space pr_nl pr_outdent pr_indent pr_unindent
    
let pp_expression_simple = ppc.expression
let pp_statement_simple  = ppc.statement
let pp_type_simple       = ppc.ty
let pp_init_simple       = ppc.init
let pp_toplevel_simple   = ppc.toplevel
let pp_flow_simple       = ppc.flow

let pp_elem_sp pr_elem pr_space =
  pretty_print_c pr_elem pr_space pr_nl pr_outdent pr_indent pr_unindent

let pp_expression_gen pr_elem pr_space =
  (pp_elem_sp pr_elem pr_space).expression

let pp_arg_list_gen pr_elem pr_space =
  (pp_elem_sp pr_elem pr_space).arg_list

let pp_statement_gen pr_elem pr_space =
  (pp_elem_sp pr_elem pr_space).statement

let pp_decl_gen pr_elem pr_space =
  (pp_elem_sp pr_elem pr_space).decl

let pp_init_gen pr_elem pr_space =
  (pp_elem_sp pr_elem pr_space).init

let pp_param_gen pr_elem pr_space =
  (pp_elem_sp pr_elem pr_space).param

let pp_type_gen pr_elem pr_space =
  (pp_elem_sp pr_elem pr_space).ty

let pp_type_with_ident_gen pr_elem pr_space =
  (pp_elem_sp pr_elem pr_space).type_with_ident

let pp_program_gen pr_elem pr_space =
  (pp_elem_sp pr_elem pr_space).toplevel



let string_of_expression e = 
  Common.format_to_string (fun () ->
    pp_expression_simple e
      )
    
let (debug_info_of_node:
       Ograph_extended.nodei -> Control_flow_c.cflow -> string) = 
  fun nodei flow -> 
    let node = flow#nodes#assoc nodei in
    let s = Common.format_to_string (fun () ->
      pp_flow_simple node
    ) in
    let pos = Lib_parsing_c.min_pinfo_of_node node in
    (spf "%s(n%d)--> %s" (Common.string_of_parse_info_bis pos) nodei s)
      
