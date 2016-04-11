(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

open Common

module F = Control_flow_c

(*****************************************************************************)
(* The functor argument  *)
(*****************************************************************************)

(* info passed recursively in monad in addition to binding *)
type xinfo = {
  optional_storage_iso : bool;
  optional_qualifier_iso : bool;
  value_format_iso : bool;
  optional_declarer_semicolon_iso : bool;
  optional_attributes_iso : bool;
  current_rule_name : string; (* used for errors *)
  index : int list (* witness tree indices *)
}

module XTRANS = struct

  (* ------------------------------------------------------------------------*)
  (* Combinators history *)
  (* ------------------------------------------------------------------------*)
  (*
   * version0:
   *  type ('a, 'b) transformer =
   *    'a -> 'b -> Lib_engine.metavars_binding -> 'b
   *  exception NoMatch
   *
   * version1:
   *   type ('a, 'b) transformer =
   *    'a -> 'b -> Lib_engine.metavars_binding -> 'b option
   * use an exception monad
   *
   * version2:
   *    type tin = Lib_engine.metavars_binding
   *)

  (* ------------------------------------------------------------------------*)
  (* Standard type and operators  *)
  (* ------------------------------------------------------------------------*)

  type tin = {
    extra: xinfo;
    binding: Lib_engine.metavars_binding;
    binding0: Lib_engine.metavars_binding; (* inherited variable *)
  }
  type 'x tout = 'x option

  type ('a, 'b) matcher = 'a -> 'b  -> tin -> ('a * 'b) tout

  let (>>=) m f = fun tin ->
     match m tin with
     | None -> None
     | Some (a,b) -> f a b tin

  let return = fun x -> fun tin ->
    Some x

  (* can have fail in transform now that the process is deterministic ? *)
  let fail = fun tin ->
    None

  let (>||>) m1 m2 = fun tin ->
    match m1 tin with
    | None -> m2 tin
    | Some x -> Some x (* stop as soon as have found something *)

  let (>|+|>) m1 m2 = m1 >||> m2

  let (>&&>) f m = fun tin ->
    if f tin then m tin else fail tin

  let optional_storage_flag f = fun tin ->
    f (tin.extra.optional_storage_iso) tin

  let optional_qualifier_flag f = fun tin ->
    f (tin.extra.optional_qualifier_iso) tin

  let value_format_flag f = fun tin ->
    f (tin.extra.value_format_iso) tin

  let optional_declarer_semicolon_flag f = fun tin ->
    f (tin.extra.optional_declarer_semicolon_iso) tin

  let optional_attributes_flag f = fun tin ->
    f (tin.extra.optional_attributes_iso) tin

  let mode = Cocci_vs_c.TransformMode

  (* ------------------------------------------------------------------------*)
  (* Env *)
  (* ------------------------------------------------------------------------*)

  (* When env is used in + code, have to strip it more to avoid circular
  references due to local variable information *)

  let clean_env env =
    List.map
      (function (v,vl) ->
	match vl with
	| Ast_c.MetaExprVal(e,ml,ty) ->
	    (v,Ast_c.MetaExprVal(Lib_parsing_c.real_al_expr e,ml,ty))
	| Ast_c.MetaExprListVal(es) ->
	    (v,Ast_c.MetaExprListVal(Lib_parsing_c.real_al_arguments es))
	| Ast_c.MetaTypeVal(ty) ->
	    (v,Ast_c.MetaTypeVal(Lib_parsing_c.real_al_type ty))
	| Ast_c.MetaInitVal(i) ->
	    (v,Ast_c.MetaInitVal(Lib_parsing_c.real_al_init i))
	| Ast_c.MetaInitListVal(is) ->
	    (v,Ast_c.MetaInitListVal(Lib_parsing_c.real_al_inits is))
	| Ast_c.MetaDeclVal(d) ->
	    (v,Ast_c.MetaDeclVal(Lib_parsing_c.real_al_decl d))
	| Ast_c.MetaStmtVal(s,ty) ->
	    (v,Ast_c.MetaStmtVal(Lib_parsing_c.real_al_statement s,ty))
	(* These don't contain local variables, but the cocci_tag field
	   causes problems too.  Why is this not needd for other metavars? *)
	| Ast_c.MetaAssignOpVal(b) ->
	    (v,Ast_c.MetaAssignOpVal(Lib_parsing_c.real_al_assignop b))
	| Ast_c.MetaBinaryOpVal(b) ->
	    (v,Ast_c.MetaBinaryOpVal(Lib_parsing_c.real_al_binop b))
	| _ -> (v,vl))
      env


  (* ------------------------------------------------------------------------*)
  (* Exp  *)
  (* ------------------------------------------------------------------------*)
  let cocciExp = fun expf expa node -> fun tin ->

    let bigf = {
      Visitor_c.default_visitor_c_s with
      Visitor_c.kexpr_s = (fun (k, bigf) expb ->
	match expf expa expb tin with
	| None -> (* failed *) k expb
	| Some (x, expb) -> expb);
    }
    in
    Some (expa, Visitor_c.vk_node_s bigf node)


  (* same as cocciExp, but for expressions in an expression, not expressions
     in a node *)
  let cocciExpExp = fun mc expf expa expb -> fun tin ->
    match mc with
      Ast_cocci.MINUS _ -> Some (expa,expb) (* do nothing *)
    | _ ->

    let bigf = {
      Visitor_c.default_visitor_c_s with
      Visitor_c.kexpr_s = (fun (k, bigf) expb ->
	match expf expa expb tin with
	| None -> (* failed *) k expb
	| Some (x, expb) -> expb);
    }
    in
    Some (expa, Visitor_c.vk_expr_s bigf expb)


  let cocciTy = fun expf expa node -> fun tin ->

    let bigf = {
      Visitor_c.default_visitor_c_s with
      Visitor_c.ktype_s = (fun (k, bigf) expb ->
	match expf expa expb tin with
	| None -> (* failed *) k expb
	| Some (x, expb) -> expb);
    }
    in
    Some (expa, Visitor_c.vk_node_s bigf node)

  let cocciId = fun expf expa node -> fun tin ->

    let bigf = {
      Visitor_c.default_visitor_c_s with
      Visitor_c.kname_s = (fun (k, bigf) expb ->
	match expf expa expb tin with
	| None -> (* failed *) k expb
	| Some (x, expb) -> expb);
    }
    in
    Some (expa, Visitor_c.vk_node_s bigf node)

  let cocciInit = fun expf expa node -> fun tin ->

    let bigf = {
      Visitor_c.default_visitor_c_s with
      Visitor_c.kini_s = (fun (k, bigf) expb ->
	match expf expa expb tin with
	| None -> (* failed *) k expb
	| Some (x, expb) -> expb);
    }
    in
    Some (expa, Visitor_c.vk_node_s bigf node)


  (* ------------------------------------------------------------------------*)
  (* Tokens *)
  (* ------------------------------------------------------------------------*)
   let check_pos info mck pos =
     match mck with
     | Ast_cocci.PLUS _ -> raise (Impossible 51)
     | Ast_cocci.CONTEXT (Ast_cocci.FixPos (i1,i2),_)
     | Ast_cocci.MINUS   (Ast_cocci.FixPos (i1,i2),_,_,_) ->
         pos <= i2 && pos >= i1
     | Ast_cocci.CONTEXT (Ast_cocci.DontCarePos,_)
     | Ast_cocci.MINUS   (Ast_cocci.DontCarePos,_,_,_) ->
         true
     | _ ->
	 match info with
	   Some info ->
	     failwith
	       (Printf.sprintf
		  "weird: dont have position info for the mcodekind in line %d column %d"
		  info.Ast_cocci.line info.Ast_cocci.column)
	 | None ->
	     failwith "weird: dont have position info for the mcodekind"

    (* these remove constraints, at least those that contain pcre regexps,
       which cannot be compared (problem in the unparser) *)
  let strip_anything anything =
    let donothing r k e = k e in
    let mcode mc = mc in
    let ident r k e =
      let e = k e in
      match Ast_cocci.unwrap e with
	Ast_cocci.MetaId(name,constraints,u,i) ->
          Ast_cocci.rewrap e
	    (Ast_cocci.MetaId(name,Ast_cocci.IdNoConstraint,u,i))
      | Ast_cocci.MetaFunc(name,constraints,u,i) ->
          Ast_cocci.rewrap e
	    (Ast_cocci.MetaFunc(name,Ast_cocci.IdNoConstraint,u,i))
      | Ast_cocci.MetaLocalFunc(name,constraints,u,i) ->
          Ast_cocci.rewrap e
	    (Ast_cocci.MetaLocalFunc(name,Ast_cocci.IdNoConstraint,u,i))
      |  _ -> e in
    let expression r k e =
      let e = k e in
      match Ast_cocci.unwrap e with
	Ast_cocci.MetaErr(name,constraints,u,i) ->
	  Ast_cocci.rewrap e
	    (Ast_cocci.MetaErr(name,Ast_cocci.NoConstraint,u,i))
      | Ast_cocci.MetaExpr(name,constraints,u,ty,form,i) ->
          Ast_cocci.rewrap e
	    (Ast_cocci.MetaExpr(name,Ast_cocci.NoConstraint,u,ty,form,i))
      | _ -> e in
    let fn = Visitor_ast.rebuilder
	mcode mcode mcode mcode mcode mcode mcode mcode mcode
	mcode mcode mcode mcode mcode
	donothing donothing donothing donothing donothing
	ident expression donothing donothing donothing donothing
	donothing donothing donothing donothing donothing
	donothing donothing donothing donothing donothing donothing in

  fn.Visitor_ast.rebuilder_anything anything

  let strip_minus_code = function
      Ast_cocci.REPLACEMENT(l,c) ->
	Ast_cocci.REPLACEMENT(List.map (List.map strip_anything) l,c)
    | Ast_cocci.NOREPLACEMENT -> Ast_cocci.NOREPLACEMENT
  let strip_context_code = function
      Ast_cocci.BEFORE(l,c) ->
	Ast_cocci.BEFORE(List.map (List.map strip_anything) l,c)
    | Ast_cocci.AFTER(l,c) ->
	Ast_cocci.AFTER(List.map (List.map strip_anything) l,c)
    | Ast_cocci.BEFOREAFTER(l1,l2,c) ->
	Ast_cocci.BEFOREAFTER(List.map (List.map strip_anything) l1,
			      List.map (List.map strip_anything) l2,c)
    | Ast_cocci.NOTHING -> Ast_cocci.NOTHING
  let strip_mck_code = function
      Ast_cocci.MINUS(p,l,a,repl) ->
	Ast_cocci.MINUS(p,l,a,strip_minus_code repl)
    | Ast_cocci.CONTEXT(p,ba) -> Ast_cocci.CONTEXT(p,strip_context_code ba)
    | Ast_cocci.PLUS(c) -> Ast_cocci.PLUS(c)

  let tag_with_mck mck ib = fun tin ->

    let cocciinforef = ib.Ast_c.cocci_tag in
    let (oldmcode, oldenvs) = Ast_c.mcode_and_env_of_cocciref cocciinforef in

    let mck =
      (* coccionly:
      if !Flag_parsing_cocci.sgrep_mode
      then Sgrep.process_sgrep ib mck
      else
      *)
        mck
    in
    (match mck, Ast_c.pinfo_of_info ib with
    | _,                  Ast_c.AbstractLineTok _ -> raise (Impossible 52)
    | Ast_cocci.MINUS(_), Ast_c.ExpandedTok _ ->
        failwith
	  (Printf.sprintf
	     "%s: %d: try to delete an expanded token: %s"
	     (Ast_c.file_of_info ib)
	     (Ast_c.line_of_info ib) (Ast_c.str_of_info ib))
    | _ -> ()
    );

    let many_context_count = function
	Ast_cocci.BEFORE(_,Ast_cocci.MANY) | Ast_cocci.AFTER(_,Ast_cocci.MANY)
      |	 Ast_cocci.BEFOREAFTER(_,_,Ast_cocci.MANY) -> true
      |	_ -> false in

    let many_minus_count = function
	Ast_cocci.REPLACEMENT(_,Ast_cocci.MANY) -> true
      |	_ -> false in

    (match (oldmcode,mck) with
    | (Ast_cocci.CONTEXT(_,Ast_cocci.NOTHING),      _) ->
	(* nothing there, so take the new stuff *)
	let update_inst inst = function
	    Ast_cocci.MINUS (pos,_,adj,any_xxs) ->
	      Ast_cocci.MINUS (pos,inst,adj,any_xxs)
	  | mck -> mck in
	let mck = strip_mck_code (update_inst tin.extra.index mck) in
	(* clean_env actually only needed if there is an addition
	   not sure the extra efficiency would be worth duplicating the code *)
        cocciinforef :=
	  Some (mck, [clean_env tin.binding])
    | (_,   Ast_cocci.CONTEXT(_,Ast_cocci.NOTHING)) ->
	(* can this case occur? stay with the old stuff *)
	()
    | (Ast_cocci.MINUS(old_pos,old_inst,old_adj,Ast_cocci.NOREPLACEMENT),
       Ast_cocci.MINUS(new_pos,new_inst,new_adj,Ast_cocci.NOREPLACEMENT))
	(* Now put no constraints on double remove.  This is needed for
	   tests/multidecl2: - T i; with int *i, i[12], k; (for symbol i)
	   The positions of the two matches are different because the types
	   end in different places (on * and then on ]) *)
      (*when old_pos = new_pos*)
	    (* not sure why the following condition is useful.
	       should be ok to double remove even if the environments are
	       different *)
          (* &&
	  (List.mem tin.binding oldenvs or !Flag.sgrep_mode2) *)
	    (* no way to combine adjacency information, just drop one *)
      ->
        cocciinforef := Some
	  (Ast_cocci.MINUS
	     (old_pos,Common.union_set old_inst new_inst,old_adj,
	      Ast_cocci.NOREPLACEMENT),
	   [tin.binding]);
        (if !Flag_matcher.show_misc
        then pr2_once "already tagged but only removed, so safe")

    (* ++ cases *)
    | (Ast_cocci.MINUS(old_pos,old_inst,old_adj,old_modif),
       Ast_cocci.MINUS(new_pos,new_inst,new_adj,new_modif))
	when old_pos = new_pos &&
	  old_modif = strip_minus_code new_modif &&
	  many_minus_count old_modif ->

          cocciinforef :=
	    Some(Ast_cocci.MINUS(old_pos,Common.union_set old_inst new_inst,
				 old_adj,old_modif),
		 (clean_env tin.binding)::oldenvs)

    | (Ast_cocci.CONTEXT(old_pos,old_modif),
       Ast_cocci.CONTEXT(new_pos,new_modif))
	when old_pos = new_pos &&
	  old_modif = strip_context_code new_modif &&
	  many_context_count old_modif ->
	    (* iteration only allowed on context; no way to replace something
	       more than once; now no need for iterable; just check a flag *)

          cocciinforef :=
	    Some(Ast_cocci.CONTEXT(old_pos,old_modif),
		 (clean_env tin.binding)::oldenvs)

    (* non ++ but same modif for two reasons *)
    | (Ast_cocci.MINUS(old_pos,old_inst,old_adj,old_modif),
       Ast_cocci.MINUS(new_pos,new_inst,new_adj,new_modif))
	when old_pos = new_pos &&
	  old_modif = strip_minus_code new_modif &&
	  List.mem (clean_env tin.binding) oldenvs ->

          cocciinforef :=
	    Some(Ast_cocci.MINUS(old_pos,Common.union_set old_inst new_inst,
				 old_adj,old_modif),
		 oldenvs)

    | (Ast_cocci.CONTEXT(old_pos,old_modif),
       Ast_cocci.CONTEXT(new_pos,new_modif))
	when old_pos = new_pos &&
	  old_modif = strip_context_code new_modif &&
	  List.mem (clean_env tin.binding) oldenvs ->
	    (* iteration only allowed on context; no way to replace something
	       more than once; now no need for iterable; just check a flag *)

          cocciinforef := Some(Ast_cocci.CONTEXT(old_pos,old_modif),oldenvs)

    | _ ->

(*
	(match (oldmcode,mck) with
	| (Ast_cocci.CONTEXT(old_pos,old_modif),
	   Ast_cocci.CONTEXT(new_pos,new_modif)) ->
	     Printf.printf "failed because of %b %b %b\n"
	       (old_pos = new_pos)
	       (old_modif = strip_context_code new_modif)
	       (many_context_count old_modif);
	     Printf.printf "failed because of %s %s\n"
	       (Dumper.dump old_modif)
	       (Dumper.dump (strip_context_code new_modif))
	| _ -> Printf.printf "other failure\n");
*)

          (* coccionly:
          if !Flag.sgrep_mode2
          then ib (* safe *)
          else
          *)
             begin
            (* coccionly:
               pad: if don't want cocci write:
                failwith
	        (match Ast_c.pinfo_of_info ib with
		  Ast_c.FakeTok _ -> "already tagged fake token"
             *)
	       let pm str mcode env =
		 Printf.sprintf
		   "%s modification:\n%s\nAccording to environment %d:\n%s\n"
		   str
		   (Common.format_to_string
		      (function _ ->
			Pretty_print_cocci.print_mcodekind mcode))
		   (List.length env)
		   (String.concat "\n"
		      (List.map
			 (function ((r,vr),vl) ->
			   Printf.sprintf "   %s.%s -> %s" r vr
			     (Common.format_to_string
				(function _ ->
				  Pretty_print_engine.pp_binding_kind vl)))
			 env)) in
	       flush stdout; flush stderr;
	       Common.pr2
		 ("\n"^ (String.concat "\n"
			   (List.map (pm "previous" oldmcode) oldenvs)) ^ "\n"
		  ^ (pm "current" mck tin.binding));
               failwith
	         (match Ast_c.pinfo_of_info ib with
		   Ast_c.FakeTok _ ->
		     Printf.sprintf "%s: already tagged fake token\n"
		       tin.extra.current_rule_name
		| _ ->
		    Printf.sprintf
		      "%s: already tagged token:\nC code context\n%s"
		      tin.extra.current_rule_name
	              (Common.error_message (Ast_c.file_of_info ib)
			 (Ast_c.str_of_info ib, Ast_c.opos_of_info ib)))
            end);
    ib

  let tokenf ia ib = fun tin ->
    let (_,i,mck,_) = ia in
    let pos = Ast_c.info_to_fixpos ib in
    if check_pos (Some i) mck pos
    then return (ia, tag_with_mck mck ib tin) tin
    else fail tin

  let tokenf_mck mck ib = fun tin ->
    let pos = Ast_c.info_to_fixpos ib in
    if check_pos None mck pos
    then return (mck, tag_with_mck mck ib tin) tin
    else fail tin


  (* ------------------------------------------------------------------------*)
  (* Distribute mcode *)
  (* ------------------------------------------------------------------------*)

  (* When in the SP we attach something to a metavariable, or delete it, as in
   * - S
   * + foo();
   * we have to minusize all the token that compose S in the C code, and
   * attach the 'foo();'  to the right token, the one at the very right.
   *)

  type 'a distributer =
      (Ast_c.info -> Ast_c.info) *  (* what to do on left *)
      (Ast_c.info -> Ast_c.info) *  (* what to do on middle *)
      (Ast_c.info -> Ast_c.info) *  (* what to do on right *)
      (Ast_c.info -> Ast_c.info) -> (* what to do on both *)
      'a -> 'a

  let distribute_mck mcodekind distributef expr tin =
    match mcodekind with
    | Ast_cocci.MINUS (pos,_,adj,any_xxs) ->
	let inst = tin.extra.index in
        distributef (
          (fun ib ->
	    tag_with_mck (Ast_cocci.MINUS (pos,inst,adj,any_xxs)) ib tin),
          (fun ib ->
	    tag_with_mck
	      (Ast_cocci.MINUS (pos,inst,adj,Ast_cocci.NOREPLACEMENT)) ib tin),
          (fun ib ->
	    tag_with_mck
	      (Ast_cocci.MINUS (pos,inst,adj,Ast_cocci.NOREPLACEMENT)) ib tin),
          (fun ib ->
	    tag_with_mck (Ast_cocci.MINUS (pos,inst,adj,any_xxs)) ib tin)
        ) expr
    | Ast_cocci.CONTEXT (pos,any_befaft) ->
        (match any_befaft with
        | Ast_cocci.NOTHING -> expr

        | Ast_cocci.BEFORE (xxs,c) ->
            distributef (
              (fun ib -> tag_with_mck
                (Ast_cocci.CONTEXT (pos,Ast_cocci.BEFORE (xxs,c))) ib tin),
              (fun x -> x),
              (fun x -> x),
              (fun ib -> tag_with_mck
                (Ast_cocci.CONTEXT (pos,Ast_cocci.BEFORE (xxs,c))) ib tin)
            ) expr
        | Ast_cocci.AFTER (xxs,c) ->
            distributef (
              (fun x -> x),
              (fun x -> x),
              (fun ib -> tag_with_mck
                (Ast_cocci.CONTEXT (pos,Ast_cocci.AFTER (xxs,c))) ib tin),
              (fun ib -> tag_with_mck
                (Ast_cocci.CONTEXT (pos,Ast_cocci.AFTER (xxs,c))) ib tin)
            ) expr

        | Ast_cocci.BEFOREAFTER (xxs, yys, c) ->
            distributef (
              (fun ib -> tag_with_mck
                (Ast_cocci.CONTEXT (pos,Ast_cocci.BEFORE (xxs,c))) ib tin),
              (fun x -> x),
              (fun ib -> tag_with_mck
                (Ast_cocci.CONTEXT (pos,Ast_cocci.AFTER (yys,c))) ib tin),
              (fun ib -> tag_with_mck
                (Ast_cocci.CONTEXT (pos,Ast_cocci.BEFOREAFTER (xxs,yys,c)))
                ib tin)
            ) expr

        )
    | Ast_cocci.PLUS _ -> raise (Impossible 53)


  (* use new strategy, collect ii, sort, recollect and tag *)

  let mk_bigf (maxpos, minpos) (lop,mop,rop,bop) =
    let bigf = {
      Visitor_c.default_visitor_c_s with
        Visitor_c.kinfo_s = (fun (k,bigf) i ->
          let pos = Ast_c.info_to_fixpos i in
          match () with
          | _ when Ast_cocci.equal_pos pos maxpos &&
	      Ast_cocci.equal_pos pos minpos -> bop i
          | _ when Ast_cocci.equal_pos pos maxpos -> rop i
          | _ when Ast_cocci.equal_pos pos minpos -> lop i
          | _ -> mop i
        )
    } in
    bigf

  let distribute_mck_expr (maxpos, minpos) = fun (lop,mop,rop,bop) -> fun x ->
    Visitor_c.vk_expr_s (mk_bigf (maxpos, minpos) (lop,mop,rop,bop)) x

  let distribute_mck_assignOp (maxpos, minpos) = fun (lop,mop,rop,bop) -> fun x ->
    Visitor_c.vk_assignOp_s (mk_bigf (maxpos, minpos) (lop,mop,rop,bop)) x

  let distribute_mck_binaryOp (maxpos, minpos) = fun (lop,mop,rop,bop) -> fun x ->
    Visitor_c.vk_binaryOp_s (mk_bigf (maxpos, minpos) (lop,mop,rop,bop)) x

  let distribute_mck_args (maxpos, minpos) = fun (lop,mop,rop,bop) -> fun x ->
    Visitor_c.vk_args_splitted_s (mk_bigf (maxpos, minpos) (lop,mop,rop,bop)) x

  let distribute_mck_type (maxpos, minpos) = fun (lop,mop,rop,bop) -> fun x ->
    Visitor_c.vk_type_s (mk_bigf (maxpos, minpos) (lop,mop,rop,bop)) x

  let distribute_mck_decl (maxpos, minpos) = fun (lop,mop,rop,bop) -> fun x ->
    Visitor_c.vk_decl_s (mk_bigf (maxpos, minpos) (lop,mop,rop,bop)) x

  let distribute_mck_field (maxpos, minpos) = fun (lop,mop,rop,bop) -> fun x ->
    Visitor_c.vk_struct_field_s (mk_bigf (maxpos, minpos) (lop,mop,rop,bop)) x

  let distribute_mck_ini (maxpos, minpos) = fun (lop,mop,rop,bop) -> fun x ->
    Visitor_c.vk_ini_s (mk_bigf (maxpos, minpos) (lop,mop,rop,bop)) x

  let distribute_mck_inis (maxpos, minpos) = fun (lop,mop,rop,bop) -> fun x ->
    Visitor_c.vk_inis_splitted_s (mk_bigf (maxpos, minpos) (lop,mop,rop,bop)) x

  let distribute_mck_param (maxpos, minpos) = fun (lop,mop,rop,bop) -> fun x ->
    Visitor_c.vk_param_s (mk_bigf (maxpos, minpos) (lop,mop,rop,bop)) x

  let distribute_mck_params (maxpos, minpos) = fun (lop,mop,rop,bop) ->fun x ->
    Visitor_c.vk_params_splitted_s (mk_bigf (maxpos, minpos) (lop,mop,rop,bop))
      x

  let distribute_mck_fragments (maxpos, minpos) =
    fun (lop,mop,rop,bop) ->fun x ->
      Visitor_c.vk_string_fragments_splitted_s
	(mk_bigf (maxpos, minpos) (lop,mop,rop,bop))
	x

  let distribute_mck_format (maxpos, minpos) = fun (lop,mop,rop,bop) ->fun x ->
    Visitor_c.vk_string_format_s (mk_bigf (maxpos, minpos) (lop,mop,rop,bop))
      x

  let distribute_mck_node (maxpos, minpos) = fun (lop,mop,rop,bop) ->fun x ->
    Visitor_c.vk_node_s (mk_bigf (maxpos, minpos) (lop,mop,rop,bop))
      x

  let distribute_mck_enum_fields (maxpos, minpos) =
    fun (lop,mop,rop,bop) ->fun x ->
      Visitor_c.vk_enum_fields_splitted_s
	(mk_bigf (maxpos, minpos) (lop,mop,rop,bop))
        x

  let distribute_mck_struct_fields (maxpos, minpos) =
    fun (lop,mop,rop,bop) ->fun x ->
      Visitor_c.vk_struct_fields_s (mk_bigf (maxpos, minpos) (lop,mop,rop,bop))
        x

  let distribute_mck_cst (maxpos, minpos) =
    fun (lop,mop,rop,bop) ->fun x ->
      Visitor_c.vk_cst_s (mk_bigf (maxpos, minpos) (lop,mop,rop,bop))
        x

  let distribute_mck_define_params (maxpos, minpos) = fun (lop,mop,rop,bop) ->
   fun x ->
    Visitor_c.vk_define_params_splitted_s
      (mk_bigf (maxpos, minpos) (lop,mop,rop,bop))
      x

  let distribute_mck_pragmainfo (maxpos, minpos) = fun (lop,mop,rop,bop) ->
   fun x ->
    Visitor_c.vk_pragmainfo_s (mk_bigf (maxpos, minpos) (lop,mop,rop,bop)) x

  let distribute_mck_ident_list (maxpos, minpos) = fun (lop,mop,rop,bop) ->
   fun x ->
    Visitor_c.vk_ident_list_splitted_s
       (mk_bigf (maxpos, minpos) (lop,mop,rop,bop)) x

  let distribute_mck_exec_code_list (maxpos, minpos) =
    fun (lop,mop,rop,bop) -> fun x ->
      Visitor_c.vk_exec_code_list_splitted_s
	(mk_bigf (maxpos, minpos) (lop,mop,rop,bop))
	x

  let distribute_mck_attrs (maxpos, minpos) =
    fun (lop,mop,rop,bop) -> fun x ->
      Visitor_c.vk_attrs_splitted_s
	(mk_bigf (maxpos, minpos) (lop,mop,rop,bop))
	x

   let get_pos mck =
     match mck with
     | Ast_cocci.PLUS _ -> raise (Impossible 54)
     | Ast_cocci.CONTEXT (Ast_cocci.FixPos (i1,i2),_)
     | Ast_cocci.MINUS   (Ast_cocci.FixPos (i1,i2),_,_,_) ->
         Ast_cocci.FixPos (i1,i2)
     | Ast_cocci.CONTEXT (Ast_cocci.DontCarePos,_)
     | Ast_cocci.MINUS   (Ast_cocci.DontCarePos,_,_,_) ->
         Ast_cocci.DontCarePos
     | _ -> failwith "weird: dont have position info for the mcodekind 2"

  let distrf (ii_of_x_f, distribute_mck_x_f) =
    fun ia x -> fun tin ->
    let mck = Ast_cocci.get_mcodekind ia in
    let (max, min) = Lib_parsing_c.max_min_by_pos (ii_of_x_f x)
    in
    if
      (* bug: check_pos mck max && check_pos mck min
       *
       * if do that then if have - f(...); and in C f(1,2); then we
       * would get a "already tagged" because the '...' would sucess in
       * transformaing both '1' and '1,2'. So being in the range is not
       * enough. We must be equal exactly to the range!
       *)
      (match get_pos mck with
      | Ast_cocci.DontCarePos -> true
      | Ast_cocci.FixPos (i1, i2) ->
          i1 = min && i2 = max
      | _ -> raise (Impossible 55)
      )

    then
      return (
        ia,
        distribute_mck mck (distribute_mck_x_f (max,min))  x tin
      ) tin
    else fail tin


  let distrf_e    = distrf (Lib_parsing_c.ii_of_expr,  distribute_mck_expr)
  let distrf_assignOp = distrf (Lib_parsing_c.ii_of_assignOp, distribute_mck_assignOp)
  let distrf_binaryOp = distrf (Lib_parsing_c.ii_of_binaryOp, distribute_mck_binaryOp)
  let distrf_args = distrf (Lib_parsing_c.ii_of_args,  distribute_mck_args)
  let distrf_type = distrf (Lib_parsing_c.ii_of_type,  distribute_mck_type)
  let distrf_param  = distrf (Lib_parsing_c.ii_of_param, distribute_mck_param)
  let distrf_params = distrf (Lib_parsing_c.ii_of_params,distribute_mck_params)
  let distrf_ini = distrf (Lib_parsing_c.ii_of_ini,distribute_mck_ini)
  let distrf_inis = distrf (Lib_parsing_c.ii_of_inis,distribute_mck_inis)
  let distrf_decl = distrf (Lib_parsing_c.ii_of_decl,distribute_mck_decl)
  let distrf_field = distrf (Lib_parsing_c.ii_of_field,distribute_mck_field)
  let distrf_node = distrf (Lib_parsing_c.ii_of_node,distribute_mck_node)
  let distrf_fragments =
    distrf (Lib_parsing_c.ii_of_fragments,distribute_mck_fragments)
  let distrf_format = distrf (Lib_parsing_c.ii_of_format,distribute_mck_format)
  let distrf_enum_fields =
    distrf (Lib_parsing_c.ii_of_enum_fields, distribute_mck_enum_fields)
  let distrf_struct_fields =
    distrf (Lib_parsing_c.ii_of_struct_fields, distribute_mck_struct_fields)
  let distrf_cst =
    distrf (Lib_parsing_c.ii_of_cst, distribute_mck_cst)
  let distrf_define_params =
    distrf (Lib_parsing_c.ii_of_define_params,distribute_mck_define_params)
  let distrf_pragmainfo =
    distrf (Lib_parsing_c.ii_of_pragmainfo,distribute_mck_pragmainfo)
  let distrf_ident_list =
    distrf (Lib_parsing_c.ii_of_ident_list,distribute_mck_ident_list)
  let distrf_exec_code_list =
    distrf (Lib_parsing_c.ii_of_exec_code_list,distribute_mck_exec_code_list)
  let distrf_attrs =
    distrf (Lib_parsing_c.ii_of_attrs,distribute_mck_attrs)


  (* ------------------------------------------------------------------------*)
  (* Environment *)
  (* ------------------------------------------------------------------------*)
  let meta_name_to_str (s1, s2) = s1 ^ "." ^ s2

  let envf keep inherited = fun (s, value, _) f tin ->
    let s = Ast_cocci.unwrap_mcode s in
    let v =
      if keep = Type_cocci.Saved
      then (
        try Some (List.assoc s tin.binding)
        with Not_found ->
          pr2 (Printf.sprintf
		"Don't find value for metavariable %s in the environment"
                (meta_name_to_str s));
          None)
      else
        (* not raise Impossible! *)
        Some (value)
    in
    match v with
    | None -> fail tin
    | Some (value') ->

        (* Ex: in cocci_vs_c someone wants to add a binding. Here in
         * transformation3 the value for this var may be already in the
         * env, because for instance its value were fixed in a previous
         * SmPL rule. So here we want to check that this is the same value.
         * If forget to do the check, what can happen ? Because of Exp
         * and other disjunctive feature of cocci_vs_c (>||>), we
         * may accept a match at a wrong position. Maybe later this
         * will be detected via the pos system on tokens, but maybe
         * not. So safer to keep the check.
         *)

        (*f () tin*)
	let equal =
	  if inherited
	  then Cocci_vs_c.equal_inh_metavarval
	  else Cocci_vs_c.equal_metavarval in
        if equal value value'
        then f () tin
        else fail tin


  let check_constraints matcher c id = fun f tin -> f () tin
  let check_constraints_ne matcher constraints exp = fun f tin -> f () tin

  (* ------------------------------------------------------------------------*)
  (* Environment, allbounds *)
  (* ------------------------------------------------------------------------*)
  let (all_bound : Ast_cocci.meta_name list -> tin -> bool) = fun l tin ->
    true (* in transform we don't care ? *)

end

(*****************************************************************************)
(* Entry point  *)
(*****************************************************************************)
module TRANS  = Cocci_vs_c.COCCI_VS_C (XTRANS)


let transform_re_node a b tin =
  match TRANS.rule_elem_node a b tin with
  | None -> raise (Impossible 56)
  | Some (_sp, b') -> b'

let (transform2: string (* rule name *) -> string list (* dropped_isos *) ->
  Lib_engine.metavars_binding (* inherited bindings *) ->
  Lib_engine.numbered_transformation_info -> F.cflow -> F.cflow) =
 fun rule_name dropped_isos binding0 xs cflow ->
   let extra = {
     optional_storage_iso   = not(List.mem "optional_storage" dropped_isos);
     optional_qualifier_iso = not(List.mem "optional_qualifier" dropped_isos);
     value_format_iso = not(List.mem "value_format" dropped_isos);
     optional_declarer_semicolon_iso =
       not(List.mem "optional_declarer_semicolon"   dropped_isos);
     optional_attributes_iso =
       not(List.mem "optional_attributes" dropped_isos);
     current_rule_name = rule_name;
     index = [];
   } in

  (* find the node, transform, update the node,  and iter for all elements *)

   xs +> List.fold_left (fun acc (index, (nodei, binding, rule_elem)) ->
      (* subtil: not cflow#nodes but acc#nodes *)
      let node  = F.KeyMap.find nodei acc#nodes in

      if !Flag.show_transinfo
      then pr2 (Printf.sprintf "transform one node: %d" nodei);

      let tin = {
        XTRANS.extra = {extra with index = index};
        XTRANS.binding = binding0@binding;
        XTRANS.binding0 = []; (* not used - everything constant for trans *)
      } in

      let node' = transform_re_node rule_elem node tin in

      (* assert that have done something. But with metaruleElem sometimes
         dont modify fake nodes. So special case before on Fake nodes. *)
      (match F.unwrap node with
      | F.Enter | F.Exit | F.ErrorExit
      | F.EndStatement _ | F.CaseNode _
      | F.Fake
      | F.TrueNode _ | F.FalseNode | F.AfterNode _ | F.FallThroughNode
          -> ()
      | _ -> () (* assert (not (node = node')); *)
      );

      (* useless, we don't go back from flow to ast now *)
      (* let node' = lastfix_comma_struct node' in *)

      acc#replace_node (nodei, node');
      acc
   ) cflow



let transform a b c d e =
  Common.profile_code "Transformation3.transform"
    (fun () -> transform2 a b c d e)
