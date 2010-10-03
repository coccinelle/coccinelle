(* Copyright (C) 2006, 2007 Yoann Padioleau
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

module F = Control_flow_c

module Flag = Flag_matcher
(*****************************************************************************)
(* The functor argument  *) 
(*****************************************************************************)

(* info passed recursively in monad in addition to binding *)
type xinfo = { 
  optional_storage_iso : bool;
  optional_qualifier_iso : bool;
  value_format_iso : bool;
  current_rule_name : string; (* used for errors *)
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

  let mode = Cocci_vs_c.TransformMode

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
  let cocciExpExp = fun expf expa expb -> fun tin -> 

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
     | Ast_cocci.PLUS -> raise Impossible
     | Ast_cocci.CONTEXT (Ast_cocci.FixPos (i1,i2),_) 
     | Ast_cocci.MINUS   (Ast_cocci.FixPos (i1,i2),_) -> 
         pos <= i2 && pos >= i1
     | Ast_cocci.CONTEXT (Ast_cocci.DontCarePos,_) 
     | Ast_cocci.MINUS   (Ast_cocci.DontCarePos,_) -> 
         true
     | _ ->
	 match info with
	   Some info ->
	     failwith
	       (Printf.sprintf
		  "wierd: dont have position info for the mcodekind in line %d column %d"
		  info.Ast_cocci.line info.Ast_cocci.column)
	 | None ->
	     failwith "wierd: dont have position info for the mcodekind"


  let tag_with_mck mck ib = fun tin -> 

    let cocciinforef = ib.Ast_c.cocci_tag in
    let (oldmcode, oldenv) = !cocciinforef in

    let mck =
      (* coccionly: 
      if !Flag_parsing_cocci.sgrep_mode
      then Sgrep.process_sgrep ib mck
      else 
      *)
        mck 
    in
    (match mck, Ast_c.pinfo_of_info ib with
    | _,                  Ast_c.AbstractLineTok _ -> raise Impossible
    | Ast_cocci.MINUS(_), Ast_c.ExpandedTok _ -> 
        failwith ("try to delete an expanded token: " ^ (Ast_c.str_of_info ib))
    | _ -> ()
    );

    match (oldmcode,mck) with
    | (Ast_cocci.CONTEXT(_,Ast_cocci.NOTHING),      _)
    | (_,   Ast_cocci.CONTEXT(_,Ast_cocci.NOTHING)) 
      ->
        cocciinforef := (mck, tin.binding);
        ib

    | _ -> 
        if (oldmcode, oldenv) = (mck, tin.binding)
        then begin
          if !Flag.show_misc 
          then pr2 "already tagged but with same mcode, so safe";
          ib
        end
        else 
          (* coccionly: 
          if !Flag.sgrep_mode2
          then ib (* safe *)
          else 
          *)
             begin
            (* coccionly:
	      Format.set_formatter_out_channel stderr;
              Common.pr2 "SP mcode ";
              Pretty_print_cocci.print_mcodekind oldmcode;
              Format.print_newline();
              Common.pr2 "C code mcode ";
              Pretty_print_cocci.print_mcodekind mck;
              Format.print_newline();
              Format.print_flush();
            *)
              failwith
	        (Common.sprintf "%s: already tagged token:\n%s"
		   tin.extra.current_rule_name
	           (Common.error_message (Ast_c.file_of_info ib)
		      (Ast_c.str_of_info ib, Ast_c.opos_of_info ib)))
            end

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
    | Ast_cocci.MINUS (pos,any_xxs) -> 
        distributef (
          (fun ib -> tag_with_mck (Ast_cocci.MINUS (pos,any_xxs)) ib tin),
          (fun ib -> tag_with_mck (Ast_cocci.MINUS (pos,[])) ib tin),
          (fun ib -> tag_with_mck (Ast_cocci.MINUS (pos,[])) ib tin),
          (fun ib -> tag_with_mck (Ast_cocci.MINUS (pos,any_xxs)) ib tin)
        ) expr
    | Ast_cocci.CONTEXT (pos,any_befaft) -> 
        (match any_befaft with
        | Ast_cocci.NOTHING -> expr
            
        | Ast_cocci.BEFORE xxs -> 
            distributef (
              (fun ib -> tag_with_mck 
                (Ast_cocci.CONTEXT (pos,Ast_cocci.BEFORE xxs)) ib tin),
              (fun x -> x), 
              (fun x -> x), 
              (fun ib -> tag_with_mck 
                (Ast_cocci.CONTEXT (pos,Ast_cocci.BEFORE xxs)) ib tin)
            ) expr
        | Ast_cocci.AFTER xxs -> 
            distributef (
              (fun x -> x), 
              (fun x -> x), 
              (fun ib -> tag_with_mck 
                (Ast_cocci.CONTEXT (pos,Ast_cocci.AFTER xxs)) ib tin),
              (fun ib -> tag_with_mck 
                (Ast_cocci.CONTEXT (pos,Ast_cocci.AFTER xxs)) ib tin)
            ) expr

        | Ast_cocci.BEFOREAFTER (xxs, yys) -> 
            distributef (
              (fun ib -> tag_with_mck 
                (Ast_cocci.CONTEXT (pos,Ast_cocci.BEFORE xxs)) ib tin),
              (fun x -> x), 
              (fun ib -> tag_with_mck 
                (Ast_cocci.CONTEXT (pos,Ast_cocci.AFTER yys)) ib tin),
              (fun ib -> tag_with_mck 
                (Ast_cocci.CONTEXT (pos,Ast_cocci.BEFOREAFTER (xxs,yys)))
                ib tin)
            ) expr

        )
    | Ast_cocci.PLUS -> raise Impossible


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

  let distribute_mck_args (maxpos, minpos) = fun (lop,mop,rop,bop) -> fun x ->
    Visitor_c.vk_args_splitted_s (mk_bigf (maxpos, minpos) (lop,mop,rop,bop)) x

  let distribute_mck_type (maxpos, minpos) = fun (lop,mop,rop,bop) -> fun x ->
    Visitor_c.vk_type_s (mk_bigf (maxpos, minpos) (lop,mop,rop,bop)) x

  let distribute_mck_ini (maxpos, minpos) = fun (lop,mop,rop,bop) -> fun x ->
    Visitor_c.vk_ini_s (mk_bigf (maxpos, minpos) (lop,mop,rop,bop)) x

  let distribute_mck_param (maxpos, minpos) = fun (lop,mop,rop,bop) -> fun x ->
    Visitor_c.vk_param_s (mk_bigf (maxpos, minpos) (lop,mop,rop,bop)) x

  let distribute_mck_params (maxpos, minpos) = fun (lop,mop,rop,bop) ->fun x ->
    Visitor_c.vk_params_splitted_s (mk_bigf (maxpos, minpos) (lop,mop,rop,bop))
      x

  let distribute_mck_node (maxpos, minpos) = fun (lop,mop,rop,bop) ->fun x ->
    Visitor_c.vk_node_s (mk_bigf (maxpos, minpos) (lop,mop,rop,bop))
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

   let get_pos mck = 
     match mck with
     | Ast_cocci.PLUS -> raise Impossible
     | Ast_cocci.CONTEXT (Ast_cocci.FixPos (i1,i2),_) 
     | Ast_cocci.MINUS   (Ast_cocci.FixPos (i1,i2),_) -> 
         Ast_cocci.FixPos (i1,i2)
     | Ast_cocci.CONTEXT (Ast_cocci.DontCarePos,_) 
     | Ast_cocci.MINUS   (Ast_cocci.DontCarePos,_) -> 
         Ast_cocci.DontCarePos
     | _ -> failwith "wierd: dont have position info for the mcodekind"      
      
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
      | _ -> raise Impossible
      )

    then 
      return (
        ia, 
        distribute_mck mck (distribute_mck_x_f (max,min))  x tin
      ) tin
    else fail tin


  let distrf_e    = distrf (Lib_parsing_c.ii_of_expr,  distribute_mck_expr)
  let distrf_args = distrf (Lib_parsing_c.ii_of_args,  distribute_mck_args)
  let distrf_type = distrf (Lib_parsing_c.ii_of_type,  distribute_mck_type)
  let distrf_param  = distrf (Lib_parsing_c.ii_of_param, distribute_mck_param)
  let distrf_params = distrf (Lib_parsing_c.ii_of_params,distribute_mck_params)
  let distrf_ini = distrf (Lib_parsing_c.ii_of_ini,distribute_mck_ini)
  let distrf_node = distrf (Lib_parsing_c.ii_of_node,distribute_mck_node)
  let distrf_struct_fields = 
    distrf (Lib_parsing_c.ii_of_struct_fields, distribute_mck_struct_fields)
  let distrf_cst = 
    distrf (Lib_parsing_c.ii_of_cst, distribute_mck_cst)
  let distrf_define_params = 
    distrf (Lib_parsing_c.ii_of_define_params,distribute_mck_define_params)


  (* ------------------------------------------------------------------------*)
  (* Environment *) 
  (* ------------------------------------------------------------------------*)
  let meta_name_to_str (s1, s2) = 
    s1 ^ "." ^ s2

  let envf keep _inherited = fun (s, value, _) f tin -> 
    let s = Ast_cocci.unwrap_mcode s in
    let v = 
      if keep = Type_cocci.Saved
      then (
        try Some (List.assoc s tin.binding)
        with Not_found -> 
          pr2(sprintf
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
        if Cocci_vs_c.equal_metavarval value value' 
        then f () tin
        else fail tin

    
  let check_constraints matcher constraints exp = fun f tin -> f () tin

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
  | None -> raise Impossible
  | Some (_sp, b') -> b'

let (transform2: string (* rule name *) -> string list (* dropped_isos *) ->
  Lib_engine.metavars_binding (* inherited bindings *) ->
  Lib_engine.transformation_info -> F.cflow -> F.cflow) = 
 fun rule_name dropped_isos binding0 xs cflow -> 

   let extra = { 
     optional_storage_iso   = not(List.mem "optional_storage" dropped_isos);
     optional_qualifier_iso = not(List.mem "optional_qualifier" dropped_isos);
     value_format_iso = not(List.mem "value_format" dropped_isos);
     current_rule_name = rule_name;
   } in

  (* find the node, transform, update the node,  and iter for all elements *)

   xs +> List.fold_left (fun acc (nodei, binding, rule_elem) -> 
      (* subtil: not cflow#nodes but acc#nodes *)
      let node  = acc#nodes#assoc nodei in 

      if !Flag.show_misc 
      then pr2 "transform one node";
      
      let tin = {
        XTRANS.extra = extra;
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
      | F.TrueNode | F.FalseNode | F.AfterNode | F.FallThroughNode 
          -> ()
      | _ -> () (* assert (not (node =*= node')); *)
      );

      (* useless, we dont go back from flow to ast now *)
      (* let node' = lastfix_comma_struct node' in *)
      
      acc#replace_node (nodei, node');
      acc
   ) cflow



let transform a b c d e = 
  Common.profile_code "Transformation3.transform" 
    (fun () -> transform2 a b c d e)
