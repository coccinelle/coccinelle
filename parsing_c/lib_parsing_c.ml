(* Yoann Padioleau
 *
 * Copyright (C) 2010, University of Copenhagen DIKU and INRIA.
 * Copyright (C) 2007, 2008, 2009 Ecole des Mines de Nantes
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

(*****************************************************************************)
(* Wrappers *)
(*****************************************************************************)
let pr2, pr2_once = Common.mk_pr2_wrappers Flag_parsing_c.verbose_parsing

(*****************************************************************************)
(* Abstract line *)
(*****************************************************************************)

(* todo?: al_expr doit enlever les infos de type ? et doit remettre en
 *  emptyAnnot ?

No!  Keeping the type information is important to ensuring that variables
of different type and declared in different places do not seem to match
each other.  On the other hand, we don't want to keep around the
information about whether the expression is a test expression, because a
term that is a test expression should match one that is not.  The test
information is only useful for matching to the CTL.

 *)

(* drop all info information *)

let strip_info_visitor _ =
  let drop_test ty =
    let (ty,_) = !ty in
    ref (ty,Ast_c.NotTest) in

  { Visitor_c.default_visitor_c_s with
    Visitor_c.kinfo_s =
    (* traversal should be deterministic... *)
    (let ctr = ref 0 in
    (function (k,_) ->
    function i -> ctr := !ctr + 1; Ast_c.al_info_cpp !ctr i));

    Visitor_c.kexpr_s = (fun (k,_) e ->
      let (e', ty), ii' = k e in
      (e', drop_test ty), ii' (* keep type - jll *)
    );

(*
    Visitor_c.ktype_s = (fun (k,_) ft ->
      let ft' = k ft in
      match Ast_c.unwrap_typeC ft' with
      | Ast_c.TypeName (s,_typ) ->
          Ast_c.TypeName (s, Ast_c.noTypedefDef()) +> Ast_c.rewrap_typeC ft'
      | _ -> ft'

    );
*)

  }

let al_expr      x = Visitor_c.vk_expr_s      (strip_info_visitor()) x
let al_declaration x = Visitor_c.vk_decl_s    (strip_info_visitor()) x
let al_field     x = Visitor_c.vk_struct_field_s (strip_info_visitor()) x
let al_statement x = Visitor_c.vk_statement_s (strip_info_visitor()) x
let al_statement_seq_list x =
  Visitor_c.vk_statement_sequencable_list_s (strip_info_visitor()) x
let al_type      x = Visitor_c.vk_type_s      (strip_info_visitor()) x
let al_init      x = Visitor_c.vk_ini_s       (strip_info_visitor()) x
let al_inits     x = Visitor_c.vk_inis_s      (strip_info_visitor()) x
let al_param     x = Visitor_c.vk_param_s     (strip_info_visitor()) x
let al_params    x = Visitor_c.vk_params_s    (strip_info_visitor()) x
let al_define_params x =
  Visitor_c.vk_define_params_s (strip_info_visitor()) x
let al_arguments x = Visitor_c.vk_arguments_s (strip_info_visitor()) x
let al_fields    x = Visitor_c.vk_struct_fields_s (strip_info_visitor()) x
let al_name      x = Visitor_c.vk_name_s      (strip_info_visitor()) x
let al_string_format x = Visitor_c.vk_string_format_s (strip_info_visitor()) x
let al_string_fragments x =
  Visitor_c.vk_string_fragments_s (strip_info_visitor()) x
let al_attribute x = Visitor_c.vk_attribute_s (strip_info_visitor()) x
let al_attr_arg  x = Visitor_c.vk_attr_arg_s  (strip_info_visitor()) x

let al_node      x = Visitor_c.vk_node_s      (strip_info_visitor()) x

let al_program  x = List.map (Visitor_c.vk_toplevel_s (strip_info_visitor())) x
let al_ii    x = Visitor_c.vk_ii_s (strip_info_visitor()) x




let strip_inh_info_visitor _ =  (* for inherited metavariables *)
  let drop_test_lv ty bigf =
    let (ty,_) = !ty in
    let ty =
      match ty with
	None -> None
      |	Some (ty,_) ->
	  let ty = Visitor_c.vk_type_s bigf ty in
	  Some (ty,Ast_c.NotLocalVar) in
    ref ((ty,Ast_c.NotTest) : Ast_c.exp_info) in

  { Visitor_c.default_visitor_c_s with
    Visitor_c.kinfo_s =
    (* traversal should be deterministic... *)
    (let ctr = ref 0 in
    (function (k,_) ->
    function i -> ctr := !ctr + 1; Ast_c.al_info_cpp !ctr i));

    Visitor_c.kexpr_s = (fun (k,bigf) e ->
      let (e', ty), ii' = k e in
      (e', drop_test_lv ty bigf), ii' (* keep type, but process it - jll *)
    );

(*
    Visitor_c.ktype_s = (fun (k,_) ft ->
      let ft' = k ft in
      match Ast_c.unwrap_typeC ft' with
      | Ast_c.TypeName (s,_typ) ->
          Ast_c.TypeName (s, Ast_c.noTypedefDef()) +> Ast_c.rewrap_typeC ft'
      | _ -> ft'

    );
*)

  }

let al_inh_expr      x = Visitor_c.vk_expr_s      (strip_inh_info_visitor()) x
let al_inh_declaration x = Visitor_c.vk_decl_s    (strip_inh_info_visitor()) x
let al_inh_field    x = Visitor_c.vk_struct_field_s (strip_inh_info_visitor()) x
let al_inh_field_list x =
  Visitor_c.vk_struct_fields_s (strip_inh_info_visitor()) x
let al_inh_statement x = Visitor_c.vk_statement_s (strip_inh_info_visitor()) x
let al_inh_statement_seq_list x =
  Visitor_c.vk_statement_sequencable_list_s (strip_inh_info_visitor()) x
let al_inh_type      x = Visitor_c.vk_type_s      (strip_inh_info_visitor()) x
let al_inh_init      x = Visitor_c.vk_ini_s       (strip_inh_info_visitor()) x
let al_inh_inits     x = Visitor_c.vk_inis_s      (strip_inh_info_visitor()) x
let al_inh_arguments x = Visitor_c.vk_arguments_s (strip_inh_info_visitor()) x
let al_inh_string_format x =
  Visitor_c.vk_string_format_s (strip_inh_info_visitor()) x
let al_inh_string_fragments x =
  Visitor_c.vk_string_fragments_s (strip_inh_info_visitor()) x
let al_inh_attribute x = Visitor_c.vk_attribute_s (strip_inh_info_visitor()) x
let al_inh_attr_arg  x = Visitor_c.vk_attr_arg_s  (strip_inh_info_visitor()) x



let semi_strip_info_visitor = (* keep position information *)
  let drop_test ty =
    let (ty,_) = !ty in
    ref (ty,Ast_c.NotTest) in

  { Visitor_c.default_visitor_c_s with
    Visitor_c.kinfo_s = (fun (k,_) i -> Ast_c.semi_al_info_cpp i);

    Visitor_c.kexpr_s = (fun (k,_) e ->
      let (e', ty),ii' = k e in
      (e', drop_test ty), ii' (* keep type - jll *)
    );

  }

let semi_al_expr      = Visitor_c.vk_expr_s      semi_strip_info_visitor
let semi_al_declaration = Visitor_c.vk_decl_s    semi_strip_info_visitor
let semi_al_field = Visitor_c.vk_struct_field_s  semi_strip_info_visitor
let semi_al_fields = Visitor_c.vk_struct_fields_s semi_strip_info_visitor
let semi_al_statement = Visitor_c.vk_statement_s semi_strip_info_visitor
let semi_al_statement_seq_list =
  Visitor_c.vk_statement_sequencable_list_s semi_strip_info_visitor
let semi_al_type      = Visitor_c.vk_type_s      semi_strip_info_visitor
let semi_al_init      = Visitor_c.vk_ini_s       semi_strip_info_visitor
let semi_al_inits     = Visitor_c.vk_inis_s      semi_strip_info_visitor
let semi_al_param     = Visitor_c.vk_param_s     semi_strip_info_visitor
let semi_al_params    = Visitor_c.vk_params_s    semi_strip_info_visitor
let semi_al_define_params =
  Visitor_c.vk_define_params_s semi_strip_info_visitor
let semi_al_arguments = Visitor_c.vk_arguments_s semi_strip_info_visitor
let semi_al_string_format =
  Visitor_c.vk_string_format_s semi_strip_info_visitor
let semi_al_string_fragments =
  Visitor_c.vk_string_fragments_s semi_strip_info_visitor
let semi_al_attribute = Visitor_c.vk_attribute_s semi_strip_info_visitor
let semi_al_attr_arg  = Visitor_c.vk_attr_arg_s semi_strip_info_visitor

let semi_al_program =
  List.map (Visitor_c.vk_toplevel_s semi_strip_info_visitor)




(* really strip, do not keep position nor anything specificities, true
 * abstracted form. This is used outside coccinelle in Yacfe and aComment *)
let real_strip_info_visitor _ =
  { Visitor_c.default_visitor_c_s with
    Visitor_c.kinfo_s = (fun (k,_) i ->
      Ast_c.real_al_info_cpp false i
    );

    Visitor_c.kexpr_s = (fun (k,_) e ->
      let (e', ty),ii' = k e in
      (e', Ast_c.noType()), ii'
    );

(*
    Visitor_c.ktype_s = (fun (k,_) ft ->
      let ft' = k ft in
      match Ast_c.unwrap_typeC ft' with
      | Ast_c.TypeName (s,_typ) ->
          Ast_c.TypeName (s, Ast_c.noTypedefDef()) +> Ast_c.rewrap_typeC ft'
      | _ -> ft'

    );
*)

  }

let real_al_expr      x = Visitor_c.vk_expr_s   (real_strip_info_visitor()) x
let real_al_arguments x = Visitor_c.vk_arguments_s (real_strip_info_visitor()) x
let real_al_node      x = Visitor_c.vk_node_s   (real_strip_info_visitor()) x
let real_al_type      x = Visitor_c.vk_type_s   (real_strip_info_visitor()) x
let real_al_binop     x = Visitor_c.vk_binaryOp_s (real_strip_info_visitor()) x
let real_al_assignop  x = Visitor_c.vk_assignOp_s (real_strip_info_visitor()) x
let real_al_decl      x = Visitor_c.vk_decl_s   (real_strip_info_visitor()) x
let real_al_init      x = Visitor_c.vk_ini_s    (real_strip_info_visitor()) x
let real_al_inits     x = Visitor_c.vk_inis_s   (real_strip_info_visitor()) x
let real_al_statement x =
  Visitor_c.vk_statement_s (real_strip_info_visitor()) x
let real_al_statement_seq_list x =
  Visitor_c.vk_statement_sequencable_list_s (real_strip_info_visitor()) x
let real_al_def       x = Visitor_c.vk_toplevel_s (real_strip_info_visitor()) x




let real_strip_info_visitor_with_comments _ =
  { Visitor_c.default_visitor_c_s with
    Visitor_c.kinfo_s = (fun (k,_) i ->
      Ast_c.real_al_info_cpp true i
    );

    Visitor_c.kexpr_s = (fun (k,_) e ->
      let (e', ty),ii' = k e in
      (e', Ast_c.noType()), ii'
    );

(*
    Visitor_c.ktype_s = (fun (k,_) ft ->
      let ft' = k ft in
      match Ast_c.unwrap_typeC ft' with
      | Ast_c.TypeName (s,_typ) ->
          Ast_c.TypeName (s, Ast_c.noTypedefDef()) +> Ast_c.rewrap_typeC ft'
      | _ -> ft'

    );
*)

  }

let real_al_decl_with_comments x =
  Visitor_c.vk_decl_s   (real_strip_info_visitor_with_comments()) x
let real_al_statement_with_comments x =
  Visitor_c.vk_statement_s (real_strip_info_visitor_with_comments()) x





(*****************************************************************************)
(* Extract infos *)
(*****************************************************************************)

let extract_info_visitor recursor x =
  let globals = ref [] in
  let visitor =
    {
      Visitor_c.default_visitor_c with
        Visitor_c.kinfo = (fun (k, _) i -> Common.push2 i globals)
    } in
  begin
    recursor visitor x;
    !globals
  end

let ii_of_def = extract_info_visitor Visitor_c.vk_def
let ii_of_decl = extract_info_visitor Visitor_c.vk_decl
let ii_of_field = extract_info_visitor Visitor_c.vk_struct_field
let ii_of_node = extract_info_visitor Visitor_c.vk_node
let ii_of_expr = extract_info_visitor Visitor_c.vk_expr
let ii_of_assignOp = extract_info_visitor Visitor_c.vk_assignOp
let ii_of_binaryOp = extract_info_visitor Visitor_c.vk_binaryOp
let ii_of_stmt = extract_info_visitor Visitor_c.vk_statement
let ii_of_stmtseq = extract_info_visitor Visitor_c.vk_statement_sequencable
let ii_of_stmtseqlist =
  extract_info_visitor Visitor_c.vk_statement_sequencable_list
let ii_of_args = extract_info_visitor Visitor_c.vk_args_splitted
let ii_of_type = extract_info_visitor Visitor_c.vk_type
let ii_of_ini  = extract_info_visitor Visitor_c.vk_ini
let ii_of_inis  = extract_info_visitor Visitor_c.vk_inis_splitted
let ii_of_param = extract_info_visitor Visitor_c.vk_param
let ii_of_params = extract_info_visitor Visitor_c.vk_params_splitted
let ii_of_enum_fields = extract_info_visitor Visitor_c.vk_enum_fields_splitted
let ii_of_struct_fields = extract_info_visitor Visitor_c.vk_struct_fields
(*let ii_of_struct_field = extract_info_visitor Visitor_c.vk_struct_field*)
let ii_of_struct_fieldkinds =
  extract_info_visitor Visitor_c.vk_struct_fieldkinds
let ii_of_cst = extract_info_visitor Visitor_c.vk_cst
let ii_of_fragments =
  extract_info_visitor Visitor_c.vk_string_fragments_splitted
let ii_of_format = extract_info_visitor Visitor_c.vk_string_format
let ii_of_define_params =
  extract_info_visitor Visitor_c.vk_define_params_splitted
let ii_of_ident_list = extract_info_visitor Visitor_c.vk_ident_list_splitted
let ii_of_exec_code_list =
  extract_info_visitor Visitor_c.vk_exec_code_list_splitted
let ii_of_attr = extract_info_visitor Visitor_c.vk_attribute
let ii_of_attr_arg = extract_info_visitor Visitor_c.vk_attr_arg
let ii_of_attrs = extract_info_visitor Visitor_c.vk_attrs_splitted
let ii_of_toplevel = extract_info_visitor Visitor_c.vk_toplevel

(*****************************************************************************)
(* Max min, range *)
(*****************************************************************************)
let max_min_ii_by_pos xs =
  match xs with
  | [] -> failwith "empty list, max_min_ii_by_pos"
  | [x] -> (x, x)
  | x::xs ->
      let pos_leq p1 p2 = (Ast_c.compare_pos p1 p2) = (-1) in
      xs +> List.fold_left (fun (maxii,minii) e ->
        let maxii' = if pos_leq maxii e then e else maxii in
        let minii' = if pos_leq e minii then e else minii in
        maxii', minii'
      ) (x,x)

(* avoid memory costs of prefiltering the list *)
let max_min_ii_by_pos_filtered f xs =
  let xs = Common.drop_until f xs in
  match xs with
  | [] -> failwith "empty list, max_min_ii_by_pos"
  | [x] -> (x, x)
  | x::xs ->
      let pos_leq p1 p2 = (Ast_c.compare_pos p1 p2) = (-1) in
      xs +> List.fold_left (fun ((maxii,minii) as acc) e ->
	if f e
	then
          let maxii' = if pos_leq maxii e then e else maxii in
          let minii' = if pos_leq e minii then e else minii in
          maxii', minii'
	else acc
      ) (x,x)

let info_to_fixpos ii =
  match Ast_c.pinfo_of_info ii with
    Ast_c.OriginTok pi -> Ast_cocci.Real pi.Common.charpos
  | Ast_c.ExpandedTok (_,(pi,offset)) ->
      Ast_cocci.Virt (pi.Common.charpos,offset)
  | Ast_c.FakeTok (_,(pi,offset)) ->
      Ast_cocci.Virt (pi.Common.charpos,offset)
  | Ast_c.AbstractLineTok pi ->
      failwith ("unexpected abstract: "^(Dumper.dump pi))

let max_min_by_pos xs =
  let (i1, i2) = max_min_ii_by_pos xs in
  (info_to_fixpos i1, info_to_fixpos i2)

let lin_col_by_pos xs =
  (* put min before max; no idea why they are backwards above *)
  let (i2, i1) =
    max_min_ii_by_pos_filtered (function ii -> not (Ast_c.is_fake ii)) xs in
  let posf x = Ast_c.col_of_info x in
  let mposf x = Ast_c.col_of_info x + String.length (Ast_c.str_of_info x) in
  (Ast_c.file_of_info i1,!Flag.current_element,
   (Ast_c.line_of_info i1, posf i1), (Ast_c.line_of_info i2, mposf i2))





let min_pinfo_of_node node =
  let ii = ii_of_node node in
  let (maxii, minii) = max_min_ii_by_pos ii in
  Ast_c.parse_info_of_info minii


let (range_of_origin_ii: Ast_c.info list -> (int * int) option) =
 fun ii ->
  try
    let (max, min) = max_min_ii_by_pos_filtered Ast_c.is_origintok ii in
    assert(Ast_c.is_origintok max);
    assert(Ast_c.is_origintok min);
    let strmax = Ast_c.str_of_info max in
    Some
      (Ast_c.pos_of_info min, Ast_c.pos_of_info max + String.length strmax)
  with _ ->
    None


(*****************************************************************************)
(* Ast getters *)
(*****************************************************************************)

let names_of_parameters_in_def def =
  match def.Ast_c.f_old_c_style with
  | Some _ ->
      pr2_once "names_of_parameters_in_def: f_old_c_style not handled";
      []
  | None ->
      let ftyp = def.Ast_c.f_type in
      let (ret, (params, bwrap)) = ftyp in
      params +> Common.map_filter (fun (param,ii) ->
        Ast_c.name_of_parameter param
      )

let names_of_parameters_in_macro xs =
  xs +> List.map (fun (xx, ii) ->
    let (s, ii2) = xx in
    s
  )



(* only used in ast_to_flow, so move it ? *)
let rec stmt_elems_of_sequencable xs =
  xs +> Common.map (fun x ->
    match x with
    | Ast_c.StmtElem e -> [e]
    | Ast_c.CppDirectiveStmt _
    | Ast_c.IfdefStmt _
        ->
        pr2_once ("stmt_elems_of_sequencable: filter a directive");
        []
    | Ast_c.IfdefStmt2 (_ifdef, xxs) ->
        pr2 ("stmt_elems_of_sequencable: IfdefStm2 TODO?");
        xxs +> List.map (fun xs ->
          let xs' = stmt_elems_of_sequencable xs in
          xs'
        ) +> List.flatten
  ) +> List.flatten
