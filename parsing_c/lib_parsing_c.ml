(* Copyright (C) 2007, 2008 Yoann Padioleau
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
(* Abstract line *)
(*****************************************************************************)

(* todo?: al_expr doit enlever les infos de type ? et doit remettre en
 *  emptyAnnot ? 
 *)

(* drop all info information *)

let strip_info_visitor _ = 
  { Visitor_c.default_visitor_c_s with
    Visitor_c.kinfo_s =
    (* traversal should be deterministic... *)
    (let ctr = ref 0 in 
    (function (k,_) ->
    function i -> ctr := !ctr + 1; Ast_c.al_info !ctr i));

    Visitor_c.kexpr_s = (fun (k,_) e -> 
      let (e', ty),ii' = k e in
      (e', Ast_c.noType()(*ref !ty*)), ii' (* keep type - jll *)
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
let al_statement x = Visitor_c.vk_statement_s (strip_info_visitor()) x
let al_type      x = Visitor_c.vk_type_s      (strip_info_visitor()) x
let al_param     x = Visitor_c.vk_param_s     (strip_info_visitor()) x
let al_params    x = Visitor_c.vk_params_s    (strip_info_visitor()) x
let al_arguments x = Visitor_c.vk_arguments_s (strip_info_visitor()) x

let al_program  x = List.map (Visitor_c.vk_toplevel_s (strip_info_visitor())) x

let semi_strip_info_visitor = (* keep position information *)
  { Visitor_c.default_visitor_c_s with
    Visitor_c.kinfo_s = (fun (k,_) i -> Ast_c.semi_al_info i);

    Visitor_c.kexpr_s = (fun (k,_) e -> 
      let (e', ty),ii' = k e in
      (e', Ast_c.noType()(*ref !ty*)), ii' (* keep type - jll *)
    );
    
  }

let semi_al_expr      = Visitor_c.vk_expr_s      semi_strip_info_visitor 
let semi_al_statement = Visitor_c.vk_statement_s semi_strip_info_visitor
let semi_al_type      = Visitor_c.vk_type_s      semi_strip_info_visitor
let semi_al_param     = Visitor_c.vk_param_s     semi_strip_info_visitor
let semi_al_params    = Visitor_c.vk_params_s    semi_strip_info_visitor
let semi_al_arguments = Visitor_c.vk_arguments_s semi_strip_info_visitor

let semi_al_program = List.map (Visitor_c.vk_toplevel_s semi_strip_info_visitor)

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

let ii_of_decl = extract_info_visitor Visitor_c.vk_decl
let ii_of_node = extract_info_visitor Visitor_c.vk_node
let ii_of_expr = extract_info_visitor Visitor_c.vk_expr
let ii_of_stmt = extract_info_visitor Visitor_c.vk_statement
let ii_of_args = extract_info_visitor Visitor_c.vk_args_splitted
let ii_of_type = extract_info_visitor Visitor_c.vk_type
let ii_of_ini  = extract_info_visitor Visitor_c.vk_ini
let ii_of_param = extract_info_visitor Visitor_c.vk_param
let ii_of_params = extract_info_visitor Visitor_c.vk_params_splitted
let ii_of_struct_fields = extract_info_visitor Visitor_c.vk_struct_fields
(*let ii_of_struct_field = extract_info_visitor Visitor_c.vk_struct_field*)
let ii_of_struct_fieldkinds = extract_info_visitor Visitor_c.vk_struct_fieldkinds
let ii_of_cst = extract_info_visitor Visitor_c.vk_cst
let ii_of_define_params = 
  extract_info_visitor Visitor_c.vk_define_params_splitted
let ii_of_toplevel = extract_info_visitor Visitor_c.vk_toplevel

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

let info_to_fixpos ii =
  match Ast_c.pinfo_of_info ii with
    Ast_c.OriginTok pi -> Ast_cocci.Real pi.Common.charpos
  | Ast_c.ExpandedTok (_,(pi,offset)) ->
      Ast_cocci.Virt (pi.Common.charpos,offset)
  | Ast_c.FakeTok (_,(pi,offset)) ->
      Ast_cocci.Virt (pi.Common.charpos,offset)
  | Ast_c.AbstractLineTok pi -> failwith "unexpected abstract"
  
let max_min_by_pos xs = 
  let (i1, i2) = max_min_ii_by_pos xs in
  (info_to_fixpos i1, info_to_fixpos i2)

let lin_col_by_pos xs = 
  (* put min before max; no idea why they are backwards above *)
  let (i2, i1) = max_min_ii_by_pos xs in
  let posf x = Ast_c.col_of_info x in
  let mposf x = Ast_c.col_of_info x + String.length (Ast_c.str_of_info x) in
  (Ast_c.file_of_info i1,
   (Ast_c.line_of_info i1, posf i1), (Ast_c.line_of_info i2, mposf i2))


