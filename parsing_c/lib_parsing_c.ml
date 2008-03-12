
open Commonop
open Common


(*****************************************************************************)
(* Abstract line *)
(*****************************************************************************)

(* todo?: al_expr doit enlever les infos de type ? et doit remettre en
 *  emptyAnnot ? 
 *)

(* drop all info information *)

let strip_info_visitor = 
  { Visitor_c.default_visitor_c_s with
    Visitor_c.kinfo_s = (fun (k,_) i -> Ast_c.al_info i);

    Visitor_c.kexpr_s = (fun (k,_) e -> 
      let (e', _),ii' = k e in
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

let al_expr      = Visitor_c.vk_expr_s      strip_info_visitor 
let al_statement = Visitor_c.vk_statement_s strip_info_visitor
let al_type      = Visitor_c.vk_type_s      strip_info_visitor
let al_param     = Visitor_c.vk_param_s     strip_info_visitor
let al_params    = Visitor_c.vk_params_s    strip_info_visitor
let al_arguments = Visitor_c.vk_arguments_s strip_info_visitor

let al_program  = List.map (Visitor_c.vk_program_s  strip_info_visitor)

let semi_strip_info_visitor = (* keep position information *)
  { Visitor_c.default_visitor_c_s with
    Visitor_c.kinfo_s = (fun (k,_) i -> Ast_c.semi_al_info i);

    Visitor_c.kexpr_s = (fun (k,_) e -> 
      let (e', _),ii' = k e in
      (e', Ast_c.noType()), ii'
    );
    
  }

let semi_al_expr      = Visitor_c.vk_expr_s      semi_strip_info_visitor 
let semi_al_statement = Visitor_c.vk_statement_s semi_strip_info_visitor
let semi_al_type      = Visitor_c.vk_type_s      semi_strip_info_visitor
let semi_al_param     = Visitor_c.vk_param_s     semi_strip_info_visitor
let semi_al_params    = Visitor_c.vk_params_s    semi_strip_info_visitor
let semi_al_arguments = Visitor_c.vk_arguments_s semi_strip_info_visitor

let semi_al_program = List.map (Visitor_c.vk_program_s semi_strip_info_visitor)

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
let ii_of_args = extract_info_visitor Visitor_c.vk_args_splitted
let ii_of_type = extract_info_visitor Visitor_c.vk_type
let ii_of_ini  = extract_info_visitor Visitor_c.vk_ini
let ii_of_param = extract_info_visitor Visitor_c.vk_param
let ii_of_params = extract_info_visitor Visitor_c.vk_params_splitted
let ii_of_struct_fields = extract_info_visitor Visitor_c.vk_struct_fields
let ii_of_cst = extract_info_visitor Visitor_c.vk_cst
let ii_of_stmt = extract_info_visitor Visitor_c.vk_statement
let ii_of_define_params = 
  extract_info_visitor Visitor_c.vk_define_params_splitted

let max_min_ii_by_pos xs = 

  match xs with
  | [] -> failwith "empty list, max_min_ii_by_pos"
  | [x] when Ast_c.mark_of_info x <> Ast_c.OriginTok -> 
      pr2_once "PB: no max or min, have fake info, should not happen";
      (x, x)
  | x::xs -> 
      xs +> List.fold_left (fun (maxii,minii) e -> 
        let posf x = Ast_c.pos_of_info x in

        if (Ast_c.mark_of_info e <> Ast_c.OriginTok)
        then begin 
          pr2_once "PB: no max or min, have fake info, should not happen";
          (maxii, minii)
        end
        else 
          let maxii' = if posf e >= posf maxii then e else maxii in
          let minii' = if posf e <= posf minii then e else minii in
          maxii', minii'
      ) (x,x)
  
let max_min_by_pos xs = 
  let (i1, i2) = max_min_ii_by_pos xs in
  let posf x = Ast_c.pos_of_info x in
  let mposf x = Ast_c.pos_of_info x in
  (mposf i1, posf i2)

let lin_col_by_pos xs = 
  (* put min before max; no idea why they are backwards above *)
  let (i2, i1) = max_min_ii_by_pos xs in
  let posf x = Ast_c.col_of_info x in
  let mposf x = Ast_c.col_of_info x + String.length (Ast_c.str_of_info x) in
  ((Ast_c.line_of_info i1, mposf i1), (Ast_c.line_of_info i2, posf i2))


