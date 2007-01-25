
open Commonop


(*****************************************************************************)
(* Abstract line *)
(*****************************************************************************)

(* todo?: al_expr doit enlever les infos de type ? et doit remettre en
 *  emptyAnnot ? 
 *)

let strip_info_visitor = 
  { Visitor_c.default_visitor_c_s with
    Visitor_c.kinfo_s = (fun (k,_) i -> Ast_c.al_info i)
  }

let al_expr      = Visitor_c.vk_expr_s      strip_info_visitor 
let al_statement = Visitor_c.vk_statement_s strip_info_visitor
let al_type      = Visitor_c.vk_type_s      strip_info_visitor 

let al_program  = List.map (Visitor_c.vk_program_s  strip_info_visitor)

