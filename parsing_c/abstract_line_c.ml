open Common open Commonop

(* todo?: al_expr doit enlever les infos de type ? et doit remettre en 
   dumb_annot ? *)

let strip_info_visitor = 
  { Visitor_c.default_visitor_c_s with
    Visitor_c.kinfo_s = (fun (k,_) i -> Ast_c.al_info i)
  }

let al_expr      = Visitor_c.visitor_expr_k_s      strip_info_visitor 
let al_statement = Visitor_c.visitor_statement_k_s strip_info_visitor
let al_type      = Visitor_c.visitor_type_k_s      strip_info_visitor 

let al_program xs = xs +> List.map (fun p -> 
  Visitor_c.visitor_program_k_s  strip_info_visitor p
  )

