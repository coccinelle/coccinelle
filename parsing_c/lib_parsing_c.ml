
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


let min_ii_by_pos xs = 
  xs +> Common.foldl1 (fun acc e -> 
    if Ast_c.get_pos_of_info e <= Ast_c.get_pos_of_info acc
      && not (Ast_c.is_al_info (fst e))
    then e
    else acc
  )
  



