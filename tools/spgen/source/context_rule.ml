(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

module Ast0 = Ast0_cocci
module Ast = Ast_cocci
module MV = Meta_variable

(* ------------------------------------------------------------------------- *)

(* Generates a context mode rule with metapositions and stars!
 * May generate an extra disjunction rule if the original rule calls for it.
 *)

(* ------------------------------------------------------------------------- *)
(* CONTEXT RULE GENERATION FUNCTIONS *)

type t = (Rule_header.t * Rule_body.t) list

let generate ~context_mode ~disj_map ~new_name ~rule =

  match rule with
  | Ast0.InitialScriptRule (nm,_,_,_,_)
  | Ast0.FinalScriptRule (nm,_,_,_,_)
  | Ast0.ScriptRule (nm,_,_,_,_,_) ->
      failwith
        ("Internal error: Can't generate a context rule for a script rule! " ^
         "The rule is: " ^ nm)

  | Ast0.CocciRule ((minus_rule,_,(isos,drop_isos,deps,old_nm,exists)),_,_) ->

      let context_nm = Globals.get_context_name ~context_mode new_name in
      let disj_nm = Globals.get_disj_name new_name in

      let meta_vars = MV.extract ~minus_rule ~rule_name:old_nm in
      let deps = Globals.add_context_dependency ~context_mode deps in
      let rh_fn = Rule_header.generate ~isos ~drop_isos ~deps ~meta_vars in

      let (pos, (context_body, disj)) =
        Rule_body.generate ~context_mode ~disj_map ~minus_rule in
      let _ = if pos = [] then failwith
        ("MEGA ERROR: Congratulations! You managed to write a Coccinelle " ^
         "rule that spgen was unable to add a position to! The rule is \"" ^
         old_nm ^ "\".") in

      (* the added position metavariables in local scope (for headers) *)
      let pos_mv = List.map (MV.make ~typ:"position ") pos in

      (* the added position metavariables in inherited scope (for scripts) *)
      let pos_inh = List.map (MV.inherit_rule ~new_rule:context_nm) pos_mv in

      match disj with
      | None ->

          let context_header =
            rh_fn ~exists ~rule_name:context_nm ~meta_pos:pos_mv in
          ([(context_header, context_body)], pos_inh)

      | Some disj_body ->

          (* context rule has no stars, therefore "exists" in header *)
          let context_header =
            rh_fn ~rule_name:context_nm ~exists:Ast.Exists ~meta_pos:pos_mv in
          let disj_header =
            rh_fn ~rule_name:disj_nm ~exists ~meta_pos:pos_inh in
          ([(context_header, context_body); (disj_header, disj_body)], pos_inh)

let print out l =
  List.iter (fun (rh,rb) -> Rule_header.print out rh; Rule_body.print out rb) l
