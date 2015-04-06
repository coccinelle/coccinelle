module Ast0 = Ast0_cocci
module Ast = Ast_cocci
module MV = Meta_variable

(* ------------------------------------------------------------------------- *)

(* Generates a context mode rule with positions and stars!
 * May generate an extra disjunction rule if the original rule calls for it.
 *
 * Invariants:
 *  - The rule contains */+/- ! No need for context generation otherwise ...
 *  - The rule's name or new name has to be valid! no whitespace funny business
 *  - (however, it can be <default rule name><number> at this point).
 *)

(* ------------------------------------------------------------------------- *)
(* CONTEXT RULE GENERATION FUNCTIONS *)

type t = (Rule_header.t * Rule_body.t) list

(* new_name is Some <new rulename> if the original rule is unnamed.
 * disj_map is a disjunction map for checking patches (see detect_patch.ml)
 * rule is the Ast0_cocci.parsed_rule that we want to generate!
 * context_mode indicates which generation logic to follow (see above)
 *
 * returns: Context_rule.t (generated rule) and list of added metapositions,
 * inherited from the generated rule.
 *)
let generate ~new_name ~disj_map ~rule ~context_mode =
  match rule with
  | Ast0.InitialScriptRule (nm,_,_,_,_)
  | Ast0.FinalScriptRule (nm,_,_,_,_)
  | Ast0.ScriptRule (nm,_,_,_,_,_) ->
      failwith
        ("Internal error: Can't generate a context rule for a script rule! " ^
         "The rule is: " ^ nm)

  | Ast0.CocciRule ((minus_rule,_,(isos,dropisos,deps,nme,exists)),_,_) ->

      let nm = (match new_name with Some nm -> nm | None -> nme) in

      (* generated rule names *)
      let cnm = Globals.get_context_name ~context_mode nm in
      let dnm = Globals.get_disj_name nm in

      (* rule header *)
      (* call mv unparser on original name in order to avoid rule inheritance *)
      let meta_vars = MV.unparse ~minus_rule ~rulename:nme in
      let rh = Rule_header.generate_context
        ~isos ~dropisos ~deps ~meta_vars ~context_mode in

      (* generated context rule body and positions *)
      let (pos,(res,disj)) =
        Rule_body.generate ~rule_name:nm ~disj_map ~context_mode ~minus_rule in

      let _ = if List.length pos = 0 then failwith
        ("MEGA ERROR: Congratulations! You managed to write a Coccinelle " ^
         "rule that sgen was unable to add a position to! The rule is \"" ^
         nme ^ "\".") in

      let pos_mv = List.map (MV.make ~typ:"position " ~rule_name:"") pos in
      let pos_inh = List.map (MV.inherit_rule ~new_rule:cnm) pos_mv in

      (* check if any extra generated disj rule *)
      match disj with
      | None ->
          let rhd = rh ~exists ~rulename:cnm ~meta_pos:pos_mv in
          ([(rhd, res)], pos_inh)
      | Some disj ->
          (* first generated rule has no stars, therefore exists in header *)
          let crh = rh ~rulename:cnm ~exists:Ast.Exists ~meta_pos:pos_mv in
          let drh = rh ~rulename:dnm ~exists ~meta_pos:pos_inh in
          ([(crh, res); (drh, disj)], pos_inh)


(* prints list of Context_rule.t's *)
let print out l =
  List.iter (fun (rh,rb) -> Rule_header.print out rh; Rule_body.print out rb) l
