module Ast0 = Ast0_cocci
module V0 = Visitor_ast0
module VT0 = Visitor_ast0_types
module IntMap = Common.IntMap

(* ------------------------------------------------------------------------- *)

(* Detects whether a rule is a */+/- rule or not.
 *
 * Generates a disjunction map for a rule, which maps each disjunction within
 * the rule to a list of bools indicating whether each disjunction case has
 * */+/-.
 * (this is useful for determining whether a rule uses 'pattern matching'.)
 *)

(* ------------------------------------------------------------------------- *)
(* HELPERS *)

(* merges two disjunction maps.
 * runs through all keys in both maps, unlikely to be a problem since there
 * won't be many keys (each key representing one disjunction) *)
let merge =
  let fn key aopt bopt = match aopt, bopt with
    | Some a, Some b -> Some (List.map2 (||) a b)
    | Some a, None | None, Some a -> Some a
    | None, None -> None in
  IntMap.merge fn

(* mcodes contain the actual information about */+/-. *)
let mcode = function
  | (x, a, info, Ast0.CONTEXT _, pos, _) -> (false, IntMap.empty)
  | (x, a, info, Ast0.MINUS _, pos, _) -> (true, IntMap.empty)
  | (x, a, info, Ast0.PLUS _, pos, _) -> (true, IntMap.empty)
  | (x, a, info, Ast0.MIXED _, pos, _) -> failwith "not possible"

(* Disjunction handler.
 * takes left and right parenthesis mcodes, list of pipe separator mcodes,
 * the list of different cases and one function to handle each case.
 * Parentheses and pipes are always context mode.
 * Returns
 * (whether there is a patch in any of the disjunction cases,
 *  a mapping of the beginning line number of the disj to a list of
 *  bools indicating whether each of the disjunctions are a patch or no) *)
let handle_disj lp rp pipelist clist cfn =
  let index = Ast0.get_mcode_line lp in
  let disj_patches (is_patch, acc_list, acc_map) case =
    let (case_is_p, case_map) = cfn case in
    (is_patch || case_is_p, case_is_p :: acc_list, merge case_map acc_map) in
  (*contains_patch is a bool denoting whether the whole disj contains a patch
   *disj_patch is a list of bools, each bool representing a disj case
   *acc are the accumulated disjunctions within the disjunction *)
  let (contains_patch, disj_patch, acc) =
    List.fold_left disj_patches (false, [], IntMap.empty) clist in
  (contains_patch, IntMap.add index (List.rev disj_patch) acc)


(* ------------------------------------------------------------------------- *)
(* THE COMBINER *)

let patch_combiner =
  let bind (x, m1) (y, m2) = (x || y, IntMap.fold IntMap.add m1 m2) in
  let option_default = (false, IntMap.empty) in

  (* apply the passed function, do nothing else *)
  let donothing r k e = k e in

  let meta_mcode = mcode in
  let string_mcode = mcode in
  let const_mcode = mcode in
  let assign_mcode = mcode in
  let fix_mcode = mcode in
  let unary_mcode = mcode in
  let binary_mcode = mcode in
  let cv_mcode = mcode in
  let sign_mcode = mcode in
  let struct_mcode = mcode in
  let storage_mcode = mcode in
  let inc_mcode = mcode in

  let dotsexprfn = donothing in
  let dotsinitfn = donothing in
  let dotsparamfn = donothing in
  let dotsdeclfn = donothing in
  let dotscasefn = donothing in
  let initfn = donothing in
  let paramfn = donothing in
  let forinfofn = donothing in
  let string_fragmentfn = donothing in
  let topfn = donothing in
  let dotsstmtfn = donothing in

  let identfn c fn v = match Ast0.unwrap v with
    | Ast0.DisjId(lp, idlist, pipelist, rp) ->
        handle_disj lp rp pipelist idlist c.VT0.combiner_rec_ident
    | _ -> fn v in
  let exprfn c fn v = match Ast0.unwrap v with
    | Ast0.DisjExpr(lp, exprlist, pipelist, rp) ->
        handle_disj lp rp pipelist exprlist c.VT0.combiner_rec_expression
    | _ -> fn v in
  let tyfn c fn v = match Ast0.unwrap v with
    | Ast0.DisjType(lp, tylist, pipelist, rp) ->
        handle_disj lp rp pipelist tylist c.VT0.combiner_rec_typeC
    | _ -> fn v in
  let declfn c fn v = match Ast0.unwrap v with
    | Ast0.DisjDecl(lp, decllist, pipelist, rp) ->
        handle_disj lp rp pipelist decllist c.VT0.combiner_rec_declaration
    | _ -> fn v in
  let casefn c fn v = match Ast0.unwrap v with
    | Ast0.DisjCase(lp, caselist, pipelist, rp) ->
        handle_disj lp rp pipelist caselist c.VT0.combiner_rec_case_line
    | _ -> fn v in
  let stmtfn c fn v = match Ast0.unwrap v with
    | Ast0.Disj(lp, sdlist, pipelist, rp) ->
        handle_disj lp rp pipelist sdlist c.VT0.combiner_rec_statement_dots
    | _ -> fn v in

  V0.flat_combiner bind option_default
    meta_mcode string_mcode const_mcode assign_mcode fix_mcode unary_mcode
    binary_mcode cv_mcode sign_mcode struct_mcode storage_mcode
    inc_mcode
    dotsexprfn dotsinitfn dotsparamfn dotsstmtfn dotsdeclfn dotscasefn
    identfn exprfn tyfn initfn paramfn declfn stmtfn forinfofn casefn
    string_fragmentfn topfn


(* ------------------------------------------------------------------------- *)
(* ENTRY POINT *)

let detect = function
  | Ast0.InitialScriptRule _
  | Ast0.FinalScriptRule _
  | Ast0.ScriptRule _ -> (false, IntMap.empty)
  | Ast0.CocciRule ((minus,_,_),(plus,_),_) ->
      let handle_toplvl (is_patch, disj) tpl =
        let (r1, r2) = patch_combiner.VT0.combiner_rec_top_level tpl in
        (is_patch || r1, merge disj r2) in
      let rule = List.fold_left handle_toplvl (false, IntMap.empty) in
      let (p1,p2) = rule plus in
      let (m1,m2) = rule minus in
      (p1 || m1, merge p2 m2)

let detect_statement_dots s =
  let (has_minus, _) = patch_combiner.VT0.combiner_rec_statement_dots s in
  has_minus

let get_patch_rules =
  let rec get fn = function
    | x::xs ->
        let (is_patch, disj_map) = detect x in
        if is_patch
        then get (fun (a,b) -> fn (x::a, disj_map::b)) xs
        else get fn xs
    | [] -> fn ([],[]) in
  get (fun x -> x)
