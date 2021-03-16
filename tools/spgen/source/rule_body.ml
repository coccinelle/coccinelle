(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

module Ast = Ast_cocci
module Ast0 = Ast0_cocci
module V0 = Visitor_ast0
module VT0 = Visitor_ast0_types
module S = Ast_tostring
module Snap = Snapshot
module PG = Position_generator
module DG = Disj_generator

(* ------------------------------------------------------------------------- *)

(* Generates the rule body of a context rule.
 *
 * Main logic for starring lines:
 * CONTEXT ( * ): put the stars where they were in the original rule.  Do not
 * change rule layout, only add positions.
 *
 * PATCH (+/-): If a statement dots contain any minus transformations, put the
 * star where the minus is. If not, put a star where a position was generated.
 * All lines are always non-starred, unless there turns out to be
 * an added metaposition (from the position generator) on that same line, in
 * which case the whole line becomes star mode. A position is added if it is
 * in Ast0.PLUS context, since natural positions are NEVER in PLUS context.
 *
 * Uses a (Snapshot.t -> Snapshot.t) combiner. Snapshot is a state type that
 * contains the generated rule and state information. So each rule component
 * gets turned into a function that modifies the state. The result is a
 * composite function that takes an initial (presumably empty) snapshot and
 * turns it into a snapshot that contains a full generated rule.
 *)

(* ------------------------------------------------------------------------- *)
(* HELPERS *)

(* Function composition *)
let ( >> ) f g x = g (f x)

(* Continuously apply fn ('b -> 'a -> 'a) with lst ('b list) and start ('a). *)
let reduce fn lst start = List.fold_left (fun a b -> fn b a) start lst

(* print helpers for context rules (which are really just string lists) *)
let print_newl out = output_string out "\n"

let printfn out x =
  List.iter (fun x -> output_string out x; print_newl out) x;
  print_newl out


(* ------------------------------------------------------------------------- *)
(* FUNCTIONS TO HANDLE SPECIAL CASES *)

let starrify_line a = Snap.set_mode_star ~arity:a

(* metapositions are represented as lists of Ast0.anythings.
 * PATCH MODE: if a position is in plus mode ("added"), ie. made by the
 * position generator, it indicates an important line which should be starred.
 * Mutual recursion because meta anythings can have metas themselves ...
 *)
let rec add_positions ~context_mode lst =
  reduce (add_pos ~context_mode) lst

and add_pos ~context_mode = function

  (* these are the added/generated positions (hence the PLUS mode) *)
  | Ast0.MetaPosTag(Ast0.MetaPos(((_,nm),arity,_,Ast0.PLUS _,_,_),_,_)) ->
      let default = Snap.add_with_arity ("@"^nm) arity in
      if context_mode then
        default
      else
        starrify_line arity
        >> default
  | Ast0.MetaPosTag(Ast0.MetaPos(((_,nm),arity,_,_,p,_),_,_))

  (* extracting the node is equivalent to calling Ast0.unwrap *)
  | Ast0.ExprTag
      {Ast0.node = Ast0.MetaExpr(((_,nm),arity,_,_,p,_),_,_,_,_,_); _}
  | Ast0.StmtTag {Ast0.node = Ast0.MetaStmt(((_,nm),arity,_,_,p,_),_,_); _}
  | Ast0.DeclTag {Ast0.node = Ast0.MetaDecl(((_,nm),arity,_,_,p,_),_,_); _}
  | Ast0.IdentTag {Ast0.node = Ast0.MetaId(((_,nm),arity,_,_,p,_),_,_,_); _}
  | Ast0.TypeCTag {Ast0.node = Ast0.MetaType(((_,nm),arity,_,_,p,_),_,_); _} ->
      Snap.add_with_arity ("@"^nm) arity
      >> add_positions ~context_mode !p
  | _ -> failwith "add_pos only supported for metavariables."

(* renders the mcode as a string in the map and updates the line number.
 * context_mode means that the stars are put where the minuses are.
 *)
let mcode ~context_mode ~tostring_fn (x, a, info, mc, pos, _) =

  let default ~add_star =
    Snap.skip ~rule_line:(info.Ast0.pos_info.Ast0.line_start)
    >> (if add_star then starrify_line a else (fun a -> a))
    >> Snap.add info.Ast0.whitespace
    >> Snap.add_with_arity (tostring_fn x) a
    >> add_positions ~context_mode !pos in

  match mc with
  | Ast0.MINUS _ -> default ~add_star:context_mode
  | Ast0.CONTEXT _ -> default ~add_star:false
  | _ -> failwith "plus and mixed not allowed, should be the minus ast0..."

(* Handle Ast0_cocci.whenmodes. Primary purpose is to handle WhenModifiers
 * and WhenNotTrue/False which are not parameterised in the visitor.
 *)
let whencodes ~strfn ~exprfn ~notfn ~alwaysfn l =
  let add_whens = function
    | Ast0.WhenNot(whenmc, notequalmc, a) ->
        strfn whenmc
        >> strfn notequalmc
        >> notfn a
    | Ast0.WhenAlways(whenmc, equalmc, a) ->
        strfn whenmc
        >> strfn equalmc
        >> alwaysfn a
    | Ast0.WhenModifier(whenmc, a) ->
        strfn whenmc
        >> Snap.add (" " ^ (S.whenmodifier_tostring a))
    | Ast0.WhenNotTrue(whenmc, notequalmc, expr) ->
        strfn whenmc
        >> strfn notequalmc
        >> Snap.add " true"
        >> exprfn expr
    | Ast0.WhenNotFalse(whenmc, equalmc, expr) ->
        strfn whenmc
        >> strfn equalmc
        >> Snap.add " false"
        >> exprfn expr in
  Snap.do_whencode (reduce add_whens l)


(* This is where the magic happens!
 * Inserts stars/positions into the statements of a statement_dots.
 * Only give positions and stars to statements if they are the first in a dots
 * or come immediately after a nest, dots, disjunction, or metastatement.
 *)
let star_dotsstmtfn ~context_mode stmtfn stmtdots =

  (* inserts position into statement where structurally appropriate *)
  let star_stmtfn stmt snp =
    let _ = assert (not (Snap.no_gen snp)) in
    match PG.statement_pos stmt snp with
    | Some (stmt, snp) -> stmtfn stmt snp
    | None -> stmtfn stmt snp in

  (* returns true if the statement can potentially cover large amounts of
   * code/requires special handling and therefore should not be starred.
   *)
  let do_not_star x =
    match Ast0.unwrap x with
    | Ast0.Nest _
    | Ast0.Dots _
    | Ast0.Disj _
    | Ast0.MetaStmt _ -> true
    | _ -> false in

  (* increase line number if not in context_mode (if context_mode, we don't
   * want to modify layout, only add positions).
   *)
  let inc_line = if context_mode then (fun x -> x) else Snap.inc_line in

  (* puts stars and positions in statements that come after one of the cases
   * in do_not_star. Insert newline after a do_not_star case.
   *)
  let rec insert_stars star_current fn =
    let starfn = if star_current then star_stmtfn else stmtfn in
    function
      | [] -> fn
      | [x] ->
          if do_not_star x
          then fn >> stmtfn x
          else fn >> starfn x
      | x::xs ->
          if do_not_star x
          then insert_stars true (fn >> stmtfn x >> inc_line) xs
          else insert_stars false (fn >> starfn x) xs in

  insert_stars true (fun x -> x) (Ast0.unwrap stmtdots)


(* ------------------------------------------------------------------------- *)
(* THE COMBINER *)

(* The type of the combiner is (Snapshot.t -> Snapshot.t) which enables us to
 * pass states from token to token. We need the states to keep track of our
 * current context and for proper line formatting.
 * The state also contains the generated rule.
 *)
let rec gen_combiner ~context_mode =
  let bind x y = x >> y in (* do x then apply y to the result *)
  let option_default = (fun x -> x) in

  (* apply the passed function, do nothing else *)
  let donothing r k e = k e in

  let mcode a = mcode ~context_mode ~tostring_fn:a in
  let meta_mcode = mcode S.meta_tostring in
  let string_mcode = mcode (fun x -> x) in
  let const_mcode = mcode S.constant_tostring in
  let simpleAssign_mcode = mcode (fun x -> x) in
  let opAssign_mcode = mcode S.arith_tostring in
  let fix_mcode = mcode S.fix_tostring in
  let unary_mcode = mcode S.unary_tostring in
  let arithOp_mcode = mcode S.arith_tostring in
  let logicalOp_mcode = mcode S.logic_tostring in
  let cv_mcode = mcode S.const_vol_tostring in
  let sign_mcode = mcode S.sign_tostring in
  let struct_mcode = mcode S.struct_union_tostring in
  let storage_mcode = mcode S.storage_tostring in
  let inc_mcode = mcode S.inc_file_tostring in

  let dotsexprfn = donothing in
  let dotsinitfn = donothing in
  let dotsparamfn = donothing in
  let dotsdeclfn = donothing in
  let dotsfieldfn = donothing in
  let dotsenumdeclfn = donothing in
  let dotscasefn = donothing in
  let dotsdefparfn = donothing in
  let assignOpfn = donothing in
  let binaryOpfn = donothing in
  let tyfn = donothing in
  let initfn = donothing in
  let enumdeclfn = donothing in
  let paramfn = donothing in
  let forinfofn = donothing in
  let casefn = donothing in
  let string_fragmentfn = donothing in
  let attributefn = donothing in
  let attr_argfn = donothing in

  (* Universal special cases, regardless of no_gen mode:
   * Disjunctions with SmPL style pattern-matching may need to be split into
   * two rules.
   *)
  let identfn _ c_identfn ident =
    match Ast0.unwrap ident with
    | Ast0.DisjId _ ->
        DG.generate_ident
          ~strfn:string_mcode ~identfn:c_identfn ~ident ~at_top:false
    | _ -> c_identfn ident in

  let exprfn _ c_exprfn expr =
    match Ast0.unwrap expr with
    | Ast0.DisjExpr _ ->
        DG.generate_expression
          ~strfn:string_mcode ~exprfn:c_exprfn ~expr ~at_top:false
    | _ -> c_exprfn expr in

  let declfn _ c_declfn decl =
    match Ast0.unwrap decl with
    | Ast0.DisjDecl _ ->
        DG.generate_declaration
          ~strfn:string_mcode ~declfn:c_declfn ~decl ~at_top:false
    | _ -> c_declfn decl in

  let fieldfn _ c_fieldfn field =
    match Ast0.unwrap field with
    | Ast0.DisjField _ ->
        DG.generate_field
          ~strfn:string_mcode ~fieldfn:c_fieldfn ~field ~at_top:false
    | _ -> c_fieldfn field in

  let stmtfn combiner c_stmtfn stmt =
    let c_dotsstmtfn = combiner.VT0.combiner_rec_statement_dots in
    let c_substmtfn = combiner.VT0.combiner_rec_statement in
    let c_exprfn = combiner.VT0.combiner_rec_expression in
    let whncodes = whencodes
      ~strfn:string_mcode ~exprfn:c_exprfn ~notfn:c_dotsstmtfn
      ~alwaysfn:c_substmtfn in
    let inc_star = if context_mode then (fun x -> x) else Snap.inc_star in

    match Ast0.unwrap stmt with

    (* nest and dots are explicitly written out rather than
     * letting the visitor handle them. Otherwise whencodes would be ignored.
     * (whencodes are difficult to parameterise in the visitor due to typing).
     *
     * nest, dots, and metastatements can represent code slices
     * of arbitrary length and should therefore not be starred, so if their
     * current line is starred, put them on a new line (inc_star).
     *)
    | Ast0.Nest(starter,stmt_dots,ender,whn,multi) ->
        inc_star
        >> string_mcode starter
        >> whncodes whn
        >> c_dotsstmtfn stmt_dots
        >> string_mcode ender

    | Ast0.Dots(dots,whn) ->
        inc_star
        >> string_mcode dots
        >> whncodes whn

    | Ast0.MetaStmt _ ->
        inc_star
        >> c_stmtfn stmt

    | Ast0.Disj _ ->
        DG.generate_statement
          ~stmtdotsfn:c_dotsstmtfn ~strfn:string_mcode ~stmtfn:c_substmtfn
	  ~stmt ~at_top:false
    | _ -> c_stmtfn stmt in

  (* positions and stars are added here!!! *)
  let dotsstmtfn c c_dotsstmtfn dotsstmt =
    let stmtfn = c.VT0.combiner_rec_statement in
    (fun snp ->
       if Snap.no_gen snp (* add no positions; this is relevant in whencodes *)
       then c_dotsstmtfn dotsstmt snp
       else star_dotsstmtfn ~context_mode stmtfn dotsstmt snp) in

  (* detect if disj is the only thing, in which case we don't want to split
   * the disjunction rule.
   * TODO: better detection of when to set at_top! for example, should not
   * split if the only other stmts are unstarrable.
   * (this includes the case where no statement outside the disjunction has a
   * minus, since the pos generator will ignore them due to minus inside disj!)
   *)
  let topfn c c_topfn top =
    match Ast0.unwrap top with
    | Ast0.CODE stmtdots ->
        (* detects if any of the statements in here contain minuses in which
         * case we put the stars where the minuses are.
         * NOTE: uses only minus rule, so does not detect plus slices. This is
         * exactly what we want to happen as plus slices are not in generated
         * rule!
         *)
        let detect_patch = Detect_patch.make_statement_dots stmtdots in
        let has_minuses = Detect_patch.is_patch detect_patch in
        let c =
          if context_mode
          then c
          else gen_combiner ~context_mode:has_minuses in
        (match Ast0.unwrap stmtdots with
         | [{Ast0.node = Ast0.Disj _; _} as x] ->
             DG.generate_statement
               ~stmtdotsfn:c.VT0.combiner_rec_statement_dots
               ~stmtfn:c.VT0.combiner_rec_statement
               ~strfn:string_mcode ~stmt:x ~at_top:true
         | _ -> c.VT0.combiner_rec_statement_dots stmtdots
        )
    | _ -> c_topfn top
  in

  V0.flat_combiner bind option_default
    meta_mcode string_mcode const_mcode simpleAssign_mcode opAssign_mcode
    fix_mcode unary_mcode arithOp_mcode logicalOp_mcode cv_mcode sign_mcode
    struct_mcode storage_mcode inc_mcode
    dotsexprfn dotsinitfn dotsparamfn dotsstmtfn dotsdeclfn dotsfieldfn
    dotsenumdeclfn dotscasefn dotsdefparfn
    identfn exprfn assignOpfn binaryOpfn tyfn initfn paramfn declfn fieldfn
    enumdeclfn stmtfn forinfofn casefn string_fragmentfn attributefn attr_argfn topfn


(* ------------------------------------------------------------------------- *)
(* ENTRY POINT *)

type t = string list

(* Creates a context mode rule for the input rule.
 * Returns list of added metapositions and the new rule.
 *)
let generate ~context_mode ~disj_map ~minus_rule =
  let snp = Snap.make ~disj_map in
  let combiner = gen_combiner ~context_mode in
  let final = reduce combiner.VT0.combiner_rec_top_level minus_rule snp in
  (Snap.get_positions final, Snap.get_result final)

let print = printfn
