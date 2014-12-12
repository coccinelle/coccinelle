module Ast = Ast_cocci
module Ast0 = Ast0_cocci
module V0 = Visitor_ast0
module VT0 = Visitor_ast0_types
module S = Ast_tostring
module GT = Generator_types
module PG = Position_generator
module DG = Disj_generator

(* ------------------------------------------------------------------------- *)

(* Generates the rule body of a context rule.
 *
 * Main logic for starring lines:
 * CONTEXT ( * ): put the stars where they were in the original rule.
 *
 * PATCH (+/-): If a statement dots contain any minus transformations, put the
 * star where the minus is. If not, put a star where a position was generated.
 * All lines are always non-starred, unless there turns out to be
 * an added metaposition (from the position generator) on that same line, in
 * which case the whole line becomes star mode. A position is added if it is
 * in Ast0.PLUS context, since natural positions are NEVER in PLUS context.
 *
 * Uses a (snapshot -> snapshot) combiner. Snapshot is a state type that
 * contains the generated rule and state information. So each rule component
 * gets turned into a function that modifies the state. The result is a
 * composite function that takes an initial (presumably empty) snapshot and
 * turns it into a snapshot that contains a full generated rule.
 *)

(* ------------------------------------------------------------------------- *)
(* HELPERS *)

(* Function composition *)
let ( >> ) f g x = g (f x)

(* Continuously apply function ('b -> 'a -> 'a) with start value ('a) and
 * the values in a 'b list. *)
let reduce fn lst start = List.fold_left (fun a b -> fn b a) start lst

(* print helpers for context rules (which are really just string lists) *)
let print_newl outch = output_string outch "\n"
let printfn outch x =
  List.iter (fun x -> output_string outch x; print_newl outch) x;
  print_newl outch

(* ------------------------------------------------------------------------- *)
(* FUNCTIONS TO HANDLE SPECIAL CASES *)

let starrify_line a = GT.set_mode_star ~arity:a

(* metapositions are represented as lists of Ast0.anythings.
 * PATCH MODE: if a position is in plus mode ("added"), ie. made by the
 * position generator, it indicates an important line which should be starred.
 * Mutual recursion because meta anythings can have metas themselves ... *)
let rec add_positions ~context_mode lst =
  reduce (add_pos ~context_mode) lst
and add_pos ~context_mode = function
  (* these are the added/generated positions (hence the PLUS mode) *)
  | Ast0.MetaPosTag(Ast0.MetaPos(((_,nm),arity,_,Ast0.PLUS _,_,_),_,_)) ->
      let default = GT.add_with_arity ("@"^nm) arity in
      if context_mode then default else starrify_line arity >> default
  | Ast0.MetaPosTag(Ast0.MetaPos(((_,nm),arity,_,_,p,_),_,_))
  (* extracting the node is equivalent to calling Ast0.unwrap *)
  | Ast0.ExprTag {Ast0.node = Ast0.MetaExpr(((_,nm),arity,_,_,p,_),_,_,_,_); _}
  | Ast0.StmtTag {Ast0.node = Ast0.MetaStmt(((_,nm),arity,_,_,p,_),_); _}
  | Ast0.DeclTag {Ast0.node = Ast0.MetaDecl(((_,nm),arity,_,_,p,_),_); _}
  | Ast0.IdentTag {Ast0.node = Ast0.MetaId(((_,nm),arity,_,_,p,_),_,_,_); _}
  | Ast0.TypeCTag {Ast0.node = Ast0.MetaType(((_,nm),arity,_,_,p,_),_); _} ->
      GT.add_with_arity ("@"^nm) arity >> add_positions ~context_mode !p
  | _ -> failwith "add_pos only supported for metavariables."

(* renders the mcode as a string in the map and updates the line number. *)
let mcode ~context_mode fn (x, a, info, mc, pos, _) =
  let default =
    GT.skip ~rule_line:(info.Ast0.pos_info.Ast0.line_start)
    >> GT.add info.Ast0.whitespace
    >> GT.add_with_arity (fn x) a
    >> add_positions ~context_mode !pos in
  match mc with
  | Ast0.MINUS _ ->
      if context_mode then starrify_line a >> default else default
  | Ast0.CONTEXT _ -> default
  | _ -> failwith "plus and mixed not allowed, should be the minus ast0..."

(* Handle Ast0_cocci.whenmodes. Primary purpose is to handle WhenModifiers
 * and WhenNotTrue/False which are not parameterised in the visitor.
 *)
let whencodes ~strfn ~exprfn ~notfn ~alwaysfn l =
  let add_whens = function
    | Ast0.WhenNot(w,e,a) -> strfn w >> strfn e >> notfn a
    | Ast0.WhenAlways(w,e,a) -> strfn w >> strfn e >> alwaysfn a
    | Ast0.WhenModifier(w,a) ->
        strfn w >> GT.add (" " ^ (S.whenmodifier_tostring a))
    | Ast0.WhenNotTrue(w,e,a) ->
        strfn w >> strfn e >> GT.add " true" >> exprfn a
    | Ast0.WhenNotFalse(w,e,a) ->
        strfn w >> strfn e >> GT.add " false" >> exprfn a in
  GT.do_whencode (reduce add_whens l)

(* This is where the magic happens!
 * only position and star statements if they are the first in a dots or
 * come immediately after a nest, dots, disjunction, or metastatement. *)
let star_dotsstmtfn comb context_mode stmtdots =

  (* detects if any of the statements in here contain minuses in which case we
   * put the stars where the minuses are. *)
  let has_minuses = Detect_patch.detect_statement_dots stmtdots in
  let c = comb ~context_mode:(context_mode || has_minuses) in
  let stmtfn = c.VT0.combiner_rec_statement in

  let star_stmtfn stmt snp =
    let _ = assert (not (GT.no_gen snp)) in
    match PG.statement_pos stmt snp with
    | Some (stmt, snp) -> stmtfn stmt snp
    | None -> stmtfn stmt snp in

  (* returns true if the statement can potentially cover large amounts of
   * code/requires special handling and therefore should not be starred. *)
  let do_not_star x = match Ast0.unwrap x with
    | Ast0.Nest _ | Ast0.Dots _ | Ast0.Circles _ | Ast0.Stars _ | Ast0.Disj _
    | Ast0.MetaStmt _ -> true | _ -> false in

  (* puts stars and positions in statements that come after one of the cases
   * in no_star. Insert newline after a no_star case. *)
  let rec insert_stars star_current fn =
    let starfn = if star_current then star_stmtfn else stmtfn in
    function
      | [] -> fn
      | [x] -> if do_not_star x then fn >> stmtfn x else fn >> starfn x
      | x::xs ->
          if do_not_star x
          then insert_stars true (fn >> stmtfn x >> GT.inc_line) xs
          else insert_stars false (fn >> starfn x) xs in

  insert_stars true (fun x -> x) (Ast0.undots stmtdots)


(* ------------------------------------------------------------------------- *)
(* THE COMBINER *)

(* The type of the combiner is (snapshot -> snapshot) which enables us to pass
 * states from token to token. We need the states to keep track of our current
 * context and for proper line formatting.
 * The state also contains the generated rule.
 * (not actually recursive, just needs to pass itself on to star_dotsstmtfn to
 *  allow context_mode toggling without making them mutually recursive)
 *)
let rec gen_combiner ~context_mode =
  let bind x y = x >> y in (* do x then apply y to the result *)
  let option_default = (fun x -> x) in

  (* apply the passed function, do nothing else *)
  let donothing r k e = k e in

  let mcode a = mcode ~context_mode a in
  let meta_mcode = mcode S.meta_tostring in
  let string_mcode = mcode (fun x -> x) in
  let const_mcode = mcode S.constant_tostring in
  let assign_mcode = mcode S.assign_tostring in
  let fix_mcode = mcode S.fix_tostring in
  let unary_mcode = mcode S.unary_tostring in
  let binary_mcode = mcode S.binary_tostring in
  let cv_mcode = mcode S.const_vol_tostring in
  let sign_mcode = mcode S.sign_tostring in
  let struct_mcode = mcode S.struct_union_tostring in
  let storage_mcode = mcode S.storage_tostring in
  let inc_mcode = mcode S.inc_file_tostring in

  let dotsexprfn = donothing in
  let dotsinitfn = donothing in
  let dotsparamfn = donothing in
  let dotsdeclfn = donothing in
  let dotscasefn = donothing in
  let tyfn = donothing in
  let initfn = donothing in
  let paramfn = donothing in
  let forinfofn = donothing in
  let casefn = donothing in
  let string_fragmentfn = donothing in

  (* universal special cases, regardless of no_gen mode:
   *- Stmts: Nest, dots, circles, and stars need special whencode handling,
   *  since whencodes are ignored in the visitor.
   *- Stmts: Nest, dots, circles, stars, and metastatements can represent code
   *  slices of arbitrary length and should therefore not be starred, so if
   *  their current line is starred, put them on a new line.
   *- Disjunctions with SmPL style pattern-matching may need to be split into
   *  two rules.
   *)
  let identfn c fn v = match Ast0.unwrap v with
    | Ast0.DisjId _ -> DG.generate_ident
        ~strfn:string_mcode ~identfn:fn ~ident:v ~at_top:false
    | _ -> fn v in

  let exprfn c fn v = match Ast0.unwrap v with
    | Ast0.DisjExpr _ -> DG.generate_expression
        ~strfn:string_mcode ~exprfn:fn ~expr:v ~at_top:false
    | _ -> fn v in

  let declfn c fn v = match Ast0.unwrap v with
    | Ast0.DisjDecl _ -> DG.generate_declaration
        ~strfn:string_mcode ~declfn:fn ~decl:v ~at_top:false
    | _ -> fn v in

  let stmtfn combiner fn v =
    let c_dotsstmtfn = combiner.VT0.combiner_rec_statement_dots in
    let c_exprfn = combiner.VT0.combiner_rec_expression in
    let whncodes = whencodes
      ~strfn:string_mcode ~exprfn:c_exprfn ~notfn:c_dotsstmtfn ~alwaysfn:fn in
    match Ast0.unwrap v with
    | Ast0.Nest(starter,stmt_dots,ender,whn,multi) ->
        GT.inc_star >> string_mcode starter >> whncodes whn
        >> c_dotsstmtfn stmt_dots >> string_mcode ender
    | Ast0.Dots(d,whn) | Ast0.Circles(d,whn) | Ast0.Stars(d,whn) ->
        GT.inc_star >> string_mcode d >> whncodes whn
    | Ast0.MetaStmt _ -> GT.inc_star >> fn v
    | Ast0.Disj _ -> DG.generate_statement
        ~stmtdotsfn:combiner.VT0.combiner_rec_statement_dots
        ~strfn:string_mcode ~stmtfn:fn ~stmt:v ~at_top:false
    | _ -> fn v in

  let dotsstmtfn c fn v =
    (fun x ->
       if GT.no_gen x (* relevant in whencodes *)
       then fn v x
       else star_dotsstmtfn gen_combiner context_mode v x) in

  (* detect if disj is the only thing, in which case we don't want to split
   * the disjunction rule.
   * TODO: should not split either if the only other stmts are unstarrable.
   * This might be okay for now since it is unlikely that a rule would contain
   * something unstarrable (dots, metastmt) and then a disjunction. *)
  let topfn c fn v =
    match Ast0.unwrap v with
    | Ast0.CODE stmtdots ->
        (match Ast0.undots stmtdots with
         | [{Ast0.node = Ast0.Disj _; _} as x] ->
             DG.generate_statement
               ~stmtdotsfn:c.VT0.combiner_rec_statement_dots
               ~stmtfn:c.VT0.combiner_rec_statement
               ~strfn:string_mcode ~stmt:x ~at_top:true
         | _ -> fn v)
    | _ -> fn v
  in

  V0.flat_combiner bind option_default
    meta_mcode string_mcode const_mcode assign_mcode fix_mcode unary_mcode
    binary_mcode cv_mcode sign_mcode struct_mcode storage_mcode
    inc_mcode
    dotsexprfn dotsinitfn dotsparamfn dotsstmtfn dotsdeclfn dotscasefn
    identfn exprfn tyfn initfn paramfn declfn stmtfn forinfofn casefn
    string_fragmentfn topfn


(* ------------------------------------------------------------------------- *)
(* ENTRY POINT *)

type t = string list

(* Creates a context mode rule for the input rule.
 * Returns list of added metapositions and the new rule.
 *)
let generate ?(disj_map = Common.IntMap.empty) ?(context_mode = false)
  ~rule_name ast0 =
  let snp = GT.snap ~disj_map in
  let combiner = gen_combiner ~context_mode in
  let final = reduce combiner.VT0.combiner_rec_top_level ast0 snp in
  (GT.get_positions final, GT.get_result final)

let print = printfn
