(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

module Ast0 = Ast0_cocci
module Snap = Snapshot
module PG = Position_generator

(* ------------------------------------------------------------------------- *)

(* Returns snapshot that has both updated result (added rule with no stars)
 * and updated disj_result (rule with stars).
 *)

(* ------------------------------------------------------------------------- *)
(* TYPE HANDLER FUNCTIONS *)

type statement_dots_fn = Ast0.statement Ast0.dots -> Snap.t -> Snap.t

type string_fn = string Ast0.mcode -> Snap.t -> Snap.t

type statement_fn = Ast0.statement -> Snap.t -> Snap.t

type expression_fn = Ast0.expression -> Snap.t -> Snap.t

type ident_fn = Ast0.ident -> Snap.t -> Snap.t

type declaration_fn = Ast0.declaration -> Snap.t -> Snap.t

type field_fn = Ast0.field -> Snap.t -> Snap.t

(* ------------------------------------------------------------------------- *)
(* DISJUNCTION HANDLER *)

let ( >> ) f g x = g (f x)

(* given the components of a disjunction + functions to handle them + snapshot:
 * returns new snapshot that has the disjunction added in the context rule.
 * this may include splitting the rule and/or adding stars where appropriate.
 *)
let handle_disj
  ~lp (* left parenthesis, string mcode *)
  ~rp (* right parenthesis, string mcode *)
  ~pipes (* separator pipes, string mcode list *)
  ~cases (* disjunction cases, 'a list *)
  ~casefn (* function to handle one disj case, 'a -> snapshot -> snapshot *)
  ~singlefn (* casefn for only one patch, same type as casefn *)
  ~strfn (* string mcode handler, string mcode -> snapshot -> snapshot *)
  ~at_top (* true: disj is the only thing so don't add another rule, bool *)
  snapshot =

  let index = Ast0.get_mcode_line lp in
  let boollist = Snap.get_disj index snapshot in
  let combined = List.combine cases boollist in

  (* determine if all or none are patches *)
  let all_same = function [] -> true | x :: xs -> List.for_all (( = ) x) xs in

  (* true if multiple patch cases; ie. we want disjunction parentheses *)
  let mult_stmt = List.length (List.filter (fun x -> x) boollist) <> 1 in
  let casefn = if mult_stmt then casefn else singlefn in

  (* keep the same positions if several disjunctions *)
  let freeze_pos = if mult_stmt then Snap.do_freeze_pos else (fun x -> x) in

  (* handle each disjunction case one at a time
   * setmodefn is the function that sets the generation mode of the snapshot.
   * tblist is a list of (disj case, whether it is a patch)
   *)
  let handle_cases set_modefn tblist pipes =
    let rec handle_cases' tblist pipes fn =
      match tblist, pipes with
      | [(t,b)], [] ->
          fn >> set_modefn b >> casefn t
      | (t,b) :: ts, p :: ps ->
          handle_cases' ts ps (fn >> set_modefn b >> casefn t >> strfn p)
      | _ ->
          assert false (* should be exactly one more stmt than pipes *) in
    handle_cases' tblist pipes (fun x -> x) in

  let disj =

    (* CASE 1: all or none are patches or toplevel, no extra rule needed,
     * always generate positions (unless we're in no_gen mode).
     *)
    if at_top || all_same boollist then begin
      let handle_no_gen = handle_cases (fun _ y -> y) in
      strfn lp >> handle_no_gen combined pipes >> strfn rp
    end

    (* CASE 2: only some are patches, generate extra rule (disj result) *)
    else begin

      (* if b is true, DO generate positions/stars and add to disj result *)
      let set_add_disj b = Snap.set_no_gen (not b) >> Snap.set_disj_mode b in
      let handle_do_gen = handle_cases set_add_disj in

      Snap.init_disj_result
      >> set_add_disj mult_stmt >> strfn lp
      >> handle_do_gen combined pipes
      >> set_add_disj mult_stmt >> strfn rp
      >> set_add_disj true
    end in

  freeze_pos disj snapshot


(* ------------------------------------------------------------------------- *)
(* ENTRY POINT *)

(* These functions all return a snapshot that has the extra disjunction rule
 * generated (if necessary) *)

(* The at_top flag means that the code is not surrounded by starrable
 * components, ie. it should not be split into two rules. (it needs to be more
 * accurate; see rule_body.ml)
 *)

(* Returns snapshot that has added generated statement disjunction rule *)
let generate_statement ~stmtdotsfn ~strfn ~stmtfn ~stmt ~at_top =

  (* inserts one position if in generation mode.
   * We only want one position per disjunction case (since they all have the
   * same metaposition), so just add position to first possible case.
   *)
  let sdotsfn sd snp =
    let std_no_pos = List.fold_left (fun a b -> a >> stmtfn b) (fun x -> x) in
    let rec std' l snp =
      match l with
      | [] -> assert false (* no disj patches with only unpositionable cases *)
      | x::xs ->
          (match PG.statement_pos x snp with
           | Some (x, snp) ->
               (Snap.set_no_gen true
                >> std_no_pos (x::xs)
                >> Snap.set_no_gen false) snp
           | None -> std' xs (stmtfn x snp)) in
    let add_pos_function = if Snap.no_gen snp then std_no_pos else std' in
    add_pos_function (Ast0.unwrap sd) snp in

  match Ast0.unwrap stmt with
  | Ast0.Disj(lp, sdlist, pipes, rp) ->
      handle_disj ~lp ~rp ~pipes ~cases:sdlist ~casefn:sdotsfn
        ~singlefn:stmtdotsfn ~strfn ~at_top
  | _ -> failwith "only disj allowed in here"


(* Returns snapshot that has added generated expression disjunction rule *)
let generate_expression ~strfn ~exprfn ~expr ~at_top s =

  (* inserts one position if in generation mode *)
  let expposfn e snp =
    if Snap.no_gen snp then
      exprfn e snp
    else
      match PG.expression_pos e snp with
      | Some (ee, snp) -> exprfn ee snp
      | None -> failwith "no unpos cases" in

  match Ast0.unwrap expr with
  | Ast0.DisjExpr(lp, elist, pipes, rp) ->
      handle_disj
        ~lp ~rp ~pipes ~cases:elist
        ~casefn:expposfn ~singlefn:expposfn ~strfn ~at_top s
  | _ -> failwith "only disj allowed in here"


(* Returns snapshot that has added generated ident disjunction rule *)
let generate_ident ~strfn ~identfn ~ident ~at_top s =

  (* inserts one position if in generation mode *)
  let idposfn i snp =
    if Snap.no_gen snp then
      identfn i snp
    else
      let (i, snp) = PG.ident_pos i snp in
      identfn i snp in

  match Ast0.unwrap ident with
  | Ast0.DisjId(lp, ilist, pipes, rp) ->
      handle_disj
        ~lp ~rp ~pipes ~cases:ilist
        ~casefn:idposfn ~singlefn:idposfn ~strfn ~at_top s
  | _ -> failwith "only disj allowed in here"


(* Returns snapshot that has added generated declaration disjunction rule *)
let generate_declaration ~strfn ~declfn ~decl ~at_top s =

  (* inserts one position if in generation mode *)
  let decposfn d snp =
    if Snap.no_gen snp then
      declfn d snp
    else
      match PG.declaration_pos d snp with
      | Some (dd, snp) -> declfn dd snp
      | None -> failwith "no unpos cases" in

  match Ast0.unwrap decl with
  | Ast0.DisjDecl(lp, dlist, pipes, rp) ->
      handle_disj
        ~lp ~rp ~pipes ~cases:dlist
        ~casefn:decposfn ~singlefn:decposfn ~strfn ~at_top s
  | _ -> failwith "only disj allowed in here"

(* Returns snapshot that has added generated declaration disjunction rule *)
let generate_field ~strfn ~fieldfn ~field ~at_top s =

  (* inserts one position if in generation mode *)
  let decposfn d snp =
    if Snap.no_gen snp then
      fieldfn d snp
    else
      match PG.field_pos d snp with
      | Some (dd, snp) -> fieldfn dd snp
      | None -> failwith "no unpos cases" in

  match Ast0.unwrap field with
  | Ast0.DisjField(lp, dlist, pipes, rp) ->
      handle_disj
        ~lp ~rp ~pipes ~cases:dlist
        ~casefn:decposfn ~singlefn:decposfn ~strfn ~at_top s
  | _ -> failwith "only disj allowed in here"
