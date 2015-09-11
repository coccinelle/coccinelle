(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

module UI = User_input
module Lex = Sgen_lexer

(* ------------------------------------------------------------------------- *)

(* Read config files for user-specified options *)

(* ------------------------------------------------------------------------- *)
(* LEXER FUNCTIONS *)

(* function to be applied continuously on the input coccinelle script *)
let lex_config lexbuf =
  let rec aux ((d,l,k,c,m,o,a,u,r) as res) =
    try
      let result = Lex.token lexbuf in
      match result with
        | Lex.Description d -> aux (d,l,k,c,m,o,a,u,r)
        | Lex.Limitations l -> aux (d,l,k,c,m,o,a,u,r)
        | Lex.Keywords k -> aux (d,l,k,c,m,o,a,u,r)
        | Lex.Confidence c -> aux (d,l,k,c,m,o,a,u,r)
        | Lex.Comments m -> aux (d,l,k,c,m,o,a,u,r)
        | Lex.Options o -> aux (d,l,k,c,m,o,a,u,r)
        | Lex.Authors a -> aux (d,l,k,c,m,o,a,u,r)
        | Lex.Url u -> aux (d,l,k,c,m,o,a,u,r)
        | Lex.Rule (rn,attr) -> aux (d,l,k,c,m,o,a,u, (rn, attr) :: r)
    with Lex.Eof -> res in
  aux ("",[],"","","","",[],"",[])

(* open the input coccinelle script and lex/parse it.
 * the table maps the absolute character position to its line and column number
 *)
let parse filename =
  let _ = Lex.table := Common.full_charpos_to_pos filename in
  Common.with_open_infile filename
    (fun channel ->
       let lexbuf = Lexing.from_channel channel in
       lex_config lexbuf)


(* ------------------------------------------------------------------------- *)
(* PARSER FUNCTIONS *)

(* parse org/report messages and associated metavars.
 * if several org/report messages defined, take the last one
 *)
let parse_msgs attributes =
  let read ((om,ov),(rm,rv)) = function
    | Lex.Org (om,ov) -> ((om,ov), (rm,rv))
    | Lex.Report (rm,rv) -> ((om,ov), (rm,rv)) in
  List.fold_left read (("",[]), ("",[])) attributes

(* add the rule to the User_input.t. *)
let add_rule rule_check t ((oldrnm,newnm),a) =
  let oldrnm = rule_check oldrnm in
  let (org,report) =
    match parse_msgs a with
    | ("",_),("",_) ->
        failwith ("Config error: must specify at least org or report message.")
    | ("",_),mgv | mgv,("",_) ->
        (try
          UI.check_format_string mgv;
          (mgv,mgv)
        with Failure msg ->
          failwith ("Config error: " ^ msg))
    | org, rep ->
        try
          UI.check_format_string org;
          UI.check_format_string rep;
          (org, rep)
        with Failure msg ->
          failwith ("Config error: " ^ msg) in
  let rule_name = match newnm with | Some nm -> nm | None -> oldrnm in
  let rule = UI.Rule.make ~rule_name ~org ~report in
  UI.add_rule ~rule_name:oldrnm rule t

(*rule list is a list of (rulename, attribute list) *)
let add_rules rule_exists rule_list t =
  List.fold_left (fun a b -> add_rule rule_exists a b) t rule_list

(* expanded constructor for User_input.t *)
let make desc limit keys conf comments options authors url =
  let t =
    try
      UI.make ~description:desc ~confidence:(UI.Confidence.from_string conf)
    with UI.Confidence.Not_confidence s ->
      failwith
        ("Config error: Confidence must be low, moderate, or high, not "^s) in
  let t = UI.set_limits limit t in
  let t = UI.set_keys keys t in
  let t = UI.set_comments comments t in
  let t = UI.set_options options t in
  let t = UI.set_authors authors t in
  UI.set_url url t

(* ------------------------------------------------------------------------- *)
(* ENTRY POINT *)

let parse_local ~rule_names ~config_name =
  let rule_check x =
    let x =
      if Globals.starts_with_digit x then "rule starting on line "^ x else x in
    if (List.mem x rule_names) then x
    else failwith ("Config error: no */+/- rule called \"" ^ x ^ "\".") in
  let (d,l,k,c,m,o,a,u,r) = parse config_name in
  let t = make d l k c m o a u in
  let t = add_rules rule_check r t in
  t

let parse_default = UI.make
  ~description:"No description."
  ~confidence:(UI.Confidence.from_string "moderate")

(* TODO: implement *)
let parse_global ~config_name =
  (None, None, "rule_", "j", "found a match around here ...", 80)
