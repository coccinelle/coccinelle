module UI = User_input
module Lex = Sgen_lexer

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
 * if several org/report messages defined, take the last one *)
let parse_msgs attributes =
  let read ((om,ov),(rm,rv)) = function
    | Lex.Org (om,ov) -> ((om,ov), (rm,rv))
    | Lex.Report (rm,rv) -> ((om,ov), (rm,rv)) in
  List.fold_left read (("",[]), ("",[])) attributes

(* add the rule to the User_input.t
 * rule_exists is a function that checks whether a rule exists*)
let add_rule rule_exists t ((oldnm,newnm),a) =
  let oldrnm = rule_exists oldnm in
  let newrnm = if newnm = None then Globals.generate_rule oldrnm else newnm in
  let (org,rep) = match parse_msgs a with
    | ("",_),("",_) ->
        failwith ("Config error: must specify at least org or report message.")
    | ("",_),mgv | mgv,("",_) ->
        (try (UI.check_format_string mgv; (mgv,mgv))
        with Failure msg -> failwith ("Config error: " ^ msg))
    | org, rep ->
        try UI.check_format_string org; UI.check_format_string rep; (org, rep)
        with Failure msg -> failwith ("Config error: " ^ msg) in
  UI.add_rule ((oldrnm, newrnm),org,rep) t

(*rule list is a list of (rulename, attribute list) *)
let add_rules rule_exists rule_list t =
  List.fold_left (fun a b -> add_rule rule_exists a b) t rule_list

(* expanded constructor for User_input.t *)
let make desc limit keys conf comments options authors url =
  let t =
    try UI.make ~description:desc ~confidence:(UI.conf_fromstring conf)
    with Failure msg -> failwith ("Config error: " ^ msg) in
  UI.set_limits limit
    (UI.set_keys keys
      (UI.set_comments comments
        (UI.set_options options
          (UI.set_authors authors
            (UI.set_url url t)))))


(* ------------------------------------------------------------------------- *)
(* ENTRY POINT *)

let parse_local ~ordered_rules ~config_name =
  let rule_exists x =
    let x =
      if Globals.starts_with_digit x then "rule starting on line "^ x
      else x in
    if (List.mem x ordered_rules) then x
    else failwith ("Config error: no */+/- rule called \"" ^ x ^ "\".") in
  let (d,l,k,c,m,o,a,u,r) = parse config_name in
  let t = make d l k c m o a u in
  let t = add_rules rule_exists r t in
  let preface = UI.get_preface t in
  let rules = UI.get_rules ordered_rules t in
  (preface, rules)

let parse_default ~ordered_rules =
  let t = UI.make ~description:"No description."
    ~confidence:(UI.conf_fromstring "moderate") in
  let preface = UI.get_preface t in
  let rules = UI.get_rules ordered_rules t in
  (preface, rules)

(*TODO*)
let parse_global ~config_name =
  (None, None, "rule_", "j", "found a match around here ...", 80)
