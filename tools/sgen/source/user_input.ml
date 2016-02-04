module M = Meta_variable
module RuleMap = Map.Make (String)

(* ------------------------------------------------------------------------- *)

(* Encapsulates all local input for the rule to be generated:
 * * Description of the script.
 * * Limitations of the script.
 * * Keywords for the script.
 * * Confidence level of the script (Low, Moderate, High).
 * * Additional comments for the script.
 * * Coccinelle options with which to call the script.
 * * Authors of the script (copyright). TODO: make globally configurable.
 * * Error messages for org and report mode for each rule.
 *)

(* ------------------------------------------------------------------------- *)
(* HELPERS *)

let opt s = if s = "" then None else Some s
let rev_opt p = function Some s -> p ^ s ^ "\n" | None -> ""

(* Count number of format variables, effectively number of unescaped %'s. *)
let count_format_vars s =
  (* ignore escaped percent signs, which in python is %%. *)
  let s = Str.global_replace (Str.regexp_string "%%") "" s in
  let rec count_pct acc i s =
    if i < 0 then acc else
    if s.[i] = '%' then count_pct (acc+1) (i-1) s else
    count_pct acc (i-1) s in
  count_pct 0 ((String.length s)-1) s

(* fails if the number of metavariables != the number of % in format str *)
let check_format_string (msg, mvs) =
  let pcts = count_format_vars msg in
  let mvcount = List.length mvs in
  if pcts <> mvcount
  then failwith
   ("Config error: ill-formed format string.\n"
    ^ "Number of format variables in the message \"" ^ msg ^ "\": "
    ^ (string_of_int pcts) ^ "\n"
    ^ "Number of declared format variables: " ^ (string_of_int mvcount))

(* reconstructs a format string from the message and metavars (as strings) *)
let make_format_string msg =
  let msg = "\"" ^ msg ^ "\"" in
  function [] -> msg | x -> msg ^ " % " ^ "(" ^ (String.concat "," x) ^ ")"

(* turn user-specified metavariable strings into metavariables.
 * important that they are created with no rulename if within the same rule,
 * since this is used to generate the right inheritance later on. *)
let make_metavars =
  let mv a =
    let a = Str.bounded_split (Str.regexp "\\.") a 2 in
    match a with
    | [a] -> M.make_metavar a
    | [a;b] -> M.make_metavar ~rulename:a b
    | _ -> failwith "bounded split" in
  List.map mv

(* sort the rules in userinput in the order specified by ordered_rules
 * ordered_rules must all be valid for generation (ie. */+/- rules)
 * if the original rule could not be found in the userinput, use default msg *)
let sort_rules ordered_rules userinput =
  let default = Globals.get_default_message() in
  let format_rule x =
    try
      let (newnm, (om, ov), (rm, rv)) = RuleMap.find x userinput in
      let ov, rv = make_metavars ov, make_metavars rv in
      ((x, newnm), (om, ov), (rm, rv))
    with Not_found ->
      let rulenm = Globals.generate_rule x in
      ((x, rulenm), (default, []), (default, [])) in
  List.map format_rule ordered_rules


(* ------------------------------------------------------------------------- *)
(* CONFIDENCE TYPE *)

(* confidence in the accuracy of the script *)
type confidence = Low | Moderate | High

let conf_tostring =
  function Low -> "Low" | Moderate -> "Moderate" | High -> "High"

let conf_fromstring s =
  match String.lowercase s with
  | "low" | "l" -> Low | "moderate" | "m" -> Moderate | "high" | "h" -> High
  | s -> failwith ("Confidence must be low, moderate, or high, not " ^ s)


(* ------------------------------------------------------------------------- *)
(* USER INPUT TYPE *)

(* user_input type that covers all the data specified by the user
 * rules are a map, mapping rulename to
 * (new rulename, (org message, org metavars), (rep message, rep metavars)
 *)
type t = {
  description : string;
  limitations : string list;
  keywords : string option;
  confidence : confidence;
  comments : string option;
  options : string option;
  authors : string list;
  url : string option;
  rules :
   (string option * (string * string list) * (string * string list)) RuleMap.t;
}

(* CONSTRUCTOR, description and confidence levels are required. *)
let make ~description ~confidence =
  if description = "" then failwith "Error: Description is required." else
  { description; limitations = []; keywords = None; confidence;
    comments = None; options = None; authors = []; url = None;
    rules = RuleMap.empty }

(* SETTERS *)
let add_limit limit t = { t with limitations = limit :: t.limitations }
let set_limits limits t = { t with limitations = limits }
let set_keys keys t = { t with keywords = opt keys }
let set_conf conf t = { t with confidence = conf }
let set_comments cmnt t = { t with comments = opt cmnt }
let set_options optn t = { t with options = opt optn }
let set_url url t = { t with url = opt url }
let add_author auth t = { t with authors = auth :: t.authors }
let set_authors auths t = { t with authors = auths }

let check_name nm t =
  let find a _ = function
    | (Some newnm,_,_) -> nm = newnm | _ -> false in
  if RuleMap.exists (find nm) t.rules
  then failwith ("Error: trying to name two rules \"" ^ nm ^"\"!")
  else Globals.check_rule ~strict:true nm

let add_rule ((rnm,newnm),(om,ov),(rm,rv)) t =
  assert (rnm <> "" && not(om = "" && rm = "")); (*sanity check*)
  let _ = match newnm with (* check if newname, if any, is valid *)
    | Some nm -> check_name nm t | None -> () in
  { t with rules = RuleMap.add rnm (newnm,(om,ov),(rm,rv)) t.rules }

(* GETTERS *)
(* format the preface information and turn it into one big string *)
let get_preface {description=d; limitations=l; keywords=k; confidence=c;
  comments=m; options=o; authors=a; url=u; _} =
  let author_format =
    let year = string_of_int (Globals.get_current_year()) in
    Globals.pre_split ~prefix:("// Copyright: (C) "^year^" ") in
  let desc = Globals.pre_split ~prefix:"/// " d in
  let limits =
    String.concat "\n" (List.map (Globals.pre_split ~prefix:"//# ") l) in
  let keys = Globals.pre_split_opt ~prefix:"// Keywords: " k in
  let confidence = "// Confidence: " ^ (conf_tostring c) in
  let comments = Globals.pre_split_opt ~prefix:"// Comments: " m in
  let options = Globals.pre_split_opt ~prefix:"// Options: " o in
  let authors = String.concat "\n" (List.map author_format a) in
  let url = Globals.pre_split_opt ~prefix:"// URL: " u in
  let preface = [desc; limits; "///"; confidence; authors; url; comments;
    options; keys] in
  String.concat "\n" (List.filter ((<>) "") preface)

(* gets rules from the input ordered according to the original */+/- rules *)
let get_rules ~ordered_rules {rules=r; _} = sort_rules ordered_rules r


(* ------------------------------------------------------------------------- *)
(* UNPARSE USER INPUT INTO CONFIG *)

(* turns a rule into the config script that generated it *)
let unparse_rule rnm (newnm,(orgmsg,orgmvs),(repmsg,repmvs)) =
  let orgmsg = make_format_string orgmsg orgmvs in
  let repmsg = make_format_string repmsg repmvs in
  let rnm = match newnm with
    | Some nm ->
        let l = Globals.extract_line rnm in (string_of_int l) ^ ":" ^ nm
    | None -> rnm in
  rnm ^ " =\n" ^ "  org:" ^ orgmsg ^ "\n  report:" ^ repmsg ^ "\n"

(* turn a user input collection into its corresponding config script *)
let unparse
  { description; limitations; keywords; confidence; comments; options; rules;
    authors; url } =
  let a = "// Generated config\n" in
  let b = "description = " ^ description ^ "\n" in
  let c = if limitations = [] then "" else
    "limitations = " ^ (String.concat "|" limitations) ^ "\n" in
  let d = rev_opt "keywords = " keywords in
  let e = "confidence = " ^ (conf_tostring confidence) ^ "\n" in
  let f = rev_opt "comments = " comments in
  let g = rev_opt "options = " options in
  let h = if authors = [] then "" else
    "authors = " ^ (String.concat "|" authors) ^ "\n" in
  let i = rev_opt "url = " url in
  let j = RuleMap.fold
    (fun rnm msg acc -> (unparse_rule rnm msg) :: acc) rules [] in
  let preface = String.concat "" [a;b;c;d;e;f;g;h;i] in
  let rules = String.concat "" j in
  preface ^ rules
