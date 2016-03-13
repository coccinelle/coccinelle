(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

module MV = Meta_variable
module RuleMap = Map.Make (String)

(* ------------------------------------------------------------------------- *)

(* Encapsulates all local input for the rule to be generated:
 *  - Description of the script.
 *  - Limitations of the script.
 *  - Keywords for the script.
 *  - Confidence level of the script (Low, Moderate, High).
 *  - Additional comments for the script.
 *  - Coccinelle options with which to call the script.
 *  - Authors of the script (copyright).
 *    Not yet globally configurable; see spgen_config.ml.
 *  - Error messages for org and report mode for each rule.
 *)

(* ------------------------------------------------------------------------- *)
(* HELPERS FOR USER INPUT *)

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
  function
    | [] -> msg
    | x ->
        let x = List.map MV.tostring_mv x in
        msg ^ " % " ^ "(" ^ (String.concat "," x) ^ ")"

(* turn user-specified metavariable strings into metavariables.
 * important that they are created with no rulename if within the same rule,
 * since this is used to generate the right inheritance later on.
 * They are initialised with type = "" since we don't need it.
 *)
let make_metavars =
  let mv a =
    let split_name = Str.bounded_split (Str.regexp "\\.") a 2 in
    match split_name with
    | [meta_name] -> MV.make ~typ:"" meta_name
    | [inherit_rule; meta_name] -> MV.make ~typ:"" ~inherit_rule meta_name
    | _ -> failwith "bounded split" in
  List.map mv


(* ------------------------------------------------------------------------- *)
(* CONFIDENCE TYPE *)

(* confidence in the accuracy of the script *)

module Confidence = struct
  type t =  Low | Moderate | High

  exception Not_confidence of string

  let to_string =
    function Low -> "Low" | Moderate -> "Moderate" | High -> "High"

  let from_string s =
    match String.lowercase s with
    | "low" | "l" -> Low
    | "moderate" | "m" -> Moderate
    | "high" | "h" -> High
    | s -> raise (Not_confidence s)
end


(* ------------------------------------------------------------------------- *)
(* USER RULE TYPE *)

(* user-specified data to put in spgenerated script.
 * type is (rulename, (org message, org metavars), (rep message, rep metavars))
 * rulename can be original rulename or a new user-specified one.
 *)

module Rule = struct
  type t = string * (string * MV.t list) * (string * MV.t list)

  (* constructor. If rulename is nameless ie. "rule starting on line ...",
   * generate a new one.
   *)
  let make ~rule_name ~org ~report =
    let ((om,ov),(rm,rv)) = (org, report) in
    let _ = assert (rule_name <> "" && not(om = "" && rm = "")) in
    let rule_name =
      if String.contains rule_name ' '
      then Globals.generate_rule rule_name
      else rule_name in
    let ov, rv = make_metavars ov, make_metavars rv in
    (rule_name, (om,ov), (rm,rv))

  let get_name (n,_,_) = n

  let get_org (_,o,_) = o

  let get_report (_,_,r) = r
end


(* ------------------------------------------------------------------------- *)
(* USER INPUT TYPE *)

(* user_input type that covers all the data specified by the user
 * rules are a map, mapping original rulename to
 * (new rulename, (org message, org metavars), (rep message, rep metavars)
 *)
type t =
{
  description : string;
  limitations : string list;
  keywords : string option;
  confidence : Confidence.t;
  comments : string option;
  options : string option;
  authors : string list;
  url : string option;
  rules : Rule.t RuleMap.t;
}

(* CONSTRUCTOR, description and confidence levels are required. *)
let make ~description ~confidence =
  if description = ""
  then failwith "Error: Description is required."
  else
    {
      description; limitations = []; keywords = None; confidence;
      comments = None; options = None; authors = []; url = None;
      rules = RuleMap.empty
    }

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

(* check that rulename is valid + is not already added. Returns unit. *)
let check_name nm t =
  let already key value =
    let new_name = Rule.get_name value in
    (key = new_name && key = nm) || (* nm is same as already added rule *)
    (new_name = nm) in              (* nm is same as other rule with new nm *)
  if RuleMap.exists already t.rules then
    failwith ("Error: already another rule named \"" ^ nm ^"\"!")
  else
    Globals.check_rule ~strict:true nm

(* add rule to rulemap in t.
 * for nameless rules: check legality of user-declared name.
 *)
let add_rule ~rule_name rule t =
  let newnm = Rule.get_name rule in
  let _ = if rule_name <> newnm then check_name newnm t in
  { t with rules = RuleMap.add rule_name rule t.rules }

(* GETTERS *)
(* format the preface information and turn it into one big string *)
let get_preface ~year
  {
    description=d; limitations=l; keywords=k; confidence=c;
    comments=m; options=o; authors=a; url=u; _
  } =

  let author_format =
    let year_string = string_of_int year in
    Globals.pre_split ~prefix:("// Copyright: (C) "^year_string^" ") in
  let desc = Globals.pre_split ~prefix:"/// " d in
  let limits =
    String.concat "\n" (List.map (Globals.pre_split ~prefix:"//# ") l) in
  let keys = Globals.pre_split_opt ~prefix:"// Keywords: " k in
  let confidence = "// Confidence: " ^ (Confidence.to_string c) in
  let comments = Globals.pre_split_opt ~prefix:"// Comments: " m in
  let options = Globals.pre_split_opt ~prefix:"// Options: " o in
  let authors = String.concat "\n" (List.map author_format a) in
  let url = Globals.pre_split_opt ~prefix:"// URL: " u in
  let preface = [desc; limits; "///"; confidence; authors; url; comments;
    options; keys] in
  String.concat "\n" (List.filter ((<>) "") preface)

(* get the user-input rulename and org and report messages for rule_name.
 * if the original rule could not be found in the userinput, use default msg
 *)
let get_rule ~rule_name {rules = r; _} =
  try
    RuleMap.find rule_name r
  with Not_found ->
    let rulenm = Globals.generate_rule rule_name in
    let default = Globals.get_default_message() in
    (rulenm, (default, []), (default, []))


(* ------------------------------------------------------------------------- *)
(* UNPARSE USER INPUT INTO CONFIG *)

(* turns a rule into the config script that generated it *)
let unparse_rule rnm (newnm,(orgmsg,orgmvs),(repmsg,repmvs)) =
  let orgmsg = make_format_string orgmsg orgmvs in
  let repmsg = make_format_string repmsg repmvs in
  let rnm =
    if rnm <> newnm then
      let l = Globals.extract_line rnm in (string_of_int l) ^ ":" ^ newnm
    else
      rnm in
  rnm ^ " =\n" ^ "  org:" ^ orgmsg ^ "\n  report:" ^ repmsg ^ "\n"

(* turn a user input collection into its corresponding config script *)
let unparse
  {
    description; limitations; keywords; confidence; comments; options; rules;
    authors; url
  } =

  let a = "// Generated config\n" in
  let b = "description = " ^ description ^ "\n" in
  let c = if limitations = [] then "" else
    "limitations = " ^ (String.concat "|" limitations) ^ "\n" in
  let d = rev_opt "keywords = " keywords in
  let e = "confidence = " ^ (Confidence.to_string confidence) ^ "\n" in
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
