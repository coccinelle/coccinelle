module Ast0 = Ast0_cocci

(* ------------------------------------------------------------------------- *)

(* Transforms the original Coccinelle script and prints it.
 * Prints preface and added virtual rules.
 *
 * In patch mode, transformations include:
 *   - adding rule names to previously unnamed rules
 *   - adding standard dependencies to existing patch rules
 *
 * In context mode, transformations include:
 *   - skipping all original context rules, since we now have the same rules
 *     but in a generated (and therefore superior!) version.
 *
 * The transformation is done alongside the printing so if anything fails,
 * some of it might already have been printed.
 * Naming conventions: outch = out_channel, inch = in_channel.
 *)

(* ------------------------------------------------------------------------- *)
(* GENERAL PURPOSE FUNCTIONS *)

let line_number = ref 0

let get_line inch = line_number := !line_number + 1; input_line inch

let nothing _ = ()
let print = output_string
let print_newline outch = output_string outch "\n"
let print_nl outch x = print outch x; print_newline outch

let print_virtuals outch virtuals =
  print_newline outch;
  List.iter (fun x -> print_nl outch ("virtual " ^ x)) virtuals;
  print_newline outch


(* ------------------------------------------------------------------------- *)
(* REGEXES AND STRING MATCH FUNCTIONS *)

(* returns true if str matches the regular expression in regexp *)
let regex_match regex str = Str.string_match (Str.regexp regex) str 0

(* regex for any number of same-line whitespace *)
let sp_re = "[ \t]*"

(* regex for at least one space or tab *)
let spp_re = "[ \t]+"

(* regex for any number of /**/ comments *)
let cmnt_re = "\\(" ^ sp_re ^ "/\\*.*\\*/" ^ sp_re ^ "\\)*"

(* regex for any number of /**/ comments with arbitrary whitespace *)
let spcmnt_re = sp_re ^ cmnt_re ^ sp_re

(* we have to handle many cases since it is technically possible to have
 * comments and large amounts of whitespace in rule header declarations.
 * If someone actually writes a script like this, they should be punished.
 *)
let escape = Str.global_replace (Str.regexp "\\$") "\\\\$"
let match_full rulename =
  regex_match ("^@"^spcmnt_re^(escape rulename)^"\\(@\\|"^spp_re^".*@\\)")
let match_part rulename =
  regex_match ("^@"^spcmnt_re^(escape rulename)^"\\("^spp_re^".*\\)?$")
let match_end = regex_match ".*@"
let match_nameless_rule = regex_match "\\(^\\(@@\\)\\|^@.*@$\\)"
let match_rule_start = regex_match "^@"
let match_rule_end = regex_match (spcmnt_re^"@@")
let match_non_empty = regex_match (spcmnt_re^"[^ \t]")


(* ------------------------------------------------------------------------- *)
(* IN_CHANNEL TRAVERSAL *)

(* just perform dofn function on line until a line which matches is found *)
let rec find_match dofn matchfn inch =
  let line = get_line inch in
  if matchfn line then (line, inch)
  else (dofn line; find_match dofn matchfn inch)

(* just perform dofn function on line until reaching the desired line number *)
let rec find_line dofn rule_line inch =
  let line = get_line inch in
  if rule_line = !line_number then (line, inch)
  else (dofn line; find_line dofn rule_line inch)

(* upon a call to regex string matching, print what follows after the match *)
let print_rest outch line =
  let i = Str.match_end() in
  let length = String.length line in
  let rest = String.sub line i (length - i) in
  if i <> length then print_nl outch rest

(* prints the contents of the opened channel until finishes, closes channel *)
let rec print_to_end outch inch =
  try
    print_nl outch (get_line inch);
    print_to_end outch inch
  with
    | End_of_file -> print_newline outch; flush outch; close_in inch
    | e -> close_in_noerr inch; raise e

(* goes through the file until finding the rule declaration of name.
 * returns the line where the rule dec ends, and the inchannel at that stage *)
let traverse name outch inch =
  let rec traverse' outch inch =
    let line = get_line inch in
    if match_full name line then
      (line, inch)
    else if match_part name line then
      find_match nothing match_end inch
    else if match_rule_start line && not(match_rule_end line) then
      let (line,inch) = find_match nothing match_non_empty inch in
      if regex_match (sp_re^name) line then
        find_match nothing match_end inch
      else (print_nl outch ("@"^line); traverse' outch inch)
    else
      (print_nl outch line; traverse' outch inch) in
  traverse' outch inch


(* ------------------------------------------------------------------------- *)
(* PATCH SPECIFIC *)

(* outputs the rule declaration with standard patch dependencies (hardcoded) *)
let print_patch_decl outch newnm = function
  | Ast0.InitialScriptRule (nm,_,_,_,_)
  | Ast0.FinalScriptRule (nm,_,_,_,_)
  | Ast0.ScriptRule (nm,_,_,_,_,_) ->
      failwith ("Error: The rule " ^ nm ^ " is a script rule ...!")
  | Ast0.CocciRule ((_,_,(isos,dropisos,deps,rulename,exists)),_,_) ->
      let rulename = (match newnm with | Some n -> n | None -> rulename) in
      let patch_header = Rule_header.generate_patch
        ~isos ~dropisos ~deps ~rulename ~exists ~meta_vars:[] ~meta_pos:[] in
      Rule_header.print_declaration outch patch_header

(* prints the file until the declaration of the rule, which is then substituted
 * with a patch dependent header *)
let print_named_rule ~rule ~name ~handler ~outch ~inch =
  let (line,inch) = traverse name outch inch in
  handler rule None line inch

(* prints the file until the declaration of the rule, which is then substituted
 * with a patch dependent header + new name *)
let print_nameless_rule ~rule ~name ~new_nm ~handler ~outch ~inch =
  let rule_line = Globals.extract_line name in
  let _ = assert (rule_line > !line_number) in
  (* found the line that the rule -allegedly- is starting on *)
  let (line, inch) = find_line (print_nl outch) rule_line inch in
  if match_nameless_rule line then
    handler rule (Some new_nm) line inch
  else if String.contains line '@' then
    let (line, inch) = find_match nothing match_end inch in
    handler rule (Some new_nm) line inch
  else failwith ("Error: Did not find a rule starting on line " ^
    (string_of_int rule_line) ^ ", instead found: " ^ line)

(* Finds the rule declaration of 'rule' and substitutes it with a patch
 * dependent version. *)
let print_rule_patch outch inch (rule, new_name) =
  (* prints out patch header. If there was anything after the rule declaration,
   * print that too. returns the in channel *)
  let handler rule new_name line inch =
    print_patch_decl outch new_name rule; print_rest outch line; inch in
  let name = Ast0.get_rule_name rule in
  try
    match new_name with
    | None -> print_named_rule ~rule ~name ~handler ~outch ~inch
    | Some n -> print_nameless_rule ~rule ~name ~new_nm:n ~handler ~outch ~inch
  with
    | End_of_file -> flush outch; close_in inch;
        failwith ("Error: Reached end of file before rule " ^ name ^
        " was found.")
    | e -> close_in_noerr inch; raise e

let print_patch outch inch rules =
  let inch = List.fold_left (print_rule_patch outch) inch rules in
  print_to_end outch inch


(* ------------------------------------------------------------------------- *)
(* CONTEXT SPECIFIC *)

(* in context mode, we do not want to keep the original rules, since our
 * generated versions contain the same information + added metapositions. *)

(* find the start of the next rule; if there is none, close the channel. *)
let next outch inch =
  try let res = find_match nothing match_rule_start inch in Some res
  with End_of_file -> flush outch; close_in inch; None

(* find the rule with that name and skip it entirely *)
let skip_named_rule ~rule ~name ~last_line ~outch ~inch =
  let (line,inch) =
    if match_part name last_line then (last_line, inch)
    else begin
      if String.contains last_line '@' then failwith
        ("Transform error: Can't currently handle this case. Don't " ^
         "put newlines in your rule declarations!!!");
      print_nl outch last_line;
      traverse name outch inch
    end in
  let (_,inch) = find_match nothing match_rule_end inch in
  next outch inch

(* find the rule that starts on that line and skip it entirely *)
let skip_nameless_rule ~rule ~name ~last_line ~outch ~inch =
  let rule_line = Globals.extract_line name in
  let _ = assert (rule_line >= !line_number) in
  let (_,inch) =
    if rule_line = !line_number then (last_line, inch)
    else find_line (fun x -> ()) rule_line inch in
  (*currently, line is the line that contains the rule header. so we need
   *the rule header end @@ and then the start of the new rule. *)
  let (_,inch) = find_match nothing match_rule_end inch in
  next outch inch

(* print a context rule (that is, don't print it, but find it and skip it!) *)
let print_rule_context outch lastres (rule, new_name) =
  let name = Ast0.get_rule_name rule in
  let (last_line, inch) = match lastres with
    | Some (l,i) -> (l,i)
    | None -> failwith ("Error: Reached end of file before rule " ^ name ^
        " was found.") in
  try
    match new_name with
    | None -> skip_named_rule ~rule ~name ~last_line ~outch ~inch
    | Some n -> skip_nameless_rule ~rule ~name ~last_line ~outch ~inch
  with
    | End_of_file -> flush outch; close_in inch;
        failwith ("Error: Reached end of file before rule " ^ name ^
        " was found.")
    | e -> close_in_noerr inch; raise e

let print_context outch inch rules =
  match List.fold_left (print_rule_context outch) (Some("",inch)) rules with
  | Some (l,i) -> print_to_end outch i
  | None -> ()


(* ------------------------------------------------------------------------- *)
(* ENTRY POINT *)

(* reads the file and prints it with transformations.
 * assumes rules are sorted in order of when they occur in the script. *)
let print ~channel ~file_name ~preface ~virtuals ~rules ~context_mode =
  line_number := 0;
  print_nl channel preface;
  print_virtuals channel virtuals;
  let inch = open_in file_name in
  if context_mode then
    print_context channel inch rules
  else
    print_patch channel inch rules
