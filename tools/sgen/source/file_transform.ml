module Ast0 = Ast0_cocci

(* ------------------------------------------------------------------------- *)

(* Transforms the original Coccinelle script and prints it.
 * Prints preface and added virtual rules.
 *
 * Naming conventions: outch = out_channel, inch = in_channel.
 *
 * TODO: There are a number of edge cases that are not handled well in this
 * module due to using pure string-matching without context.
 * Example: @'s inside comments within rule declarations.
 * For most _reasonable_ SmPL scripts, this shouldn't be a problem though.
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

exception Eof_error of string

let fail_eof name =
  let errmsg = "Error: Reached end of file before rule "^name^" was found." in
  raise (Eof_error errmsg)


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

let match_full rule_name =
  regex_match ("^@"^spcmnt_re^(escape rule_name)^"\\(@\\|"^spp_re^".*@\\)")

let match_part rule_name =
  regex_match ("^@"^spcmnt_re^(escape rule_name)^"\\("^spp_re^".*\\)?$")

let match_end = regex_match ".*@"

let match_nameless_rule = regex_match "\\(^\\(@@\\)\\|^@.*@$\\)"

let match_rule_start = regex_match ("^@")

let match_rule_start_arob = regex_match ("^@"^spcmnt_re^"$")

let match_rule_end = regex_match (spcmnt_re^"@@")

let match_non_empty = regex_match (spcmnt_re^"[^ \t]")


(* ------------------------------------------------------------------------- *)
(* IN_CHANNEL TRAVERSAL *)

let rec find_match ~do_this ~until inch =
  let line = get_line inch in
  if until line then (line, inch)
  else begin
    do_this line;
    find_match ~do_this ~until inch
  end

let rec find_line ~do_this ~until_line inch =
  find_match ~do_this ~until:(fun _ -> until_line = !line_number) inch

(* upon a call to regex string matching, print what follows after the match *)
let print_rest outch line =
  let i = Str.match_end() in
  let length = String.length line in
  let rest = String.sub line i (length - i) in
  if i <> length then print_nl outch rest

(* prints the contents of the opened channel until finishes *)
let rec print_to_end outch inch =
  (try
    print_nl outch (get_line inch)
  with
    End_of_file -> (print_newline outch; raise End_of_file));
  print_to_end outch inch

(* goes through the file, printing it as it goes, until finding the rule
 * declaration of name, without printing the rule declaration.
 * returns the line where the rule dec ends, and the in_channel at that stage.
 *)
let skip_rule_dec name outch inch =

  let rec traverse outch inch =
    let line = get_line inch in
    if match_full name line then (* "@rulename@" *)
      (line, inch)
    else if match_part name line then (* "@rulename" *)
      find_match ~do_this:nothing ~until:match_end inch
    else if match_rule_start_arob line then (* "@", next line maybe rulename *)
      let (line,inch) =
        find_match ~do_this:nothing ~until:match_non_empty inch in
      if regex_match (sp_re^name) line then
        find_match ~do_this:nothing ~until:match_end inch
      else begin
        print_nl outch ("@"^line);
        traverse outch inch
      end
    else begin (* line does not contain rule dec *)
      print_nl outch line;
      traverse outch inch
    end in

  traverse outch inch


(* ------------------------------------------------------------------------- *)
(* PATCH SPECIFIC *)

(* outputs the rule declaration with standard patch dependencies.
 * rule_name is the new name which overrules the one in the Ast0 rule.
 *)
let print_patch_decl outch rule_name = function
  | Ast0.InitialScriptRule (nm,_,_,_,_)
  | Ast0.FinalScriptRule (nm,_,_,_,_)
  | Ast0.ScriptRule (nm,_,_,_,_,_) ->
      failwith ("Error: The rule " ^ nm ^ " is a script rule ...!")
  | Ast0.CocciRule ((_,_,(isos,drop_isos,deps,_,exists)),_,_) ->
      let deps = Globals.add_patch_dependency deps in
      let patch_header = Rule_header.generate
        ~isos ~drop_isos ~deps ~rule_name ~exists ~meta_vars:[] ~meta_pos:[] in
      Rule_header.print_declaration outch patch_header

(* prints the file until the declaration of the rule, which is then substituted
 * with whatever handler does.
 *)
let print_named_rule ~rule ~handler ~outch ~inch =
  let name = Ast0.get_rule_name rule in
  let (line,inch) = skip_rule_dec name outch inch in
  handler line inch

(* prints the file until the rule declaration (rule_name must follow the format
 * "rule starting on line <num>"), which is substituted with whatever handler
 * does.
 *)
let print_nameless_rule ~rule ~handler ~outch ~inch =
  let rule_name = Ast0.get_rule_name rule in
  let rule_line = Globals.extract_line rule_name in
  let _ = assert (rule_line > !line_number) in
  let (line, inch) =
    find_line ~do_this:(print_nl outch) ~until_line:rule_line inch in

  if match_nameless_rule line then
    handler line inch
  else if String.contains line '@' then
    let (line, inch) = find_match ~do_this:nothing ~until:match_end inch in
    handler line inch
  else
    failwith ("Error: Did not find a " ^rule_name^ ", instead found: " ^line)

(* Finds the declaration of the input rule ("@rulename ...@") and substitutes
 * it with a patch dependent version ("@rulename depends on patch ...@").
 *)
let print_rule_patch outch inch (rule, new_name) =

  (* prints out patch header. If there was anything after the rule declaration,
   * print that too. returns the in_channel at the point after the printing.
   *)
  let handler line inch =
    print_patch_decl outch new_name rule;
    print_rest outch line;
    inch in

  let old_name = Ast0.get_rule_name rule in

  try
    if old_name = new_name then
       print_named_rule ~rule ~handler ~outch ~inch
    else
       print_nameless_rule ~rule ~handler ~outch ~inch
  with
    | End_of_file -> fail_eof old_name
    | e -> raise e (* propagate exception upwards *)


let print_patch outch inch rules =
  let inch = List.fold_left (print_rule_patch outch) inch rules in
  print_to_end outch inch


(* ------------------------------------------------------------------------- *)
(* CONTEXT SPECIFIC *)

(* in context mode, we do not want to keep the original rules, since our
 * generated versions contain the same information + added metapositions. *)

(* find the start of the next rule. *)
let next outch inch =
  try
    let r = find_match ~do_this:nothing ~until:match_rule_start inch in Some r
  with
    End_of_file -> None (* there were no more rules *)

(* find the rule with that name and skip it entirely. *)
let skip_named_rule ~rule ~last_line ~outch ~inch =
  let name = Ast0.get_rule_name rule in
  let (_,inch) =
    if match_part name last_line
    then (last_line, inch)
    else begin
      if String.contains last_line '@' then failwith
        ("Transform error: Can't currently handle this case. Don't " ^
         "put newlines in your rule declarations!!!");
      print_nl outch last_line;
      skip_rule_dec name outch inch
    end in
  let (_,inch) = find_match ~do_this:nothing ~until:match_rule_end inch in
  next outch inch

(* find the rule that starts on that line and skip it entirely *)
let skip_nameless_rule ~rule ~outch ~inch =
  let name = Ast0.get_rule_name rule in
  let rule_line = Globals.extract_line name in
  let _ = assert (rule_line >= !line_number) in
  let inch =
    if rule_line = !line_number then
      inch
    else
      snd (find_line ~do_this:(print_nl outch) ~until_line:rule_line inch) in

  (* at this point, line is the line that contains the rule header. so we need
   * the rule header end @@ and then the start of the next rule.
   *)
  let (_,inch) = find_match ~do_this:nothing ~until:match_rule_end inch in
  next outch inch

(* print a context rule (that is, don't print it, but find it and skip it!)
 * last_res is (the last line contents, the in_channel) from the previous call.
 *
 * returns Some (last line contents, in_channel) if there was another rule
 * after the input rule, otherwise None.
 *)
let print_rule_context outch last_res (rule, new_name) =

  let old_name = Ast0.get_rule_name rule in

  try
    match last_res with
    | None -> raise End_of_file
    | Some (last_line, inch) ->
        if old_name = new_name then
          skip_named_rule ~rule ~last_line ~outch ~inch
        else
          skip_nameless_rule ~rule ~outch ~inch
  with
    | End_of_file -> fail_eof old_name
    | e -> raise e (* propagate exception upwards *)

let print_context outch inch rules =
  let res = List.fold_left (print_rule_context outch) (Some ("",inch)) rules in
  match res with
    | Some (_,i) -> print_to_end outch i
    | None -> raise End_of_file

(* ------------------------------------------------------------------------- *)
(* ENTRY POINT *)

(* reads the file and prints it with transformations.
 * assumes rules are sorted in order of when they occur in the script.
 *)
let print ~context_mode ~file_name ~preface ~virtuals ~ordered_rules outch =
  let _ = line_number := 0 in
  let _ = print_nl outch preface in
  let _ = print_virtuals outch virtuals in
  let inch = open_in file_name in
  try
    if context_mode then
      print_context outch inch ordered_rules
    else
      print_patch outch inch ordered_rules
  with
    | End_of_file -> flush outch; close_in inch (* ended safely *)
    | Eof_error msg -> flush outch; close_in inch; failwith msg
    | e -> close_in_noerr inch; raise e
