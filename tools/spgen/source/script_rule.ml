(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

module MV = Meta_variable
module UI = User_input

(* ------------------------------------------------------------------------- *)

(* Generate script rules for org and report. *)

(* ------------------------------------------------------------------------- *)
(* TYPES AND HELPERS *)

type t = string list (*org*) * string list (*rep*)

let comma_sep = String.concat ","

(* invariant: always at least one position *)
let split_pos = function mv::mvs -> (mv,mvs) | _ -> assert false

(* print helpers for script rules (which are really just string lists) *)
let print_newl out = output_string out "\n"
let printfn out x =
  List.iter (fun x -> output_string out x; print_newl out) x;
  print_newl out

(* remove period at the end of the message if there is one (prevent doubles) *)
let remove_period msg =
  let len_but_one = String.length msg - 1 in
  let last = msg.[len_but_one] in
  if last = '.' then String.sub msg 0 len_but_one else msg

(* ------------------------------------------------------------------------- *)
(* CONSTANTS *)

let print_todo_fn = "coccilib.org.print_todo"
let print_safe_todo_fn = "coccilib.org.print_safe_todo"
let print_link_fn = "coccilib.org.print_link"
let print_report_fn = "coccilib.report.print_report"


(* ------------------------------------------------------------------------- *)
(* SCRIPT GENERATION FUNCTIONS *)

(* Format the variables used in the format string.
 * Returns ("around lines" string for metapos's, comma-separated mv names)
 *)
let line_vars ~meta_pos ~meta_vars =
  let line = List.map (fun x -> x ^ "[0].line") in
  match meta_pos with
   | [] -> ("", comma_sep meta_vars)
   | [x] -> ("around line %s.", comma_sep (meta_vars @ (line [x])))
   | x -> let agg = comma_sep (List.map (fun _ -> "%s") x) in
       ("around lines " ^ agg ^ ".", comma_sep (meta_vars @ (line x)))

(* turn metavariables into script header variables *)
let format_header_vars =
  let binding mv =
    let (rn,nm) = (MV.get_rule mv, MV.get_name mv) in
    nm ^ " << " ^ rn ^ "." ^ nm ^ ";" in
  List.map binding

(* only include metavars/positions in the format string if more than one *)
let format_err_msg err_msg mpnames mvnames =
  if (List.length mvnames + List.length mpnames) > 0 then
    let (linepos, formatvars) =
      line_vars ~meta_pos:mpnames ~meta_vars:mvnames in
    "msg = \"" ^ err_msg ^ " " ^ linepos ^ "\" % (" ^ formatvars ^ ")"
  else
    "msg = \"" ^ err_msg ^ ".\""

(* assembles an org script rule. *)
let gen_org_rule nm (firstpos, restpos) metavars err_msg =

  (* if there are metavars, they might contain brackets which conflict with
   * the todo format. In that case, use safe mode (replaces brackets).
   *)
  let printfn = if metavars <> [] then print_safe_todo_fn else print_todo_fn in
  let new_rulenm = Globals.get_org_name nm in
  let headervars = format_header_vars (metavars @ (firstpos :: restpos)) in
  (* the error message is used as is, positions are inserted in print calls *)
  let metavars = List.map MV.get_name metavars in
  let err_msg = format_err_msg err_msg [] metavars in
  let zero p = (MV.get_name p) ^ "[0]" in
  [
   (*header*)
   Globals.concat_limit_width
    ["@script:python "; new_rulenm; " depends on org@"];
   String.concat "\n" headervars;
   "@@\n";
   (*body*)
   err_msg;
   (printfn ^ "(" ^ (zero firstpos) ^ ", msg)")
  ] @ (List.map (fun x -> print_link_fn ^ "(" ^ (zero x) ^ ", \"\")") restpos)

(* assembles a report script rule. *)
let gen_report_rule nm (firstpos, restpos) metavars err_msg =
  let new_rulenm = Globals.get_report_name nm in
  let headervars = format_header_vars (metavars @ (firstpos :: restpos)) in
  let firstpos = (MV.get_name (firstpos)) ^ "[0]" in
  let restpos = List.map MV.get_name restpos in
  let metavars = List.map MV.get_name metavars in
  let err_msg = format_err_msg err_msg restpos metavars in
  [
   (*header*)
   Globals.concat_limit_width
    ["@script:python "; new_rulenm; " depends on report@"];
   String.concat "\n" headervars;
   "@@\n";
   (*body*)
   err_msg;
   (print_report_fn ^ "(" ^ firstpos ^ ", msg)")
  ]


(* ------------------------------------------------------------------------- *)
(* ENTRY POINT *)

(* generate org and report rule for the added metapositions and with the user
 * specified information (error messages).
 *)
let generate ~meta_pos ~user_rule =

  (* extract user-specified data from rule *)
  let nm = UI.Rule.get_name user_rule in
  let org_msg, omv = UI.Rule.get_org user_rule in
  let report_msg, rmv = UI.Rule.get_report user_rule in
  let org_msg, report_msg = remove_period org_msg, remove_period report_msg in

  (* find the first position, fails if the meta_pos list is empty *)
  let firstpos, restpos = split_pos meta_pos in
  let new_rule = MV.get_rule firstpos in

  (* make sure user-specified metavars are inherited from the context rule *)
  let omv = List.map (MV.inherit_rule ~new_rule) omv in
  let rmv = List.map (MV.inherit_rule ~new_rule) rmv in

  (* generate org and report script rules *)
  let org = gen_org_rule nm (firstpos, restpos) omv org_msg in
  let report = gen_report_rule nm (firstpos, restpos) rmv report_msg in
  org, report

let print_org out (org, _) = printfn out org

let print_report out (_, rep) = printfn out rep
