module M = Meta_variable

(* ------------------------------------------------------------------------- *)

(* Generate script rules for org and report. *)

(* ------------------------------------------------------------------------- *)
(* TYPES AND HELPERS *)

type t = string list (*org*) * string list (*rep*)

let comma_sep = String.concat ","

(* invariant: always at least one position *)
let split_pos = function mv::mvs -> (mv,mvs) | _ -> assert false

(* print helpers for script rules (which are really just string lists) *)
let print_newl outch = output_string outch "\n"
let printfn outch x =
  List.iter (fun x -> output_string outch x; print_newl outch) x;
  print_newl outch


(* ------------------------------------------------------------------------- *)
(* CONSTANTS *)

let print_todo_fn = "coccilib.org.print_todo"
let print_safe_todo_fn = "coccilib.org.print_safe_todo"
let print_link_fn = "coccilib.org.print_link"
let print_report_fn = "coccilib.report.print_report"


(* ------------------------------------------------------------------------- *)
(* SCRIPT GENERATION FUNCTIONS *)

(* Format the variables used in the format string.
 * Returns ("on lines" string for metapositions, comma-separated mv names) *)
let line_vars ~metapos ~metavars =
  let line = List.map (fun x -> x ^ "[0].line") in
  match metapos with
   | [] -> ("", comma_sep metavars)
   | [x] -> ("on line %s.", comma_sep (metavars @ (line [x])))
   | x -> let agg = comma_sep (List.map (fun _ -> "%s") x) in
       ("on lines " ^ agg ^ ".", comma_sep (metavars @ (line x)))

(* turn metavariables into script header variables *)
let format_header_vars =
  let binding mv =
    let (rn,nm) = (M.get_rule mv, M.get_name mv) in
    nm ^ " << " ^ rn ^ "." ^ nm ^ ";" in
  List.map binding

(* only include metavars/positions in the format string if more than one *)
let format_err_msg err_msg mpnames mvnames =
  if (List.length mvnames + List.length mpnames) > 0 then
    let (linepos, formatvars) = line_vars ~metapos:mpnames ~metavars:mvnames in
    "msg = \"" ^ err_msg ^ " " ^ linepos ^ "\" % (" ^ formatvars ^ ")"
  else
    "msg = \"" ^ err_msg ^ ".\""

(* assembles an org script rule. *)
let gen_org_rule nm (firstpos, restpos) metavars err_msg =
  (* if there are metavars, they might contain brackets which conflict with
   * the todo format. In that case, use safe mode (replaces brackets). *)
  let printfn = if metavars <> [] then print_safe_todo_fn else print_todo_fn in
  let new_rulenm = nm ^ "_org" in
  let headervars = format_header_vars (metavars @ (firstpos :: restpos)) in
  (* the error message is used as is, positions are inserted in print calls *)
  let metavars = List.map M.get_name metavars in
  let err_msg = format_err_msg err_msg [] metavars in
  let zero p = (M.get_name p) ^ "[0]" in
  [
   (*header*)
   "@script:python " ^ new_rulenm ^ " depends on org@";
   String.concat "\n" headervars;
   "@@\n";
   (*body*)
   err_msg;
   (printfn ^ "(" ^ (zero firstpos) ^ ", msg)")
  ] @ (List.map (fun x -> print_link_fn ^ "(" ^ (zero x) ^ ", \"\")") restpos)

(* assembles a report script rule. *)
let gen_report_rule nm (firstpos, restpos) metavars err_msg =
  let new_rulenm = nm ^ "_report" in
  let headervars = format_header_vars (metavars @ (firstpos :: restpos)) in
  let firstpos = (M.get_name (firstpos)) ^ "[0]" in
  let restpos = List.map M.get_name restpos in
  let metavars = List.map M.get_name metavars in
  let err_msg = format_err_msg err_msg restpos metavars in
  [
   (*header*)
   "@script:python " ^ new_rulenm ^ " depends on report@";
   String.concat "\n" headervars;
   "@@\n";
   (*body*)
   err_msg;
   (print_report_fn ^ "(" ^ firstpos ^ ", msg)")
  ]


(* ------------------------------------------------------------------------- *)
(* ENTRY POINT *)

(* generate org and report rule for the added metapositions and with the user
 * specified information (error messages). *)
let generate ~metapos ~user_input = match user_input with
  | ((_, Some nm), (org_msg, omv), (report_msg, rmv))
  | ((nm, None), (org_msg, omv), (report_msg, rmv)) ->
  let (firstpos, restpos) = split_pos metapos in
  let new_rule = M.get_rule firstpos in
  (*make sure user-specified metavars are inherited from the context rule*)
  let omv, rmv = M.inherit_rule ~new_rule omv, M.inherit_rule ~new_rule rmv in
  let org = gen_org_rule nm (firstpos, restpos) omv org_msg in
  let report = gen_report_rule nm (firstpos, restpos) rmv report_msg in
  (org, report)

(* print the script rules *)
let print outch (org, rep) = printfn outch org; printfn outch rep

(* print first orgs then reps, call between() in between. *)
let print_split outch r between =
  let (orgs, reps) = List.split r in
  List.iter (printfn outch) orgs; between(); List.iter (printfn outch) reps
