(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at https://coccinelle.gitlabpages.inria.fr/website/
 *)

module UI = User_input

(* ------------------------------------------------------------------------- *)

(* Interactive commandline mode for getting (local) user input. *)

(* ------------------------------------------------------------------------- *)
(* GENERAL PURPOSE FUNCTIONS *)

(* name of the file to save to. Is set at the entry point. *)
let name = ref ""

(* persistence *)
let write_file ~file s =
  let chan =
    try open_out file
    with Sys_error msg ->
      let msg = "Error: Invalid filename: " ^ file ^ ". Message: " ^ msg in
      failwith msg in
  try
    (output_string chan s; close_out chan)
  with Sys_error msg ->
    let msg = "Error: failed writing to " ^ file ^ ". Message: " ^ msg in
    failwith msg

(* termination *)
let exit() = print_string "\n~*~ GOODBYE! ~*~\n"; exit 0

(* allow the user to save the current progress in a config file *)
let save t =

  (* unparse the current configurations and save them to a file *)
  let save' t name =
    let unp = UI.unparse t in
    let _ = write_file ~file:name unp in
    print_string ("\nSaved progress to " ^ name ^ "!\n" ^ "-------------\n") in

  let name = !name in
  let _ = print_string ("\nSave progress to " ^name^"?\n") in
  let _ = print_string ("Options:\n" ^
    "  Type y(es) or press <enter> to save.\n"^
    "  Type n(o) to not save.\n"^
    "  Write another filename to save to.\n") in

  match Stdcompat.String.lowercase_ascii (read_line()) with
  | "" | "y" | "yes" -> save' t name
  | "n" | "no" -> ()
  | file -> save' t file

let get_input_save t =
  let input = read_line() in
  if input = "q()" then (save t; exit()) else input

let get_input() =
  let input = read_line() in
  if input = "q()" then exit() else input

let print_error msg =
  print_string "\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n\n";
  print_string msg;
  print_string "\n\n<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n"


(* ------------------------------------------------------------------------- *)
(* INTERACTIVE USER INPUT GETTERS *)

let rec get_description() =
  print_string "\nWrite a description for the Coccinelle script (required):\n";
  match get_input() with
  | "" -> get_description()
  | x -> x

let rec get_confidence() =
  print_string ("\nSpecify a confidence level for the script (required):\n" ^
    "Options: l(ow), m(oderate), h(igh).\n");
  try
    UI.Confidence.from_string (get_input())
  with UI.Confidence.Not_confidence _ ->
    (print_error "Error: Ill-formed confidence level!"; get_confidence())

let get_limitations t =
  let rec get first t =
    print_string ("\nSpecify a" ^ (if first then "" else "nother") ^
      " limitation for the script or press <enter> to continue:\n");
    match get_input_save t with
    | "" -> t
    | x -> get false (UI.add_limit x t) in
  get true t

let get_keywords t =
  print_string ("\nSpecify keywords for the script or press <enter> to " ^
    "continue:\n");
  let keys = get_input_save t in
  UI.set_keys keys t

let get_options t =
  print_string ("\nSpecify options for the script or press <enter> to " ^
    "continue:\n");
  let options = get_input_save t in UI.set_options options t

let get_url t =
  print_string ("\nSpecify an URL for the script or press <enter> to " ^
    "continue:\n");
  let url = get_input_save t in UI.set_url url t

let get_authors t =
  let rec get first t =
    print_string ("\nSpecify an" ^ (if first then "" else "other") ^
      " author for the script or press <enter> to continue:\n" ^
      "Standard format is: <author name>, <affiliation>. <license>.\n");
    match get_input_save t with
    | "" -> t
    | x -> get false (UI.add_author x t) in
  get true t

let get_comments t =
  print_string ("\nWrite any further comments for the script or press " ^
    "<enter> to continue:\n");
  let comments = get_input_save t in UI.set_comments comments t

(* get org or report msg. strict denotes whether it is required. *)
let rec get_message pmsg strict t =
  print_string pmsg;
  let msg = get_input_save t in
  if msg = "" && strict then

    (* message required but none given; try again *)
    get_message pmsg strict t

  else begin

    (* if there are no format variables, just use the message alone*)
    let pcts = UI.count_format_vars msg in
    if pcts = 0 then (msg,[])
    else begin

      (* there were format variables; allow user to declare them *)
      let _ = print_string (
        "\nDeclare the " ^(string_of_int pcts) ^" variable(s) " ^
        "used in the message, in order, separated by comma.\n" ^
        "Inherited metavariables are declared by <rulename>.<metavarname>.\n"
      ) in
      let mv = get_input_save t in
      let mv = Str.split (Str.regexp " *, *") mv in

      (* if user declared format variables successfully, return *)
      if List.length mv = pcts
      then (msg, mv)
      else

        (* the number of format variables did not match the actual message *)
        let _ = print_error (
          "Error: Ill-formed message; " ^
          "number of format variables does not match number of declared " ^
          "metavariables. Try again."
        ) in
        get_message pmsg strict t
    end
  end

(* gets rulename if there is currently none (rule starting on ...).
 * if there is one, just return the same.
 *)
let get_name r t =
  let rec get_name' r =
    print_string ("\nSpecify a name for the " ^ r ^ ":\n");
    let newnm = get_input_save t in
    try
      UI.check_name newnm t;
      newnm
    with Failure m ->
      print_error m;
      get_name' r in
  if String.contains r ' ' then get_name' r else r

(* returns rule mapped to user-specified org and report messages + new name. *)
let get_rule (rulename : string) (t : UI.t) =
  let _ = print_string ("\nHandling rule \"" ^ rulename ^ "\" ...") in
  let nm = get_name rulename t in
  let _ = print_string ("\n~ Getting messages for org and report mode ~\n\n" ^
    "No quotes necessary around the message.\n" ^
    "It is possible to include formatted variables like %s (Python style), " ^
    "you will get a chance to declare them afterwards.\n" ^
    "Example message: Unneeded variable \\\"%s\\\". Return \\\"%s\\\".\n" ^
    "Example format variables: x, other_rule.y. \n") in
  let org = get_message ("\nRule \"" ^ nm ^ "\". Write a message"^
    " for org mode:\n") true t in
  let (repmsg,repmvs) = get_message ("\nRule \"" ^ nm ^ "\". Write a message" ^
    " for report or press <enter> to reuse the org message.\n") false t in
  let report = if repmsg = "" then org else (repmsg,repmvs) in
  UI.Rule.make ~rule_name:nm ~org ~report


(* ------------------------------------------------------------------------- *)
(* ENTRY POINT *)

let interact ~rule_names ~config_name =
  name := config_name;
  print_string ("\n~*~ WELCOME TO SPGEN INTERACTIVE MODE ~*~\n\n" ^
  "At any time, write q() to quit the program.\n");
  let description = get_description() in
  let confidence = get_confidence() in
  let t = UI.make ~description ~confidence in
  let t = get_limitations t in
  let t = get_keywords t in
  let t = get_options t in
  let t = get_authors t in
  let t = get_url t in
  let t = get_comments t in
  let rec add t r =
    try UI.add_rule ~rule_name:r (get_rule r t) t
    with Failure msg ->
      print_error msg;
      add t r in
  let t = List.fold_left add t rule_names in
  let _ = save t in
  t
