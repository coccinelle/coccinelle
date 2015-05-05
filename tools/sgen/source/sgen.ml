(* ------------------------------------------------------------------------- *)

(* Driver module for the whole program.
 *
 * The high-level process is something like:
 * 1. Parse the cocci file using the main parser
 * 2. Parse the user-provided input for the sgenerated file
 * 3. Generate context rules (ie. rules with *'s and positions)
 * 4. Generate script rules (ie. rules for org and report modes)
 * 5. Print the preface (ie. metadata and virtuals)
 * 6. Print the original file with transformations in dependencies, names, etc.
 * 7. Print the generated context and script rules
 *)

(* ------------------------------------------------------------------------- *)

type options =
{
  file : string;
  config : string;
  output : string;
  interactive : bool;
  default : bool;
  hide : bool;
}

(* if no options are set, it defaults to interactive mode *)
let make_options
  ?(config = "")
  ?(output = "")
  ?(interactive = false)
  ?(default = false)
  ?(hide = false)
  file =
    { file; config; output; interactive; default; hide }


(* ------------------------------------------------------------------------- *)
(* ENTRY POINT *)

let run { file; config; output; interactive; default; hide; } =

  (* ------------- CHECK ------------- *)

  if not(Sys.file_exists file) then
    failwith ("The file \""^ file ^"\" doesn't exist!");

  if config <> "" && not(Sys.file_exists config) then
    failwith ("The config file \"" ^ config ^ "\" doesn't exist!");

  if file = output then
    failwith ("Input file cannot be the same as output file!");


  (* ------------- SETTINGS ------------- *)

  (* default config name. Ie. <file_name>.config *)
  let (dir, base, _) = Common.dbe_of_filename file in
  let name = Common.filename_of_dbe (dir, base, "config") in

  (* if no config specified, but the default config exists, use it *)
  let config =
    if config = "" && Sys.file_exists name
    then name
    else config in

  (* if no config specified and no default config, run in interactive mode *)
  let interactive = (config = "") || interactive in


  (* ------------- PARSE ------------- *)

  Flag_parsing_cocci.generating_mode := true;
  let (_, rules, virtuals, _) = Parse_cocci.parse file in
  Flag_parsing_cocci.generating_mode := false; (* cleanup! for tests, etc. *)

  (* if the rule is a star rule, the sgrep_mode2 flag is set after parsing *)
  let context_mode = !Flag.sgrep_mode2 in

  (* get the */+/- rules and check that their names don't create conflicts *)
  let rules_disj_maps = Detect_patch.filter_patch_rules rules in
  let rule_names =
    List.map (fun (x,_) -> Ast0_cocci.get_rule_name x) rules_disj_maps in
  let _ = List.iter (Globals.check_rule ~strict:false) rule_names in


  (* ------------- GLOBALS ------------- *)

  (* these are settings that are usually the same regardless of rule *)

  let (_(*author*), _(*license*), rule_name, pos_name, error_msg, char_limit) =
    Sgen_config.parse_global ~config_name:"" in
  let _ = Globals.init ~rule_name ~pos_name ~error_msg ~char_limit in
  let virtuals = Globals.key_virtuals ~context_mode virtuals in


  (* ------------- LOCALS ------------- *)

  let user_input =
    if default then
      Sgen_config.parse_default
    else if interactive then
      Sgen_interactive.interact ~rule_names ~config_name:name
    else
      Sgen_config.parse_local ~rule_names ~config_name:config in

  let preface = User_input.get_preface user_input in


  (* ------------- GENERATE ------------- *)

  (* drules is the ordered list of patch rules, tupled with their disj maps. *)
  let generate drules =
    let rec generate' rules fn =
      match rules with
      | (rule, disj_map) :: rs ->

          (* extract the corresponding user input to current rule *)
          let old_name = Ast0_cocci.get_rule_name rule in
          let user_rule = User_input.get_rule ~rule_name:old_name user_input in
          let new_name = User_input.Rule.get_name user_rule in

          (* generate context and script rules *)
          let nrule = (rule, new_name) in
          let (ctxt, meta_pos) =
            Context_rule.generate ~context_mode ~new_name ~disj_map ~rule in
          let script =
            Script_rule.generate ~meta_pos ~user_rule in

          let add (rs, cs, ss) = fn (nrule::rs, ctxt::cs, script::ss) in
          generate' rs add

      | [] -> fn ([],[],[]) in
    generate' drules (fun x -> x) in

  let (namedrules, contexts, scripts) = generate rules_disj_maps in


  (* ------------- PRINT ------------- *)

  if not(hide) then begin
    let out = if output = "" then stdout else (open_out output) in
    let split() = output_string out
      ("// --------------------------------------" ^
       "--------------------------------------\n\n") in
    try

      File_transform.print
        ~context_mode ~file_name:file ~preface ~virtuals
        ~ordered_rules:namedrules out;
      split();
      List.iter (Context_rule.print out) contexts;
      split();
      List.iter (Script_rule.print_org out) scripts;
      split();
      List.iter (Script_rule.print_report out) scripts;
      flush out;
      close_out out

    with Failure msg ->

      flush out;
      close_out out;
      if output <> "" && Sys.file_exists output then Sys.remove output;
      failwith msg

  end
