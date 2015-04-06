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

(* if no options are set, it actually defaults to interactive mode *)
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


  (* ------------- SETTINGS ------------- *)

  (* default config name. Ie. <file_name>.config *)
  let name = Globals.new_extension ~new_ext:"config" file in

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

  (* if the rule is a star rule, the sgrep_mode2 flag is set after parsing *)
  let context_mode = !Flag.sgrep_mode2 in

  (* check rulenames for validity and get the */+/- rules *)
  let rulenames = List.map Ast0_cocci.get_rule_name rules in
  let _ = List.iter (Globals.check_rule ~strict:false) rulenames in
  let rules_disj_maps = Detect_patch.filter_patch_rules rules in
  let rulenames =
    List.map (fun (x,_) -> Ast0_cocci.get_rule_name x) rules_disj_maps in


  (* ------------- GLOBALS ------------- *)

  (* these are settings that are usually the same regardless of rule *)

  let (_(*author*), _(*license*), rule_name, pos_name, error_msg, char_limit) =
    Sgen_config.parse_global ~config_name:"" in
  let _ = Globals.init ~rule_name ~pos_name ~error_msg ~char_limit in
  let virtuals = Globals.key_virtuals virtuals context_mode in


  (* ------------- LOCALS ------------- *)

  let (preface, input) =
    if default then
      Sgen_config.parse_default ~ordered_rules:rulenames
    else if interactive then
      Sgen_interactive.interact ~ordered_rules:rulenames ~config_name:name
    else
      Sgen_config.parse_local ~ordered_rules:rulenames ~config_name:config in


  (* ------------- GENERATE ------------- *)

  (* drules are all patch rules, tupled with their disj maps.
   * userinput are the ordered user input infos for the patch rules.
   *)
  let generate drules userinput =
    let rec generate_split rules ui fn =
      match rules, ui with
      | [], [] -> fn ([],[],[])
      | (rule,disj_map)::rs,
        (((old_name,new_name),_,_) as user_input)::us ->

          (* make sure the lists are aligned; if fail, it's probably because
           * rulenames and rules_disj_maps are not properly aligned *)
          let _ = assert (Ast0_cocci.get_rule_name rule = old_name) in
          let rle = (rule, new_name) in
          let (ctxt, metapos) =
            Context_rule.generate ~new_name ~disj_map ~rule ~context_mode in
          let script = Script_rule.generate ~metapos ~user_input in
          generate_split rs us (fun (a,b,c) -> fn (rle::a, ctxt::b, script::c))

      | _ -> failwith "Internal error: drules.length <> userinput.length." in
    generate_split drules userinput (fun x -> x) in

  let (namedrules, contexts, scripts) = generate rules_disj_maps input in


  (* ------------- PRINT ------------- *)

  if not(hide) then begin
    let out = if output = "" then stdout else (open_out output) in
    let split() = output_string out
      ("// -----------------------------------------------------------------" ^
      "-----------\n\n") in
    try

      File_transform.print
        ~file_name:file ~preface ~virtuals ~ordered_rules:namedrules
        ~context_mode out;
      split();
      List.iter (Context_rule.print out) contexts;
      split();
      Script_rule.print_split out split scripts;
      flush out;
      close_out out

    with Failure msg ->

      flush out; close_out out;
      if output <> "" && Sys.file_exists output then Sys.remove output;
      failwith msg

  end
