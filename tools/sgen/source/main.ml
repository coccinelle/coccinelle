(* ------------------------------------------------------------------------- *)
(* OPTIONS *)

(* the cocci script to be generated *)
let file = ref ""

(* the config file to draw user input from *)
let config = ref ""

(* whether to draw user input interactively *)
let interactive = ref false

(* whether to generate without user input (using default values) *)
let default = ref false

(* where to output the resulting generated file *)
let output = ref ""

(* hide resulting generated file *)
let hide = ref false

let set_config x = config := x; interactive := false

let anonymous s = if !file = "" then file := s

let usage =
  Printf.sprintf "Usage: %s [options] <filename> \nOptions are:"
    (Filename.basename Sys.argv.(0))

let speclist =
[ ("--config", Arg.String set_config,
   " <file> Configuration file for the generated file.");
  ("-c", Arg.String set_config, " <file> Shorthand for --config.");
  ("--interactive", Arg.Set interactive,
   " Run the program in interactive mode.");
  ("-i", Arg.Set interactive, " Shorthand for --interactive.");
  ("-o", Arg.Set_string output,
   " <file> Output result to this file instead of standard output.");
  ("--default", Arg.Set default,
   " Generate the file using generic default values instead of user input.");
  ("--no-output", Arg.Set hide, " Don't print the result.");
]

let fail m = Arg.usage (Arg.align speclist) usage; failwith m


(* ------------------------------------------------------------------------- *)
(* ENTRY POINT *)

let main _ =
  Arg.parse (Arg.align speclist) anonymous usage;
  if !file = "" then
    fail "Filename required.\n Example: sgen --config file.config file.cocci.";
  if not(Sys.file_exists !file) then
    fail ("The file \""^ !file ^"\" doesn't exist!");
  if !config <> "" && not(Sys.file_exists !config) then
    fail ("The config file \"" ^ !config ^ "\" doesn't exist!");

  (* if not interactive mode and not specified config name, look for
   * <file>.config. If no config, run in interactive mode *)
  let name = Globals.new_extension ~new_ext:"config" !file in
  if not(!interactive || !default) && !config = "" && Sys.file_exists name then
    config := name
  else if not(!default) && !config = "" then
    interactive := true;

  (* ------------- PARSE ------------- *)
  Flag_parsing_cocci.generating_mode := true;
  let (_, rules, virtuals, _) = Parse_cocci.parse !file in

  (* the sgrep_mode2 flag is set after parsing, if the rule is a star rule *)
  let context_mode = !Flag.sgrep_mode2 in

  (* check rulenames for validity and get the */+/- rules *)
  let rulenames = List.map Ast0_cocci.get_rule_name rules in
  let _ = List.iter (Globals.check_rule ~strict:false) rulenames in
  let (rules, disj_maps) = Detect_patch.get_patch_rules rules in
  let rulenames = List.map Ast0_cocci.get_rule_name rules in

  (* ------------- GLOBALS ------------- *)

  let (_(*author*), _(*license*), rule_name, pos_name, error_msg, char_limit) =
    Sgen_config.parse_global ~config_name:"" in
  Globals.init ~rule_name ~pos_name ~error_msg ~char_limit;
  let virtuals = Globals.key_virtuals virtuals context_mode in

  (* ------------- LOCALS ------------- *)
  let (preface, input) =
    if !interactive then
      Sgen_interactive.interact ~ordered_rules:rulenames ~config_name:name
    else if !default then
      Sgen_config.parse_default ~ordered_rules:rulenames
    else
      Sgen_config.parse_local ~ordered_rules:rulenames ~config_name:!config in

  (* ------------- GENERATE ------------- *)
  (* drules are all patch rules, tupled with their disj maps
   * userinput are the user input infos for the patch rules, in sorted order *)
  let generate drules userinput =
    let generate_rule (rule,disj_map) (((old_name,new_name),_,_) as ui) =
      let _ = assert (Ast0_cocci.get_rule_name rule = old_name) in
      let (generated, pos) =
        Context_rule.generate ~new_name ~disj_map ~rule ~context_mode in
      let scripted = Script_rule.generate ~metapos:pos ~user_input:ui in
      ((rule, new_name), (generated, scripted)) in
    let rec split fn = function
      | (nrule,(gen,script))::xs ->
          split (fun (a,b,c) -> fn (nrule::a, gen::b, script::c)) xs
      | [] -> fn ([],[],[]) in
    split (fun x -> x) (List.map2 generate_rule drules userinput) in

  let combined = List.combine rules disj_maps in
  let (namedrules, contexts, scripts) = generate combined input in

  (* ------------- PRINT ------------- *)
  if not(!hide) then begin
    let outp = if !output = "" then stdout else (open_out !output) in
    let split() = output_string outp
      ("// -----------------------------------------------------------------" ^
      "-----------\n\n") in
    try
      File_transform.print ~channel:outp ~file_name:!file ~preface ~virtuals
        ~rules:namedrules ~context_mode;
      split(); List.iter (Context_rule.print outp) contexts;
      split(); Script_rule.print_split outp scripts split;
      flush outp; close_out outp
    with Failure msg ->
      flush outp;
      close_out outp;
      if !output <> "" && Sys.file_exists !output then Sys.remove !output;
      failwith msg
  end

let _ = main ()
