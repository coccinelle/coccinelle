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

(* path to directory with sgen tests *)
let test_dir = ref ""

let set_config x = config := x; interactive := false

let anonymous s = if !file = "" then file := s

let usage =
  let msg =
    "Usage: %s [options] <filename>\n" ^^ (* format string concatenation *)
    "Example: sgen --config file.config file.cocci.\n\n" ^^
    "Options are:" in
  Printf.sprintf msg (Filename.basename Sys.argv.(0))

let speclist =
[
  ("--config", Arg.String set_config,
   "<file> Configuration file for the generated file.");

  ("-c", Arg.String set_config,
   "<file> Shorthand for --config.");

  ("--interactive", Arg.Set interactive,
   " Run the program in interactive mode.");

  ("-i", Arg.Set interactive,
   " Shorthand for --interactive.");

  ("-o", Arg.Set_string output,
   "<file> Output result to this file instead of standard output.");

  ("--default", Arg.Set default,
   " Generate the file using generic default values instead of user input.");

  ("--no-output", Arg.Set hide,
   " Don't print the result.");

  ("--test", Arg.Set_string test_dir,
   "<path_to_tests_dir> Run the regression tests.");
]

(* ------------------------------------------------------------------------- *)
(* ENTRY POINT *)

let main _ =

  Arg.parse (Arg.align speclist) anonymous usage;

  if !test_dir <> "" then

    Sgen_test.regression_test ~test_dir:(!test_dir)

  else begin

    if !file = "" then (Arg.usage (Arg.align speclist) usage; exit 1);
    let options =
      Sgen.make_options
        ~config:!config
        ~interactive:!interactive
        ~default:!default
        ~output:!output
        ~hide:!hide
        !file in
    Sgen.run options

  end

let _ = main ()
