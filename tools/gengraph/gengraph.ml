
let existfile = ref ""
let bugfile = ref ""
let orgfile = ref ""
let prefix = ref ""
let verbose = ref false
let help = ref false

let usage_msg =
  "Usage: " ^ Filename.basename Sys.argv.(0) ^ " -e <existfile> -b <bugfile>\n"

let options = [
  "-e", Arg.Set_string existfile, "file CSV file-exist list";
  "-b", Arg.Set_string bugfile, "file simplified org file containing bug list";
  "-o", Arg.Set_string orgfile, "file org file";
  "-p", Arg.Set_string prefix, "path prefix of the source directories";
  "-v", Arg.Set verbose, " verbose mode";
  "-h", Arg.Set help, " Display this list of options"
]

let _ =
  let aligned = Arg.align options in
  Arg.parse_argv Sys.argv aligned (fun x -> ()) usage_msg;
  if (!help) then
    Arg.usage aligned usage_msg
  else
    if (!orgfile != "") then
      let orgs = Org.parse_org !orgfile in
	Org.show_org !prefix orgs
    else
      let ast_exist = Exists.parse_exist !existfile in
      let bugs = Bugs.parse_bugs !bugfile in
      let (max_ver, fileexist) = Exists.compute_exist ast_exist in
	(*     List.iter show_entry fileexist; *)
      let bfl = List.map (Bugs.compute_bug max_ver) bugs in
	Graph.show_stat !verbose max_ver fileexist bfl;
	Graph.draw_graph max_ver fileexist bfl
