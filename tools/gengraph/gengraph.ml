
let difffile = ref ""
let existfile = ref ""
let bugfile = ref ""
let orgfile = ref ""
let prefix = ref ""
let verbose = ref false
let help = ref false

let usage_msg =
  "Usage: " ^ Filename.basename Sys.argv.(0) ^
    " [-e <existfile> -b <bugfile>]" ^
    " [-p <prefix> [-d <difffile> | -o <orgfile>]]\n"

let options = [
  "-d", Arg.Set_string difffile, "file patchset between every version";
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
    if ((String.length !orgfile) != 0)
    && ((String.length !difffile) != 0)
    && ((String.length !prefix) != 0) then
      let diffs = (Diff.parse_diff !prefix !difffile) in
      let orgs = Org.parse_org !orgfile in
  	Diff.show_diff !verbose diffs;
	Org.show_org !verbose !prefix diffs orgs;
	Org.print_org  !prefix diffs orgs
    else
      if ((String.length !existfile) != 0)
      && ((String.length !bugfile) != 0) then
	let ast_exist = Exists.parse_exist !existfile in
	let bugs = Bugs.parse_bugs !bugfile in
	let (max_ver, fileexist) = Exists.compute_exist ast_exist in
	let bfl = List.map (Bugs.compute_bug max_ver) bugs in
	  Graph.show_stat !verbose max_ver fileexist bfl;
	  Graph.draw_graph max_ver fileexist bfl
      else
	Arg.usage aligned usage_msg
