
let existfile = ref ""
let bugfile = ref ""

let usage_msg =
  "Usage: " ^ Filename.basename Sys.argv.(0) ^ " -e <existfile> -b <bugfile>\n"

let options = [
  "-e",  Arg.Set_string existfile, "";
  "-b",  Arg.Set_string bugfile, ""
]

let _ =
  Arg.parse_argv Sys.argv (Arg.align options) (fun x -> ()) usage_msg;
  let ast_exist = Exists.parse_exist !existfile in
  let bugs = Bugs.parse_bugs !bugfile in
  let (max_ver, fileexist) = Exists.compute_exist ast_exist in
(*     List.iter show_entry fileexist; *)
  let bfl = List.map (Bugs.compute_bug max_ver) bugs in
(*     Graph.show_stat fileexist bfl; *)
    Graph.draw_graph max_ver fileexist bfl
