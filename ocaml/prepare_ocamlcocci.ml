module Ast = Ast_cocci

exception CompileFailure of string
exception LinkFailure of string

let init_ocamlcocci _ =
  "open Coccilib\n"

let prepare_rule (name, metavars, code) =
  let fname = String.concat "_" (Str.split (Str.regexp " ") name) in
  (* function header *)
  let function_header body =
    Printf.sprintf "let %s args =\n %s" fname body in
  (* parameter list *)
  let build_parameter_list body =
    let ctr = ref 0 in
    List.fold_left
      (function body ->
	function (nm,_,mv) ->
	  let endlet =
	    Printf.sprintf "| _ -> failwith \"bad value\" in\n%s" body in
	  let index = !ctr in
	  ctr := !ctr + 1;
	  match mv with
	    Ast.MetaPosDecl(_,_) ->
	      Printf.sprintf
		"let %s = match List.nth args %d with Coccilib.Pos x -> x %s"
		nm index endlet
	  | _ (* strings for everything else *) ->
	      Printf.sprintf
		"let %s = match List.nth args %d with Coccilib.Str x -> x %s"
		nm index endlet)
      body metavars in
  (* add to hash table *)
  let hash_add body =
    Printf.sprintf
      "%s\nlet _ = Hashtbl.add Coccilib.fcts \"%s\" %s\n" body name fname in
  hash_add (function_header (build_parameter_list code))

let prepare coccifile code =
  let init_rules =
    List.fold_left
      (function prev ->
	function
	    Ast_cocci.InitialScriptRule (name,"ocaml",deps,code) ->
	      code :: prev
	  | _ -> prev)
      [] code in
  let init_rules = List.rev init_rules in
  let other_rules =
    List.fold_left
      (function prev ->
	function
	    Ast_cocci.ScriptRule (name,"ocaml",deps,mv,code) ->
	      (name,mv,code) :: prev
	  | Ast_cocci.InitialScriptRule (name,"ocaml",deps,code) -> prev
	  | Ast_cocci.FinalScriptRule (name,"ocaml",deps,code) ->
	      (name,[],code) :: prev
	  | _ -> prev)
      [] code in
  let other_rules = List.rev other_rules in
  if init_rules = [] && other_rules = []
  then None
  else
    let basefile = Filename.basename (Filename.chop_extension coccifile) in
    let (file,o) = Filename.open_temp_file  basefile ".ml" in
    (* Global initialization *)
    Printf.fprintf o "%s" (init_ocamlcocci());
    (* Semantic patch specific initialization *)
    Printf.fprintf o "%s" (String.concat "\n\n" init_rules);
    (* Semantic patch rules and finalizer *)
    let rule_code = List.map prepare_rule other_rules in
    Printf.fprintf o "%s" (String.concat "\n\n" rule_code);
    close_out o;
    Some file

let filter_dep acc dep =
  match dep with
      "Array" | "String" | "Printf" | "Arg" | "Obj" | "Printexc"
    | "Hashtbl" | "List" | "Coccilib" -> acc
    | _ -> String.lowercase dep::acc

let dep_flag mlfile =
  let depcmd  = "ocamldep -modules "^mlfile^"| cut -f2 -d':'" in
    match Common.cmd_to_list depcmd with
	[dep] ->
	  let deplist = Str.split (Str.regexp_string " ") dep in
	  let orderdep = List.rev (List.fold_left filter_dep [] deplist) in
	  let packages = String.concat " " orderdep in
	  let inclcmd = "ocamlfind query -i-format "^packages in
	  let inclflags = Common.cmd_to_list inclcmd in
	    Common.pr2 ("Packages used: "^packages);
	    String.concat " " inclflags
      | _ -> raise (CompileFailure ("Wrong dependencies for "^mlfile))

(*************************************************************

let loadstr = format_of_string
"let _ =
  print_endline \"Loading %s module\";
"

let regstr = format_of_string "  Hashtbl.add Tbl.fcts \"%s\" %s"

let write_file coccifile initcode rulecode =
  let coccimlfile = Filename.temp_file coccifile ".ml" in
  let ch = open_out coccimlfile in
    output_string ch initcode;
    let regcode =
      List.map
	(fun (name, str) ->
	   let fct = "let "^name^ " () =" in
	     output_string ch fct;
	     output_string ch (str^"\n");
	     Printf.sprintf regstr name name
	) rulecode
    in
      output_string ch (Printf.sprintf loadstr coccifile);
      output_string ch ((String.concat ";\n" regcode)^";\n");
      output_string ch ("  init ()\n");
      close_out ch;
      coccimlfile

*************************************************************)

let compile_bytecode_cmd flags mlfile =
  let obj = (Filename.chop_extension mlfile) ^ ".cmo" in
    (obj, Printf.sprintf "%s -c %s %s %s" !Config.ocamlc obj flags mlfile)

let compile_native_cmd flags mlfile =
  let obj = (Filename.chop_extension mlfile) ^ ".cmxs" in
    (obj, Printf.sprintf "%s -shared -o %s %s %s" !Config.ocamlopt obj flags mlfile)

let compile mlfile cmd =
  Common.pr2 cmd;
  match Sys.command cmd with
      0 -> ()
    | _ -> raise (CompileFailure mlfile)

let load_file mlfile =
  let flags = "-g " ^ (dep_flag mlfile) ^ " -I "^Config.path^"/ocaml/" in
  let (obj, cmd) =
    if Dynlink.is_native
    then compile_native_cmd flags mlfile
    else compile_bytecode_cmd flags mlfile
  in
    compile mlfile cmd;
    Common.pr2 "Compilation OK! Loading...";
    try
      Dynlink.loadfile obj
    with Dynlink.Error e ->
      Common.pr2 (Dynlink.error_message e);
      raise (LinkFailure obj)

let clean_file mlfile =
  let basefile = Filename.chop_extension mlfile in
  let files =
    if Dynlink.is_native then
      [basefile ^ ".cmxs";
       basefile ^ ".cmx";
       basefile ^ ".o"]
    else
      [basefile ^ ".cmo"]
  in
    Sys.remove mlfile;
    Sys.remove (basefile^".cmi");
    List.iter (fun f -> Sys.remove f) files

(*
  This function should be removed
  when the others will work!
*)
let test () =
  Hashtbl.iter
    (fun key fct ->
       Common.pr2 ("Fct registered: \""^key^"\"")
    ) Coccilib.fcts


