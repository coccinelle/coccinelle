module Ast = Ast_cocci

exception CompileFailure of string
exception LinkFailure of string

let ext = if Dynlink.is_native then ".cmxs" else ".cma"
let has_ocamlfind = ref false

let check_cmd cmd =
  match Sys.command cmd with
      0 -> true
    | _ -> false

let check_runtime () =
  let has_opt  = check_cmd (!Flag.ocamlc ^".opt -version 2>&1 > /dev/null") in
  let has_c    = check_cmd (!Flag.ocamlc ^" -version 2>&1 > /dev/null") in
  let has_find = check_cmd (!Flag.ocamlfind ^ " printconf 2>&1 > /dev/null") in
    has_ocamlfind := has_find;
    if has_opt then
      begin
	Flag.ocamlc   := !Flag.ocamlc   ^ ".opt";
	Flag.ocamlopt := !Flag.ocamlopt ^ ".opt";
	Flag.ocamldep := !Flag.ocamldep ^ ".opt";
	Common.pr2 "Using native version of ocamlc/ocamlopt/ocamldep"
      end
    else
      if has_c then
	Common.pr2 "Using bytecode version of ocamlc/ocamlopt/ocamldep"
      else
	if Dynlink.is_native then
	  failwith
	    "No OCaml compiler found! Install either ocamlopt or ocamlopt.opt"
	else
	  failwith
	    "No OCaml compiler found! Install either ocamlc or ocamlc.opt"

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
	check_runtime ();
	Some file

let filter_dep (accld, accinc) dep =
  match dep with
      (* Built-in and OCaml defaults are filtered out *)
      "Arg" | "Arith_status" | "Array" | "ArrayLabels" | "Big_int" | "Bigarray"
    | "Buffer" | "Callback" | "CamlinternalLazy" | "CamlinternalMod" | "CamlinternalOO"
    | "Char" | "Complex" | "Condition" | "Digest" | "Dynlink" | "Event" | "Filename"
    | "Format" | "Gc" | "Genlex" | "GraphicsX11" | "Hashtbl" | "Int32" | "Int64"
    | "Lazy" | "Lexing" | "List" | "ListLabels" | "Map" | "Marshal" | "MoreLabels" | "Mutex"
    | "Nativeint" | "Num" | "Obj" | "Oo" | "Parsing" | "Pervasives" | "Printexc" | "Printf"
    | "Queue" | "Random" | "Scanf" | "Set" | "Sort" | "Stack" | "StdLabels" | "Str" | "Stream"
    | "String" | "StringLabels" | "Sys" | "ThreadUnix" | "Unix" | "UnixLabels"
    | "Weak"

    (* Coccilib is filtered out too *)
    | "Coccilib" -> (accld, accinc)

    | "Dbm"      -> ("dbm"::accld, accinc)
    | "Graphics" -> ("graphics"::accld, accinc)
    | "Thread"   -> ("thread"::accld, accinc)
    | "Tk"       -> ("tk"::accld, accinc)

    | _ ->
	let l = Char.lowercase (String.get dep 0)in
	  String.set dep 0 l;
	  (accld, dep::accinc)

let get_dir p =
  let inclcmd = !Flag.ocamlfind ^" query "^p in
  let dir = List.hd (Common.cmd_to_list inclcmd) in
    (dir, p)

let parse_dep mlfile depout =
  let re_colon = Str.regexp_string ":" in
  match Str.split re_colon depout with
      _::[dep] ->
	let deplist = Str.split (Str.regexp_string " ") dep in
	let (libs, orderdep) = List.fold_left filter_dep ([],[]) deplist in
	  if libs <> [] || orderdep <> [] then
	    if !has_ocamlfind then
	      let packages = List.rev orderdep in
	      let inclflags = List.map get_dir packages in
	      let intlib = List.map get_dir libs in
	      let alllibs = List.rev_append intlib inclflags in
	      let plist = List.fold_left (fun acc (_,p) -> acc ^" "^p) "" alllibs in
	      let flags = String.concat " " (List.map (fun (d,_) -> "-I "^d) inclflags) in
		if flags <> "" || libs <> [] then
		  (
		    Common.pr2 ("Extra OCaml packages used in the semantic patch:"^ plist);
		    (alllibs (* , inclflags *), flags)
		  )
		else
		  raise (CompileFailure ("ocamlfind did not found "^
					   (if (List.length libs + List.length orderdep) = 1
					    then "this package:"
					    else "one of these packages:")^ plist))
	    else
	      raise (CompileFailure ("ocamlfind not found but "^mlfile^" uses "^dep))
	  else
	    ([] (* , [] *), "")
    | _ -> raise (CompileFailure ("Wrong dependencies for "^mlfile^" (Got "^depout^")"))

let dep_flag mlfile =
  let depcmd  = !Flag.ocamldep ^" -modules "^mlfile in
    match Common.cmd_to_list depcmd with
	[dep] -> parse_dep mlfile dep
      | _ -> raise (CompileFailure ("Wrong dependencies for "^mlfile))

let compile_bytecode_cmd flags mlfile =
  let obj = (Filename.chop_extension mlfile) ^ ".cmo" in
    (obj, Printf.sprintf "%s -c %s %s %s" !Flag.ocamlc obj flags mlfile)

let compile_native_cmd flags mlfile =
  let obj = (Filename.chop_extension mlfile) ^ ".cmxs" in
    (obj, Printf.sprintf "%s -shared -o %s %s %s" !Flag.ocamlopt obj flags mlfile)

let compile mlfile cmd =
  Common.pr2 cmd;
  match Sys.command cmd with
      0 -> ()
    | _ -> raise (CompileFailure mlfile)

let rec load_obj obj =
  try
    Dynlink.loadfile obj
  with Dynlink.Error e ->
    match e with
	Dynlink.Unsafe_file ->
	  Dynlink.allow_unsafe_modules true;
	  load_obj obj
      | _ ->
	  Common.pr2 (Dynlink.error_message e);
	  raise (LinkFailure obj)

(*
let link_lib (dir, name) = name ^ ext

let link_libs libs =
    String.concat " " (List.map link_lib libs)
*)

let load_lib (dir, name) =
  let obj = dir ^ "/" ^name ^ ext in
    Common.pr2 ("Loading "^ obj ^"...");
    load_obj obj

let load_libs libs =
  List.iter load_lib libs

let load_file mlfile =
  let (ldlibs (* , lklibs *), inc) = dep_flag mlfile in
(*   let linklibs = link_libs lklibs in *)
  let flags = "-thread -g -dtypes -I /usr/lib/ocaml " ^ inc ^ " -I "^Config.path^"/ocaml/ " in
  let (obj, cmd) =
    if Dynlink.is_native
    then compile_native_cmd flags mlfile
    else compile_bytecode_cmd flags mlfile
  in
    compile mlfile cmd;
    Common.pr2 "Compilation OK!";
    load_libs ldlibs;
    Common.pr2 "Loading ML code of the SP...";
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
  This function is used in testing.ml.
  Once the ML file is compiled and loaded,
  newly available functions are reported here.
*)
let test () =
  Hashtbl.iter
    (fun key fct ->
       Common.pr2 ("Fct registered: \""^key^"\"")
    ) Coccilib.fcts
