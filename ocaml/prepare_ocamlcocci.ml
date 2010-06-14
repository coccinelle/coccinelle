module Ast = Ast_cocci

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

let prepare code =
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
    let (file,o) = Filename.open_temp_file "ocaml_script" "ml" in
    (* Global initialization *)
    Printf.fprintf o "%s" (init_ocamlcocci());
    (* Semantic patch specific initialization *)
    Printf.fprintf o "%s" (String.concat "\n\n" init_rules);
    (* Semantic patch rules and finalizer *)
    let rule_code = List.map prepare_rule other_rules in
    Printf.fprintf o "%s" (String.concat "\n\n" rule_code);
    close_out o;
    Some file
