
exception CompileFailure of string
exception LinkFailure of string

let prepare coccifile code =
  let ocamls_rules =
    List.fold_left
      (function prev ->
	function
	    Ast_cocci.ScriptRule (name,"ocaml",deps,mv,script_vars,_pos,code) ->
	      code :: prev
	  | Ast_cocci.InitialScriptRule (name,"ocaml",deps,mvs,_pos,code) ->
	      code :: prev
	  | Ast_cocci.FinalScriptRule (name,"ocaml",deps,mvs,_pos,code) ->
	      code :: prev
	  | _ -> prev)
      [] code in
  if ocamls_rules = []
  then None
  else failwith "OCaml scripting is unsupported."

let prepare_simple _ =
  failwith "OCaml scripting is unsupported. Compile spatch with OCaml version >= 3.11"

let load_file mlfile = ()
let clean_file mlfile = ()
let test () = ()
