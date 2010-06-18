open Common

let run mv ve name code =
  (* set up variables *)
  let find_binding (r,m) =
    try
      let elem =
	List.find (function ((re,rm),_) -> r =*= re && m =$= rm) ve in
      Some elem
    with Not_found -> None in

  let args =
    List.map
      (function (ocaml_name,(r,m),_) ->
	match find_binding (r,m) with
	  None -> failwith "unbound variable"
	| Some (_, Ast_c.MetaPosValList l) ->
	    let locs =
	      List.map
		(function
		    (fname,current_element,(line,col),(line_end,col_end)) ->
		      { Coccilib.current_element = current_element;
			Coccilib.line = string_of_int line;
			Coccilib.col = string_of_int col;
			Coccilib.line_end = string_of_int line_end;
			Coccilib.col_end = string_of_int col_end }) l in
	    Coccilib.Pos locs
	| Some (_,binding) ->
	    Coccilib.Str (Ocamlcocci_aux.stringrep binding))
      mv in

  (* call the function *)
  Coccilib.include_match true;
  let fn =
    try Hashtbl.find Coccilib.fcts name
    with Not_found -> failwith (Printf.sprintf "%s not found" name) in
  fn args
