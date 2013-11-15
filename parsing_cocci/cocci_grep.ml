(* input should be in CNF, ie an outer list, representing a conjunction, 
with inner lists of words, representing disjunctions.  There is no negation. *)

let interpret_clause clause l =
  List.exists
    (function re ->
      try let _ = Str.search_forward re l 0 in true
      with Not_found -> false)
    clause

let interpret regexp file =
  let i = open_in file in
  let rec loop regexp =
    let l = input_line i in
    let rec iloop = function
	[] -> None
      |	clause :: rest ->
	  if interpret_clause clause l
	  then Some rest
	  else
	    match iloop rest with
	      Some rest -> Some (clause :: rest)
	    | None -> None in
    match iloop regexp with
      Some [] -> true (* found everything *)
    | Some regexp -> loop regexp (* improved on this line *)
    | None -> loop regexp in (* failed on this line *)
  let res = try loop regexp with End_of_file -> false in
  close_in i;
  res
