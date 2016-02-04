let read_to_diff i =
  let lines = ref [] in
  let rec loop _ =
    let l = input_line i in
    if Str.string_match (Str.regexp "diff -u -p ") l 0
    then (List.rev !lines,Some l)
    else (lines := l::!lines; loop ()) in
  try loop() with End_of_file -> (List.rev !lines,None)

let get_file l = (* l is a diff line *)
  match Str.split (Str.regexp " ") l with
    [_diff;_u;_p;old;_new] -> old
  | _ -> failwith "bad diff line"

let get_files prefix =
  let files = Array.to_list(Sys.readdir(Sys.getcwd())) in
  let relevant name =
    let rel_re = Str.regexp "\\(.*\\)\\.[0-9]+\\.out" in
      if Str.string_match rel_re name 0 then
	let pref = Str.matched_group 1 name in
	  pref = prefix
      else
	false
  in
  List.filter relevant files

let process_file fl =
  let i = open_in fl in
  let elements = ref [] in
  (match read_to_diff i with
    (_,Some first_line) ->
      let rec loop diff =
	let (cur,more) = read_to_diff i in
	elements := (get_file diff,diff::cur) :: !elements;
	match more with
	  Some next_line -> loop next_line
	| None -> () in
      loop first_line
  | _ -> ()); (* file is empty *)
  close_in i;
  !elements

let process_all_files files out =
  let elements = [] (* no clue what this is supposed to do,
		       but can discard output
    List.sort compare (List.concat (List.map process_file files)) *) in
  match elements with
    [] -> (* only python output *)
      let fl = String.concat " " (List.sort compare files) in
      let _ = Sys.command (Printf.sprintf "cat %s > %s" fl out) in
      ()
  | _ ->
      let o = open_out out in
      List.iter
	(function (_,code) ->
	  List.iter
	    (function x -> Printf.fprintf o "%s\n" x)
	    code)
	elements

let _ =
  let arg = List.hd(List.tl(Array.to_list Sys.argv)) in
  Printf.printf "arg %s\n" arg;
  let arg = Filename.chop_extension arg in
  let files = get_files arg in
  process_all_files files (arg^".out");
  let tmp_files =
    String.concat " "
      (List.map (function x -> "tmp."^x) (List.sort compare files)) in
  let _ = Sys.command (Printf.sprintf "cat %s > %s.tmp" tmp_files arg) in
  List.iter
    (function file ->
      let _ = Sys.command (Printf.sprintf "/bin/rm %s" file) in
      let _ = Sys.command (Printf.sprintf "/bin/rm tmp.%s" file) in ())
    files
