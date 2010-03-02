(* split patch per file *)

let is_diff = Str.regexp "diff "
let split_patch i =
  let patches = ref [] in
  let cur = ref [] in
  let rec loop _ =
    let l = input_line i in
    (if Str.string_match is_diff l 0
    then
      (if List.length !cur > 0
      then begin patches := List.rev !cur :: !patches; cur := [l] end)
    else cur := l :: !cur);
    loop() in
  try loop() with End_of_file -> !patches

(* ------------------------------------------------------------------------ *)

(* can get_maintainers takea file as an argument, or only a patch? *)
let resolve_maintainers cmd patches =
  let maintainer_table = Hashtbl.create (List.length patches) in
  List.iter
    (function
	diff_line::rest ->
	  (match Str.split (Str.regexp " a/") diff_line with
	    [before;after] ->
	      (match Str.split (Str.regexp " ") after with
		file::_ ->
		  (match Common.cmd_to_list (cmd ^ " " ^ file) with
		    [info] ->
		      let cell =
			try Hashtbl.find maintainer_table info
			with Not_found ->
			  let cell = ref [] in
			  Hashtbl.add maintainer_table info cell;
			  cell in
		      cell := (diff_line :: rest) :: !cell
		  | _ -> failwith "badly formatted maintainer result")
	      |	_ -> failwith "filename not found")
	  | _ ->
	      failwith (Printf.sprintf "prefix a/ not found in %s" diff_line))
      |	_ -> failwith "bad diff line")
    patches;
  maintainer_table

(* ------------------------------------------------------------------------ *)

let print_all o l =
  List.iter (function x -> Printf.fprintf o "%s\n" x) l

let make_output_files template maintainer_table patch =
  let ctr = ref 0 in
  Hashtbl.iter
    (function maintainers ->
      function diffs ->
	ctr := !ctr + 1;
	let o = open_out (Printf.sprintf "%s%d" patch !ctr) in
	Printf.fprintf o "To: %s\n\n" maintainers;
	print_all o template;
	List.iter (print_all o) (List.rev !diffs);
	close_out o)
    maintainer_table

(* ------------------------------------------------------------------------ *)

let command = ref "get_maintainers.pl"
let file = ref ""

let options =
  ["-cmd", Arg.String (function x -> command := x), "get maintainer command"]

let usage = ""

let anonymous x = file := x

let _ =
  Arg.parse (Arg.align options) (fun x -> file := x) usage;
  let i = open_in !file in
  let patches = split_patch i in
  let maintainer_table = resolve_maintainers !command patches in
  let template = Common.cmd_to_list (Printf.sprintf "cat %s.tmp" !file) in
  make_output_files template maintainer_table !file
