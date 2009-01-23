open Lexing

exception Unexpected of string
exception Break

let read_pos pos =
  let pos_re = Str.regexp "^\\([0-9]+\\)\\(,\\([0-9]+\\)\\)?$" in
  let posb = Str.string_match pos_re pos 0 in
    if posb then
      let linepos = int_of_string (Str.matched_group 1 pos) in
      let linenum =
	try int_of_string (Str.matched_group 3 pos)
	with Not_found -> 1
      in
	(linepos, linenum)
    else
      raise (Unexpected "bad hunk position format")

let rec skip in_ch lineskip =
  if lineskip > 0 then
    begin
      ignore(input_line in_ch);
      skip in_ch (lineskip -1)
    end

let rec read_hunks in_ch =
  let hunk_desc = input_line in_ch in
  let hunk_re = Str.regexp "^@@ -\\([^ ]+\\) \\+\\([^ ]+\\) @@$" in
  let hunkb = Str.string_match hunk_re hunk_desc 0 in
    if hunkb then
      let before = read_pos (Str.matched_group 1 hunk_desc) in
	ignore(Str.string_match hunk_re hunk_desc 0);
      let after = read_pos (Str.matched_group 2 hunk_desc) in
	skip in_ch ((snd before) + (snd after));
	let tail =
	  try
	    read_hunks in_ch
	  with End_of_file -> []
	in
	  (before, after)::tail
    else
      begin
	seek_in in_ch ((pos_in in_ch) - (1+String.length hunk_desc));
	[]
      end

let rec read_diff prefix in_ch =
  let line_rm0 = input_line in_ch in
    ignore(input_line in_ch); (* Read +++ line *)
    let tab_re = Str.regexp "\t" in
    let line_rm = Str.global_replace tab_re " " line_rm0 in
    let rm_re = Str.regexp "^--- \\([^ ]+\\) .*$" in
    let rmb = Str.string_match rm_re line_rm 0 in
      if rmb then
	let rmfile = Str.matched_group 1 line_rm in
	let hunks = read_hunks in_ch in
	let tail =
	  try
	    read_diff prefix in_ch
	  with End_of_file -> []
	in
	let ver_file = Misc.strip_prefix prefix rmfile in
	  (ver_file, hunks)::tail
      else
	begin
	  prerr_endline "No more diff found";
	  []
	end

let parse_diff prefix file : Ast_diff.diffs =
  let in_ch = open_in file in
  try
    let ast = read_diff prefix in_ch in
      close_in in_ch;
      ast
  with
      (Unexpected msg) ->
	prerr_endline ("Unexpected token: "^msg);
	close_in in_ch;
	raise (Unexpected msg)

let compute_new_pos (diffs: Ast_diff.diffs) file ver pos =
  try
    let hunks = List.assoc (ver, file) diffs in
    let new_pos = List.fold_left
      (fun (line, colb, cole) ((bl,bsize),(al,asize)) ->
	 if line > bl then
	   (line + asize - bsize, colb, cole)
	 else
	   (line, colb, cole)
      ) pos hunks
    in new_pos
  with Not_found -> pos

let show_diff verbose ast =
  if verbose then
    List.iter (fun ((ver, file), hunks) ->
		 print_string file;
		 print_string " from ";
		 print_int ver;
		 print_string " to ";
		 print_int (ver +1);
		 print_endline "";
		 List.iter (fun ((bl,bsize),(al,asize)) ->
			      print_int bl;
			      print_string "(";
			      print_int bsize;
			      print_string ") -> ";
			      print_int al;
			      print_string "(";
			      print_int asize;
			      print_string "), "
			   ) hunks;
		 print_endline ""
	      ) ast
