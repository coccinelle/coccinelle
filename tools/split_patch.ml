(*
 * Copyright 2010, INRIA, University of Copenhagen
 * Julia Lawall, Rene Rydhof Hansen, Gilles Muller, Nicolas Palix
 * Copyright 2005-2009, Ecole des Mines de Nantes, University of Copenhagen
 * Yoann Padioleau, Julia Lawall, Rene Rydhof Hansen, Henrik Stuart, Gilles Muller, Nicolas Palix
 * This file is part of Coccinelle.
 *
 * Coccinelle is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, according to version 2 of the License.
 *
 * Coccinelle is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Coccinelle.  If not, see <http://www.gnu.org/licenses/>.
 *
 * The authors reserve the right to distribute this or future versions of
 * Coccinelle under other licenses.
 *)


(* split patch per file *)

let git_tree = "/var/linuxes/linux-next"

let maintainer_command file =
  Printf.sprintf
    "cd %s; scripts/get_maintainer.pl --separator , --nomultiline --nogit -f %s"
    git_tree file

let subsystem_command file =
  Printf.sprintf
    "cd %s; scripts/get_maintainer.pl --nogit --subsystem -f %s | grep -v @"
    git_tree file

let checkpatch_command file =
  Printf.sprintf "cd %s; scripts/checkpatch.pl %s" git_tree file

let default_string = "THE REST" (* split by file *)

let extra_cc = Some "kernel-janitors@vger.kernel.org" (* comma separated *)

let prefix_before = Some "/var/linuxes/linux-next"
let prefix_after = Some "/var/julia/linuxcopy"

(* ------------------------------------------------------------------------ *)

let spaces = Str.regexp "[ \t]"

let fix_before_after l prefix = function
    Some old_prefix ->
      (match Str.split spaces l with
	("diff"|"+++"|"---")::_ ->
	  (match Str.split (Str.regexp old_prefix) l with
	    [a;b] ->
	      (match Str.split_delim (Str.regexp ("[ \t]"^prefix)) a with
		[_;""] -> a^b (* prefix is alwaredy there *)
	      |	_ -> a^prefix^b)
	  | _ -> l)
      |	_ -> l)
  | _ -> l

let fix_date l =
  match Str.split spaces l with
    (("+++"|"---") as a)::path::rest -> Printf.sprintf "%s %s" a path
  | _ -> l

(* ------------------------------------------------------------------------ *)

let is_diff = Str.regexp "diff "
let split_patch i =
  let patches = ref [] in
  let cur = ref [] in
  let get_size l =
    match Str.split_delim (Str.regexp ",") l with
      [_;size] -> int_of_string size
    | _ -> failwith ("bad size: "^l) in
  let rec read_diff_or_atat _ =
    let l = input_line i in
    let l = fix_date(fix_before_after l "a" prefix_before) in
    let l = fix_date(fix_before_after l "b" prefix_after) in
    match Str.split spaces l with
      "diff"::_ ->
	(if List.length !cur > 0
	then patches := List.rev !cur :: !patches);
	cur := [l];
	read_diff()
    | "@@"::min::pl::"@@"::rest ->
	let msize = get_size min in
	let psize = get_size pl in
	cur := l :: !cur;
	read_hunk msize psize
    | "\\"::_ -> cur := l :: !cur; read_diff_or_atat()
    | _ ->
	failwith
	  "expected diff or @@ (diffstat information should not be present)"
  and read_diff _ =
    let l = input_line i in
    let l = fix_date(fix_before_after l "a" prefix_before) in
    let l = fix_date(fix_before_after l "b" prefix_after) in
    cur := l :: !cur;
    match Str.split spaces l with
      "+++"::_ -> read_diff_or_atat()
    | _ -> read_diff()
  and read_hunk msize psize =
    if msize = 0 && psize = 0
    then read_diff_or_atat()
    else
      let l = input_line i in
      cur := l :: !cur;
      match String.get l 0 with
	'-' -> read_hunk (msize - 1) psize
      |	'+' -> read_hunk msize (psize - 1)
      |	_ -> read_hunk (msize - 1) (psize - 1) in
  try read_diff_or_atat()
  with End_of_file -> List.rev ((List.rev !cur)::!patches)

(* ------------------------------------------------------------------------ *)

(* can get_maintainers take a file as an argument, or only a patch? *)
let resolve_maintainers patches =
  let maintainer_table = Hashtbl.create (List.length patches) in
  List.iter
    (function
	diff_line::rest ->
	  (match Str.split (Str.regexp " a/") diff_line with
	    [before;after] ->
	      (match Str.split spaces after with
		file::_ ->
		  let maintainers =
		    List.hd (Common.cmd_to_list (maintainer_command file)) in
		  let maintainers =
		    match extra_cc with
		      None -> maintainers
		    | Some extra_cc -> maintainers ^ "," ^ extra_cc in
		  let subsystems =
		    Common.cmd_to_list (subsystem_command file) in
		  let info = (subsystems,maintainers) in
		  let cell =
		    try Hashtbl.find maintainer_table info
		    with Not_found ->
		      let cell = ref [] in
		      Hashtbl.add maintainer_table info cell;
		      cell in
		  cell := (file,(diff_line :: rest)) :: !cell
	      |	_ -> failwith "filename not found")
	  | _ ->
	      failwith (Printf.sprintf "prefix a/ not found in %s" diff_line))
      |	_ -> failwith "bad diff line")
    patches;
  maintainer_table

(* ------------------------------------------------------------------------ *)

let common_prefix l1 l2 =
  let rec loop = function
      ([],_) | (_,[]) -> []
    | (x::xs,y::ys) when x = y -> x :: (loop (xs,ys))
    | _ -> [] in
  match loop (l1,l2) with
    [] ->
      failwith
	(Printf.sprintf "found nothing in common for %s and %s"
	   (String.concat "/" l1) (String.concat "/" l2))
  | res -> res

let merge_files the_rest files =
  let butlast l = if the_rest then l else List.rev(List.tl(List.rev l)) in
  match List.map (function s -> Str.split (Str.regexp "/") s) files with
    first::rest ->
      let rec loop res = function
	  [] -> String.concat "/" res
	| x::rest -> loop (common_prefix res x) rest in
      loop (butlast first) rest
  | _ -> failwith "not possible"

(* ------------------------------------------------------------------------ *)

let print_all o l =
  List.iter (function x -> Printf.fprintf o "%s\n" x) l

let make_output_files template maintainer_table patch =
  let ctr = ref 0 in
  let elements =
    Hashtbl.fold
      (function (services,maintainers) ->
	function diffs ->
	  function rest ->
	    if services=[default_string]
	    then
	      (* if no maintainer, then one file per diff *)
	      (List.map
		 (function (file,diff) ->
		   ctr := !ctr + 1;
		   (!ctr,true,maintainers,[file],[diff]))
		 (List.rev !diffs)) @
	      rest
	    else
	      begin
		ctr := !ctr + 1;
		let (files,diffs) = List.split (List.rev !diffs) in
		(!ctr,false,maintainers,files,diffs)::rest
	      end)
      maintainer_table [] in
  let number = List.length elements in
  List.iter
    (function (ctr,the_rest,maintainers,files,diffs) ->
      let output_file = Printf.sprintf "%s%d" patch ctr in
      let o = open_out output_file in
      Printf.fprintf o "To: %s\n\n" maintainers;
      Printf.fprintf o "Subject: [PATCH %d/%d] %s: "
	ctr number (merge_files the_rest files);
      print_all o template;
      let (nm,o1) = Filename.open_temp_file "patch" "patch" in
      List.iter (print_all o1) (List.rev diffs);
      close_out o1;
      let diffstat =
	Common.cmd_to_list
	  (Printf.sprintf "diffstat -p1 < %s ; /bin/rm %s" nm nm) in
      List.iter (print_all o) [diffstat];
      Printf.fprintf o "\n";
      List.iter (print_all o) diffs;
      close_out o;
      let (info,stat) =
	Common.cmd_to_list_and_status
	  (checkpatch_command ((Sys.getcwd())^"/"^output_file)) in
      if not(stat = Unix.WEXITED 0)
      then (print_all stderr info; Printf.fprintf stderr "\n"))
    (List.rev elements);
  let later = Printf.sprintf "%s%d" patch (number + 1) in
  if Sys.file_exists later
  then Printf.fprintf stderr "Warning: %s and other files may be left over from a previous run\n" later
    
(* ------------------------------------------------------------------------ *)

let file = ref ""

let options = []

let usage = ""

let anonymous x = file := x

let _ =
  Arg.parse (Arg.align options) (fun x -> file := x) usage;
  let i = open_in !file in
  let patches = split_patch i in
  close_in i;
  let maintainer_table = resolve_maintainers patches in
  let template = Common.cmd_to_list (Printf.sprintf "cat %s.tmp" !file) in
  make_output_files template maintainer_table !file
