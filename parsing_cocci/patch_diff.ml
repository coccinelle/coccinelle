open Printf
open Str

(*Breakdown split_result_list type to primtive type*)
let rec decompose_list (l: Str.split_result list)  =
  match l with
  | [] -> []
  | Text hd :: tl ->  decompose_list tl
  | Delim hd :: tl -> hd ::  decompose_list tl

let get_root fpath =
  let current_dir = List.hd (Common.cmd_to_list "pwd") in
  let command_get_root = "cd " ^ fpath ^ " && git rev-parse --show-toplevel" in
  let root = List.hd (Common.cmd_to_list command_get_root) in
  ignore (Sys.command ("cd " ^ current_dir));
  root

  let extract_numbers line =
  let sep = String.split_on_char ',' line in
  match sep with
  | [l] -> (int_of_string l, int_of_string l)
  | [f ; e] -> (int_of_string f, int_of_string e)
  | _ -> failwith "no line numbers found, sorry"

let fetch_file_name line =
  let pat_filename = Str.regexp "\\(+++ b\\)/\\(.+\\)\\.[a-z]+" in
  let s = Str.full_split pat_filename line in
  decompose_list s

let fetch_line_number line  =
  let pat_line_num = Str.regexp "\\(\\+\\([0-9]+,[0-9]+\\)\\)" in
  let s = Str.full_split pat_line_num line in
  let fa = List.hd (decompose_list s) in
  let (b, e) = extract_numbers fa in
  (b, (b + e)) (*start of diff, end of diff *)

type diff_info =
{
    file_name: string;
    line_no: (int * int) list;
}

type patch_info = No_info | File_info of string list | Line_info of (int * int)

(*check line and extract file name or file number*)
let extract_info line =
  if Str.string_match (Str.regexp_string "+++") line 0
  then File_info (fetch_file_name line)
  else if Str.string_match (Str.regexp_string "@@ ") line 0
  then Line_info (fetch_line_number line)
  else No_info

let rec reorg_helper new_list = function
    [] -> (List.rev new_list, [])
  | No_info :: tl -> reorg_helper new_list tl
  | File_info file_list :: tl -> (List.rev new_list, File_info file_list :: tl)
  | Line_info line_list :: tl ->  reorg_helper (line_list :: new_list) tl

let rec reorg fpath toproot = function
    [] -> []
  | No_info :: tl -> reorg fpath toproot tl
  | File_info [file] :: tl ->
      let lines, rest = reorg_helper [] tl in
      {
      file_name = Str.replace_first (Str.regexp "+++ b") toproot file;
      line_no = lines;
    } :: reorg fpath toproot rest
  | File_info file_list :: tl ->
      let lines, rest = reorg_helper [] tl in
      reorg fpath toproot rest
  | Line_info line_list :: tl ->  failwith "bad case"

let rec mlines lines =
  match lines with
  | [] -> []
  | hd :: tl -> extract_info hd :: mlines tl

let final_info dir =
  let git_root = get_root dir in
  let git_read_command =
    "cd " ^ git_root ^ " && git diff " ^ dir ^ " | egrep '^+++|^@'" in
  let list_diff =
    reorg dir git_root (mlines (Common.cmd_to_list git_read_command)) in
  list_diff

let final_patch_info dir startid endid =
  let ver = List.hd (Common.cmd_to_list "git --version | cut -d' ' -f3") in
  let recent =
    try
      let pieces =
	List.map int_of_string (Str.split (Str.regexp_string ".") ver) in
      match pieces with
	[v1;v2;v3] -> v1 > 2 || v2 >= 30
      | _ -> false
    with _ -> false in
  let git_root = get_root dir in
  let git_read_command =
    let extra =
      if recent
      then "--merge-base"
      else "" in
    Printf.sprintf " cd %s && git diff %s %s %s | egrep '^+++|^@'"
      git_root extra startid endid in
  let list_diff =
    reorg dir git_root (mlines (Common.cmd_to_list git_read_command)) in
  list_diff

let rec print_tuple_list l =
  match l with
  | [] -> ()
  | (a,b) :: tl -> printf "\n%d, %d" a b; print_tuple_list tl

let getdiff dir = final_info dir

let getpatchdiff dir startid endid = final_patch_info dir startid endid

let split s =
  match Str.split (Str.regexp_string "..") s with
    [a;b] -> (a,b)
  | [a] -> (a^"^",a)
  | _ -> failwith ("invalid range specification: "^s)
