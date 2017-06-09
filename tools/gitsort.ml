(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

(* sort a list of git codes such that the most recent comes first *)

let git_home = ref "/home/julia/linux-2.6"

let unwind_protect f cleanup =
    try f ()
    with e -> begin cleanup e; raise e end

let (with_open_infile: string -> ((in_channel) -> 'a) -> 'a) = fun file f ->
  let chan = open_in file in
  unwind_protect (fun () ->
    let res = f chan in
    close_in chan;
    res)
    (fun e -> close_in chan)

(* ----------------------------------------------------------------------- *)

let months =
  [("Jan",1);("Feb",2);("Mar",3);("Apr",4);("May",5);("Jun",6);("Jul",7);
    ("Aug",8);("Sep",9);("Oct",10);("Nov",11);("Dec",12)]

let antimonths =
  [(1,31);(2,28);(3,31);(4,30);(5,31); (6,30);(7,31);(8,31);(9,30);(10,31);
    (11,30);(12,31);(0,31)]

let normalize (year,month,day,hour,minute,second) =
  if hour < 0
  then
    let (day,hour) = (day - 1,hour + 24) in
    if day = 0
    then
      let month = month - 1 in
      let day = List.assoc month antimonths in
      let day =
	if month = 2 && year / 4 * 4 = year && not (year / 100 * 100 = year)
	then 29
	else day in
      if month = 0
      then (year-1,12,day,hour,minute,second)
      else (year,month,day,hour,minute,second)
    else (year,month,day,hour,minute,second)
  else (year,month,day,hour,minute,second)

exception Fail of string

let read_info code =
  let _ =
    Sys.command
      (Printf.sprintf
	 "pushd %s >& /dev/null ; git log %s^..%s | grep Date: > /tmp/gitsort_info ; popd >& /dev/null"
	 !git_home code code) in
  with_open_infile "/tmp/gitsort_info" (fun i ->
    let l =
      try input_line i
      with End_of_file -> raise (Fail "bad git file") in
    match Str.split (Str.regexp " ") l with
      [date;_;_;weekday;month;day;time;year;offset] ->
	let day = int_of_string day in
	let month = List.assoc month months in
	let year = int_of_string year in
	(match Str.split (Str.regexp ":") time with
	  [hour;minute;second] ->
	    let hour = int_of_string hour in
	    let minute = int_of_string minute in
	    let second = int_of_string second in
	    let modifier =
	      match String.get offset 0 with
		'-' -> -1
	      |	'+' -> 1
	      |	_ -> raise (Fail "bad offset") in
	    (if not (String.sub offset 3 2 = "00")
	    then raise (Fail "require 0 minutes difference"));
	    let hour =
	      hour + (modifier * (int_of_string (String.sub offset 1 2))) in
	    normalize (year,month,day,hour,minute,second)
	| _ -> raise (Fail "bad date2"))
    | l -> raise (Fail ("bad date1: "^(String.concat "|" l))))

let rec get_dates = function
    [] -> []
  | code::rest ->
      let date =
	try Some (read_info code)
	with
	  Fail s -> Printf.printf "problem in %s: %s\n" code s; None
	| _ -> Printf.printf "problem in %s\n" code; None in
      match date with
	Some date -> (date,code)::(get_dates rest)
      |	None -> get_dates rest

let get_codes file =
  let gits = ref ([] : string list) in
  with_open_infile file (fun i ->
    let rec loop _ =
      let git = try Some (input_line i) with End_of_file -> None in
      match git with
	Some x -> gits := x :: !gits; loop()
      |	None -> () in
    loop ());
  List.concat
    (List.map
       (function l ->
	 List.filter
	   (* all because I don't know how to make a backslash regexp...*)
	   (function x -> String.length x > 10)
	   (Str.split (Str.regexp "[ \t]+") l))
       !gits)

let _ =
  let args = Array.to_list Sys.argv in
  let file =
    match args with
      [_;git_home_info;gits] -> git_home := git_home_info; gits
    | [_;gits] -> gits
    | _ -> failwith "args: [git home] git_codes_file" in
  let codes = get_codes file in
  let dates = get_dates codes in
  match List.sort compare dates with
    (_,last)::prev ->
      List.iter (function (_,x) -> Printf.printf "%s \\\n" x) (List.rev prev);
      Printf.printf "%s\n" last
  | _ -> ()
