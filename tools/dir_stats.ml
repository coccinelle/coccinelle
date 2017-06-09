(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

(* for each marked thing, how often does it occur and in what files and
directories *)

let collect i =
  let info = ref [] in
  let rec loop _ =
    let l = input_line i in
    (if String.length l > 2 && String.get l 0 = '+'
    then info := (String.sub l 1 (String.length l - 1))::!info);
    loop() in
  try loop()
  with End_of_file -> List.rev !info

let split l =
  let rec loop acc = function
    [] -> acc
  | x::xs ->
      if String.get x 0 = '+' (* the start of a new file *)
      then
	(match Str.split (Str.regexp " ") x with
	  _::x::_ -> loop ((x,[])::acc) xs
	| _ -> failwith ("no file: "^x))
      else
	let acc =
	  match acc with
	    (file,instances)::rest -> (file,x::instances)::rest
	  | _ -> failwith "not possible" in
	loop acc xs in
  let res = List.rev (loop [] l) in
  List.map (function (x,l) -> (x,List.rev l)) res

let detect_alloc_free str l =
  let try_add a f l =
    let (same,diff) = List.partition (function (a1,f1) -> a = a1) l in
    match same with
      [(a1,f1)] -> if List.mem f f1 then l else (a1,f::f1) :: diff
    | _ -> (a,[f])::l in
  let rec loop acc = function
      [] -> acc
    | x::xs ->
	match Str.split (Str.regexp (str^"\", ")) x with
	  _::matches ->
	    let acc =
	      List.fold_left
		(function acc ->
		  function rest ->
		    (match Str.split (Str.regexp "[, )]+") rest with
		      alloc::free::_ -> try_add alloc free acc
		    | _ -> acc))
		acc matches in
	    loop acc xs
	| _ -> loop acc xs in
  List.sort compare
    (List.map (function (a,f) -> (a,List.sort compare f)) (loop [] l))

let rec iterate str = function
    [] -> []
  | (x,l)::xs ->
      List.fold_left
	(function rest ->
	  function info ->
	    let (same,diff) =
	      List.partition (function (x1,l1) -> l1 = info) rest in
	    match same with
	      [(files,info)] -> (x::files,info)::diff
	    |	_ -> ([x],info)::diff)
	(iterate str xs) (detect_alloc_free str l)

(* ------------------------------------------------------------------------ *)

let get_dir d = Filename.dirname d

let get_subsystem d =
  let pieces = Str.split (Str.regexp "/") d in
  let front = List.hd(List.tl pieces) in
  match front with
    "arch" | "drivers" -> front ^ "/" ^ (List.hd(List.tl(List.tl pieces)))
  | _ -> front

let rec remdup = function
    [] -> []
  | x::xs -> if List.mem x xs then remdup xs else x :: remdup xs

let inc tbl key =
  let cell =
    (try let cell = Hashtbl.find tbl key in cell
    with Not_found -> let c = ref 0 in Hashtbl.add tbl key c; c) in
  cell := !cell + 1

let files_per_protocol = Hashtbl.create(10)
let dirs_per_protocol = Hashtbl.create(10)
let subsystems_per_protocol = Hashtbl.create(10)
let protocols_per_subsystem = Hashtbl.create(10)

let collect_counts l =
  List.iter
    (function (files,(a,fs)) ->
      let how_many_files = List.length files in
      let how_many_dirs = remdup (List.map get_dir files) in
      let how_many_subsystems = remdup (List.map get_subsystem files) in
      let ct =
	if how_many_files < 10
	then how_many_files
	else ((how_many_files / 10) * 10) in
      inc files_per_protocol ct;
      inc dirs_per_protocol (List.length how_many_dirs);
      inc subsystems_per_protocol (List.length how_many_subsystems);
      List.iter (inc protocols_per_subsystem) how_many_subsystems)
    l

let print_hashtable f tbl =
  let l =
    Hashtbl.fold
      (function key -> function vl -> function rest ->
	(key,!vl) :: rest)
      tbl [] in
  let l = List.sort compare l in
  List.iter
    (function (key,vl) ->
      Printf.printf "   "; f key; Printf.printf ": %d\n" vl)
    l

let print_range_int_hashtable range =
  print_hashtable
    (function x ->
      if x < range
      then Printf.printf "%d" x
      else Printf.printf "%d-%d" x (x + range - 1))
let print_int_hashtable =
  print_hashtable (function x -> Printf.printf "%d" x)
let print_string_hashtable =
  print_hashtable (function x -> Printf.printf "%s" x)

let histify _ =
  Printf.printf "files per protocol:\n";
  print_range_int_hashtable 10 files_per_protocol;
  Printf.printf "dirs per protocol:\n";
  print_int_hashtable dirs_per_protocol;
  Printf.printf "subsystems per protocol:\n";
  print_int_hashtable subsystems_per_protocol;
  Printf.printf "protocols per subsystem:\n";
  print_string_hashtable protocols_per_subsystem

(* ------------------------------------------------------------------------ *)

let dir = ref "p2"
let file = ref ""
let str = ref "detected allocator"

let options = []
let usage = ""

let _ =
  Arg.parse (Arg.align options) (fun x -> file := x) usage;
  let i = open_in !file in
  let l = collect i in
  close_in i;
  let l = split l in
  let l = iterate !str l in
  collect_counts l;
  histify()
