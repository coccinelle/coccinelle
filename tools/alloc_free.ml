(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at https://coccinelle.gitlabpages.inria.fr/website/
 *)

(* The following finds out for each file, how it does deallocation for each
allocator *)

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
(* The following prints that information *)

let print_output l =
  List.iter
    (function (files,(a,fs)) ->
      List.iter (function x -> Printf.printf "%s\n" x) files;
      Printf.printf "   alloc: %s, free: %s\n" a (String.concat ", " fs);
      Printf.printf "\n")
    l

(* ------------------------------------------------------------------------ *)
(* The following makes a semantic patch for that information *)

let sedify o generic_file dir l =
  List.iter
    (function (files,(a,fs)) ->
      match fs with
	[f] ->
	  let _ =
	    Sys.command
	      (Printf.sprintf
		 "sed s/ALLOC/%s/ %s | sed s/FREE/%s/ > %s/%s-%s.cocci\n"
		 a generic_file f dir a f) in ()
      | _ -> ())
    l;
  List.iter
    (function (files,(a,fs)) ->
      match fs with
	[f] -> Printf.fprintf o "mono_spatch_linux %s-%s.cocci &\n" a f
      | _ -> ())
    l

let collect_allocs l =
  let union =
    List.fold_left
      (function rest -> function x ->
	if List.mem x rest then rest else x::rest) in
  List.fold_left
    (function rest ->
      function (files,(a,fs)) ->
	let (same,diff) =
	  List.partition (function (a1,fs1) -> a = a1) rest in
	match same with
	  [(a1,fs1)] -> (a,union fs fs1)::diff
	| [] -> (a,fs)::rest
	| _ -> failwith "not possible")
    [] l

let sedify_ors o generic_file dir l =
  let l = collect_allocs l in
  List.iter
    (function (a,fs) ->
      match fs with
	[_] | [] -> ()
      |	(f::_) ->
	  let sfs =
	    Printf.sprintf "\"\\\\\\(%s\\\\\\)\""
	      (String.concat "\\\\\\|" fs) in
	  let _ =
	    Sys.command
	      (Printf.sprintf
		 "sed s/ALLOC/%s/ %s | sed s/FREE/%s/ > %s/%s-%s_et_al.cocci\n"
		 a generic_file sfs dir a f) in ())
    l;
  List.iter
    (function (a,fs) ->
      match fs with
	[_] | [] -> ()
      |	(f::_) ->
	  Printf.fprintf o "mono_spatch_linux %s-%s_et_al.cocci &\n" a f)
    l

(* ------------------------------------------------------------------------ *)

let sed = ref false
let gen = ref "generic2.cocci"
let dir = ref "p2"
let file = ref ""
let str = ref "detected allocator"

let options = [ "-sed", Arg.Set sed, "sed output";
		"-sp", Arg.String (function x -> gen := x),
		"detection string";
		"-str", Arg.String (function x -> str := x),
		"cocci file for use with sed";
		"-dir", Arg.String (function x -> dir := x),
		"dir for sed output"; ]
let usage = ""

let _ =
  Arg.parse (Arg.align options) (fun x -> file := x) usage;
  let i = open_in !file in
  let l = collect i in
  close_in i;
  let l = split l in
  let l = iterate !str l in
  (if !sed
  then
    begin
      let o = open_out (Printf.sprintf "%s/files" !dir) in
      Printf.fprintf o "#! /bin/sh\n\n";
      sedify o !gen !dir l;
      sedify_ors o !gen !dir l;
      Printf.fprintf o "\nwait\n/bin/rm tmp*out\n";
      close_out o
    end);
  if not !sed then print_output l
