(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

(* evaluation of nested and and or using id-utils *)

module GC = Get_constants2

let evaluated = Hashtbl.create(101)

exception Out

let memoize exp vl = Hashtbl.add evaluated exp vl; vl

let interpret_query index exp =
  let rec loop exp =
    let res = try Some (Hashtbl.find evaluated exp) with Not_found -> None in
    match res with
      Some x -> x
    | None ->
	memoize exp
	  (match exp with
	    GC.Elem oo ->
	      let cmd =
		Printf.sprintf "lid -f %s -l %s -S newline" index oo in
		  (* lid puts the matched word at the beginning of the first
		     line of the output... *)
	      (match Common.cmd_to_list cmd with
		[] -> []
	      | x::xs ->
		  (match Str.split (Str.regexp "[ \t]+") x with
		    [oop;file] when oo = oop -> file :: xs
		  | _ ->
		      failwith(Printf.sprintf "unexpected output of %s" cmd)))
	  | GC.And l ->
	      let rec iloop = function
		  [] -> failwith "bad and"
		| [x] -> loop x
		| x :: xs ->
		    (match loop x with
		      [] -> raise Out
		    | resx ->
			let resxs = iloop xs in
			Common.inter_set resx resxs) in
	      (try iloop l with Out -> [])
	  | GC.Or l ->
	      List.fold_left
		(function prev -> function cur ->
		  Common.union_set (loop cur) prev)
		[] l
	  | GC.Not _ -> failwith "unexpected not"
	  | GC.False -> failwith "unexpected false"
	  | GC.True -> failwith "unexpected true") in
  loop exp

let rec interpret dir query =
  let index =
    try
      if String.get !Flag_parsing_cocci.id_utils_index 0 = '/'
      then !Flag_parsing_cocci.id_utils_index
      else
	Printf.sprintf "%s/%s" dir !Flag_parsing_cocci.id_utils_index
    with Invalid_argument _ -> failwith "empty idutils index name" in
  let res =
    if not (Sys.file_exists index)
    then
      (Common.pr2
	 (Printf.sprintf
	    "index %s unavailable, run scripts/idutils_index.sh from %s"
	    !Flag_parsing_cocci.id_utils_index dir);
       None)
    else
      match query with
	None -> Common.pr2 "no inferred idutils keywords"; None
      | Some exp -> Some (interpret_query index exp) in
  Hashtbl.reset evaluated;
  res
