(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

(* evaluation of nested and and or using id-utils *)

module GC = Get_constants2

let evaluated = Hashtbl.create(100)

exception Out

let memoize exp vl = Hashtbl.add evaluated exp vl; vl

let rec interpret dir exp =
  let res = try Some (Hashtbl.find evaluated exp) with Not_found -> None in
  match res with
    Some x -> x
  | None ->
      memoize exp
	(match exp with
	  GC.Elem oo ->
	    let cmd =
	      try
		let index =
		  if String.get !Flag_parsing_cocci.id_utils_index 0 = '/'
		  then !Flag_parsing_cocci.id_utils_index
		  else
		    Printf.sprintf "%s/%s" dir
		      !Flag_parsing_cocci.id_utils_index in
		Printf.sprintf "lid -f %s -l %s -S newline" index oo
	      with Invalid_argument _ -> failwith "empty idutils index name" in
	    (* lid puts the matched word at the beginning of the first line of
	       the output... *)
	    (match Common.cmd_to_list cmd with
	      [] -> []
	    | x::xs ->
		(match Str.split (Str.regexp "[ \t]+") x with
		  [oop;file] when oo = oop ->
		    file :: xs
		| _ -> failwith (Printf.sprintf "unexpected output of %s" cmd)))
	| GC.And l ->
	    let rec loop = function
		[] -> failwith "bad and"
	      | [x] -> interpret dir x
	      | x :: xs ->
		  (match interpret dir x with
		    [] -> raise Out
		  | resx ->
		      let resxs = loop xs in
		      Common.inter_set resx resxs) in
	    (try loop l with Out -> [])
	| GC.Or l ->
	    List.fold_left
	      (function prev -> function cur ->
		Common.union_set (interpret dir cur) prev)
	      [] l
	| _ -> failwith "not possible")
