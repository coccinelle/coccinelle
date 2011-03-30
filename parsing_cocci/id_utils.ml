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
	      Printf.sprintf
		"lid -f %s/.id-utils.index -l %s -S newline" dir oo in
	    Common.cmd_to_list cmd
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
