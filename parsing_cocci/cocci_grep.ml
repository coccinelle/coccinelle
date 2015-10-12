(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

(* input should be in CNF, ie an outer list, representing a conjunction,
with inner lists of words, representing disjunctions.  There is no negation. *)

let interpret_clause re l =
  try let _ = Str.search_forward re l 0 in true
  with Not_found -> false

let interpret (big_regexp,regexps) file =
  let i = try open_in file with _ -> raise (Flag.UnreadableFile file) in
  let simple = match regexps with [_] -> true | _ -> false in
  let rec loop big_regexp simple regexps =
    let l = input_line i in
    if interpret_clause big_regexp l
    then
      if simple
      then true
      else
	let res =
	  List.partition (function regexp -> interpret_clause regexp l)
	    regexps in
	match res with
	  (_,[]) -> true
	| (_,([regexp] as regexps)) -> loop regexp true regexps
	| (_,regexps) -> loop big_regexp simple regexps
    else loop big_regexp simple regexps in
  let res = try loop big_regexp simple regexps with End_of_file -> false in
  close_in i;
  res

(* --------------------------------------------------------------------- *)
(* Helpers for get_constants2 *)

let count_atoms l =
  let tbl = Hashtbl.create 101 in
  let add x =
    let cell =
      try Hashtbl.find tbl x
      with Not_found ->
	let cell = ref 0 in
	Hashtbl.add tbl x cell;
	cell in
    cell := !cell + 1 in
  List.iter (List.iter add) l;
  let lst =
    Hashtbl.fold (fun element ct rest -> (!ct,element)::rest) tbl [] in
  let lst = List.sort compare lst in
(*
  Printf.printf "table\n";
  List.iter
    (function (ct,ele) -> Printf.printf "%d: %s\n" ct ele)
    lst;
*)
  List.map (fun (ct,element) -> (element,ct)) lst

let subset l1 l2 = List.for_all (fun e1 -> List.mem e1 l2) l1

let flatten ls =
  List.fold_left (fun res elem -> Common.union_set elem res) [] ls

let extend element res available =
  let (added,available) = List.partition (List.mem element) available in
  let added = flatten added in
  if added = []
  then (res,available)
  else
    begin
      let added = Common.union_set added res in
      (added,
       List.fold_left
	 (function available ->
	   function clause ->
	     if subset clause added
	     then available
	     else clause :: available)
	 [] available)
    end

let indexify l =
  let rec loop n = function
      [] -> []
    | x :: xs -> (x,n) :: loop (n+1) xs in
  loop 0 l

let split l =
(*
  Printf.printf "formula\n";
  List.iter
    (fun clause -> Printf.printf "%s\n" (String.concat " " clause))
    l;
  Printf.printf "\n";
*)
  let tbl = count_atoms l in
  let (pretbl,tbl) =
    List.partition (function (_,1) -> true | _ -> false) tbl in
  let (preres,available) =
    List.fold_left
      (function (preres,available) ->
	function (f,ct) ->
	  let (res,available) = extend f [] available in
	  match res with
	    [] -> (preres,available)
	  | _ -> (res::preres,available))
      ([],l) pretbl in
  let tbl = indexify tbl in
  let rec loop front back leftres rightres = function
      [] -> (leftres,rightres)
    | available ->
	match (front,back) with
	  ((((f,_),nf)::front),(((b,_),nb)::back)) ->
	    if nf < nb
	    then
	      let (leftres,available) = extend f leftres available in
	      let (rightres,available) = extend b rightres available in
	      loop front back leftres rightres available
	    else if nf = nb
	    then (Common.union_set (flatten available) leftres,rightres)
	    else (leftres,rightres)
	| ([],[]) -> (leftres,rightres)
	| _ -> failwith "not possible" in
  let (a,b) = loop tbl (List.rev tbl) [] [] available in
  (* discard empties *)
  let a = match a with [] -> [] | _ -> [a] in
  let b = match b with [] -> [] | _ -> [b] in
  let res = a@b@preres in
(*List.iter
    (function a ->
      Printf.printf "*** %s\n" (String.concat " " a))
    res; *)
  res
