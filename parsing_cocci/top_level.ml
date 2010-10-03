(* Reorganize the top level of a rule to be a list of either top-level
declarations or code dots.  A function declaration is always considered top
level.  A statement is always considered code dots.  A variable declaration
is ambiguous.  We use the heuristic that if there are code dots somewhere
else, then the variable declaration is at top level, otherwise it applies
both at top level and to all code. *)

(* This is assumed to be done before compute_lines, and thus the info on a
complex term is assumed to be Ast0.default_info *)

module Ast0 = Ast0_cocci

let top_dots l =
  let circle x =
    match Ast0.unwrap x with Ast0.Circles(_) -> true | _ -> false in
  let star x =
    match Ast0.unwrap x with Ast0.Stars(_) -> true | _ -> false in
  if List.exists circle l
  then Ast0.wrap (Ast0.CIRCLES(l))
  else if List.exists star l
  then Ast0.wrap (Ast0.STARS(l))
  else Ast0.wrap (Ast0.DOTS(l))

let scan_code l =
  let statements = ref false in
  let rec loop = function
      [] -> ([],[])
    | (x::xs) as all ->
	(match Ast0.unwrap x with
	  (Ast0.OTHER(code)) ->
	    (match Ast0.unwrap code with
	      Ast0.Decl(_) ->
		let (front,rest) = loop xs in
		(code::front,rest)
	    | _ ->
		statements := true;
		let (front,rest) = loop xs in
		(code::front,rest))
	| _ -> ([],all)) in
  match loop l with
    ([],_) as res -> res
  | (code,rest) ->
      if !statements = true
      then ([Ast0.wrap(Ast0.CODE(top_dots code))],rest)
      else
	(List.map
	   (function d ->
	     match Ast0.unwrap d with
	       Ast0.Decl(bef,x) -> Ast0.wrap (Ast0.DECL(d))
	     | _ -> failwith "impossible")
	   code,
	 rest)

let rec scan_top_decl = function
    [] -> ([],[])
  | ((topdecl::rest) as all) ->
      (match Ast0.unwrap topdecl with
	Ast0.OTHER(_) -> ([],all)
      | _ -> let (front,rest) = scan_top_decl rest in (topdecl::front,rest))

(* for debugging *)
let l2c l =
  match Ast0.unwrap l with
    Ast0.DECL(_) -> "decl"
  | Ast0.CODE(_) -> "code"
  | Ast0.FILEINFO(_,_) -> "fileinfo"
  | Ast0.ERRORWORDS(_) -> "errorwords"
  | Ast0.OTHER(_) -> "other"

let rec top_level l =
  match scan_code l with
    (code,[]) -> code
  | (code,rest) ->
      (match scan_top_decl rest with
	(top_decls,[]) -> code@top_decls
      |	(top_decls,rest) -> code @ top_decls @ (top_level rest))
