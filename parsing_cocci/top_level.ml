(* Reorganize the top level of a rule to be a list of either top-level
declarations or code dots.  A function declaration is always considered top
level.  A statement is always considered code dots.  A variable declaration
is ambiguous.  We use the heuristic that if there are code dots somewhere
else, then the variable declaration is at top level, otherwise it applies
both at top level and to all code. *)

module Ast0 = Ast0_cocci

let mkres e (_,lstart) (_,lend) =
  (e,{Ast0.logical_start = lstart.Ast0.logical_start;
       Ast0.logical_end = lend.Ast0.logical_end})

let top_dots l =
  if List.exists (function (Ast0.Circles(_),_) -> true | _ -> false) l
  then mkres (Ast0.CIRCLES(l)) (List.hd l) (List.hd (List.rev l))
  else if List.exists (function (Ast0.Stars(_),_) -> true | _ -> false) l
  then mkres (Ast0.STARS(l)) (List.hd l) (List.hd (List.rev l))
  else mkres (Ast0.DOTS(l)) (List.hd l) (List.hd (List.rev l))

let scan_code l =
  let statements = ref false in
  let rec loop = function
      [] -> ([],[])
    | (Ast0.OTHER((Ast0.Decl(_),_) as code))::rest ->
	let (front,rest) = loop rest in
	(code::front,rest)
    | Ast0.OTHER(code)::rest ->
	statements := true;
	let (front,rest) = loop rest in
	(code::front,rest)
    | (top_decl::_) as rest -> ([],rest) in
  match loop l with
    ([],_) as res -> res
  | (code,rest) ->
      if !statements = true
      then ([Ast0.CODE(top_dots code)],rest)
      else
	(List.map
	   (function
	       (Ast0.Decl(x),_) -> Ast0.DECL x
	     | _ -> failwith "impossible")
	   code,
	 rest)

let rec scan_top_decl = function
    [] -> ([],[])
  | (Ast0.OTHER(_)::_) as rest -> ([],rest)
  | (topdecl::rest) ->
      let (front,rest) = scan_top_decl rest in
      (topdecl::front,rest)

let rec top_level l =
  match scan_code l with
    (code,[]) -> code
  | (code,rest) ->
      (match scan_top_decl l with
	(top_decls,[]) -> code@top_decls
      |	(top_decls,rest) -> code @ top_decls @ (top_level rest))
