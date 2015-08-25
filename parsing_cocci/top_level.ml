(* Reorganize the top level of a rule to be a list of either top-level
declarations or code dots.  A function declaration is always considered top
level.  A statement is always considered code dots.  A variable declaration
is ambiguous.  We use the heuristic that if there are code dots somewhere
else, then the variable declaration is at top level, otherwise it applies
both at top level and to all code. *)

(* This is assumed to be done before compute_lines, and thus the info on a
complex term is assumed to be Ast0.default_info *)

module Ast0 = Ast0_cocci

let top_dots l = Ast0.wrap l

let rec is_decl s =
  match Ast0.unwrap s with
    Ast0.Decl(_,e) -> true
  | _ -> false

let isonly f l = match Ast0.unwrap l with [s] -> f s | _ -> false
let isall f l = List.for_all (isonly f) l

let rec is_toplevel s =
  match Ast0.unwrap s with
    Ast0.Decl(_,e) -> true
  | Ast0.FunDecl(_,_,_,_,_,_,_,_,_,_,_) -> true
  | Ast0.Disj(_,stmts,_,_) -> isall is_toplevel stmts
  | Ast0.ExprStatement(Some fc,_) -> false
  | Ast0.Include(_,_) -> true
  | Ast0.Undef(_,_) -> true
  | Ast0.Define(_,_,_,_) -> true
  | Ast0.Pragma(_,_,_) -> true
  | _ -> false

let scan_code must_be_code l =
  let rec loop = function
      [] -> ([],[])
    | (x::xs) as all ->
        (match Ast0.unwrap x with
          (Ast0.OTHER(code)) ->
            let (front,rest) = loop xs in
            (code::front,rest)
        | _ -> ([],all)) in
  match loop l with
    ([],_) as res -> res
  | (code,rest) ->
      (match code with
      | [x] when is_decl x && must_be_code ->
	  ([Ast0.wrap(Ast0.NONDECL x)],rest)
      | _ when List.for_all is_toplevel code ->
	  ([Ast0.wrap(Ast0.TOPCODE(top_dots code))],rest)
      | _ ->
	  ([Ast0.wrap(Ast0.CODE(top_dots code))],rest))

let rec scan_top_decl = function
    [] -> ([],[])
  | ((topdecl::rest) as all) ->
      (match Ast0.unwrap topdecl with
	Ast0.OTHER(_) -> ([],all)
      | _ ->
	  let (front,rest) = scan_top_decl rest
	  in (topdecl::front,rest))

(* for debugging *)
let l2c l =
  match Ast0.unwrap l with
    Ast0.NONDECL(_) -> "decl"
  | Ast0.CODE(_) -> "code"
  | Ast0.TOPCODE(_) -> "code"
  | Ast0.FILEINFO(_,_) -> "fileinfo"
  | Ast0.ERRORWORDS(_) -> "errorwords"
  | Ast0.OTHER(_) -> "other"

let rec top_level must_be_code l =
  match scan_code must_be_code l with
    (code,[]) -> code
  | (code,rest) ->
      (match scan_top_decl rest with
	(top_decls,[]) -> code@top_decls
      |	(top_decls,rest) -> code @ top_decls @ (top_level must_be_code rest))

let clean l =
  List.map
    (function tl ->
      match Ast0.unwrap tl with
	Ast0.TOPCODE x -> Ast0.rewrap tl (Ast0.CODE x)
      |	_ -> tl)
    l
