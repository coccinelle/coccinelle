(*
 * This file is part of Coccinelle, licensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

module V0 = Visitor_ast0
module VT0 = Visitor_ast0_types
module Ast0 = Ast0_cocci
module Ast = Ast_cocci

(* Detects where position variables can be present in the match of an
isomorphism.  This is allowed if all elements of an isomorphism have only
one token or if we can somehow match up equal tokens of all of the
isomorphic variants. *)

type mcodeinfo =
    MetaName of Ast.meta_name
  | Str of string
  | Cst of Ast.constant
  | SAOp of Ast0.simpleAssignOp
  | FixOp of Ast.fixOp
  | UnOp of Ast.unaryOp
  | ArOp of Ast.arithOp
  | LogOp of Ast.logicalOp
  | CV of Ast.const_vol
  | Sign of Ast.sign
  | Struct of Ast.structUnion
  | Store of Ast.storage
  | Inc of Ast.inc_file

let sequence_tokens =
  let mcode f x =
    (* have to convert the mcodes to a common type so that we can make a list
       out of them *)
    [(f x,Ast0.get_pos_ref x)] in
  let donothing r k e = k e in
  let bind x y = x @ y in
  let option_default = [] in
  V0.flat_combiner bind option_default
    (mcode (function x -> MetaName (Ast0.unwrap_mcode x)))
    (mcode (function x -> Str (Ast0.unwrap_mcode x)))
    (mcode (function x -> Cst (Ast0.unwrap_mcode x)))
    (mcode (function x -> SAOp (Ast0.unwrap_mcode x)))
    (mcode (function x -> ArOp (Ast0.unwrap_mcode x)))
    (mcode (function x -> FixOp (Ast0.unwrap_mcode x)))
    (mcode (function x -> UnOp (Ast0.unwrap_mcode x)))
    (mcode (function x -> ArOp (Ast0.unwrap_mcode x)))
    (mcode (function x -> LogOp (Ast0.unwrap_mcode x)))
    (mcode (function x -> CV (Ast0.unwrap_mcode x)))
    (mcode (function x -> Sign (Ast0.unwrap_mcode x)))
    (mcode (function x -> Struct (Ast0.unwrap_mcode x)))
    (mcode (function x -> Store (Ast0.unwrap_mcode x)))
    (mcode (function x -> Inc (Ast0.unwrap_mcode x)))
    donothing donothing donothing donothing donothing donothing donothing
    donothing donothing donothing donothing donothing donothing donothing
    donothing donothing donothing donothing donothing donothing donothing
    donothing donothing donothing donothing donothing

(* In general, we will get a list of lists:

[[tokens1;tokens2;tokens3];[tokens4;tokens5;tokens6];[tokens7;tokens8]]

If all of the lists of tokens contain only one element, we are done.

Otherwise, we focus on tokens1.  For each of its elements, if they are
present in all of the others, then a position is assigned, and if not then
a position is not.  The order of the elements in the other lists is
irrelevant; we just take the first unannotated element that matches.  Once
we are done with the elements of tokens1, we skip to tokens 4 and repeat,
including considering the one-element special case. *)

let pctr = ref 0
let get_p _ =
  let c = !pctr in
  pctr := c + 1;
  let name = ("",Printf.sprintf "p%d" c) in
  (* pos var just gives a name we can look up, used for historical reasons *)
  Ast0.HiddenVarTag
    ([Ast0.MetaPosTag(Ast0.MetaPos(Ast0.make_mcode name,Ast.CstrTrue,Ast.PER))])

let process_info l =
   let rec loop previously_used = function
       [] -> ()
     | ((f::r)::xs) as a ->
	 let safe_add p pos =
	   (* don't add pos var where a pos var is already present *)
	   if Common.inter_set previously_used pos = [] then p::pos else pos in
	 let new_previously_used =
	   if List.for_all (List.for_all (function e -> List.length e = 1)) a
	   then
	     let p = get_p() in
             List.iter
	       (List.iter
		  (List.iter (function (_,pos) -> pos := safe_add p !pos)))
	       a;
	     p::previously_used
	   else
	     let all = r @ List.concat xs in
	     let rec find_first_available a previously_used = function
		 [] -> raise Not_found
	       | (str,pos)::xs ->
		   if str = a && Common.inter_set previously_used !pos = []
		   then pos
		   else find_first_available a previously_used xs in
	     List.fold_left
	       (function prev ->
		 function (str,pos) ->
		   if Common.inter_set previously_used !pos = []
		   then
		     try
		       let entries =
			 List.map (find_first_available str prev) all in
		       let p = get_p() in
		       pos := p::!pos;
		       List.iter (function pos -> pos := p :: !pos) entries;
		       p::prev
		     with Not_found -> prev
		   (* otherwise already annotated *)
		   else prev)
	       previously_used f in
	 loop new_previously_used xs
     | _ -> failwith "bad iso" in
   loop l

(* Entry point *)

let process (metavars,alts,name) =
  let toks =
    List.map (List.map sequence_tokens.VT0.combiner_rec_anything) alts in
  process_info [] toks
