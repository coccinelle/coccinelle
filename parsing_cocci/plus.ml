(*
 * This file is part of Coccinelle, lincensed under the terms of the GPL v2.
 * See copyright.txt in the Coccinelle source code for more information.
 * The Coccinelle source code can be obtained at http://coccinelle.lip6.fr
 *)

(* The plus fragments are converted to a list of lists of lists.
Innermost list: Elements have type anything.  For any pair of successive
elements, n and n+1, the ending line of n is the same as the starting line
of n+1.
Middle lists: For any pair of successive elements, n and n+1, the ending
line of n is one less than the starting line of n+1.
Outer list: For any pair of successive elements, n and n+1, the ending
line of n is more than one less than the starting line of n+1. *)

(* For nests and disjs, we are relying on the fact that <... ...> ( | )
must appear on lines by themselves, meaning that the various + fragments
can't be contiguous to each other or to unrelated things. *)

module Ast = Ast_cocci
module V = Visitor_ast

(* --------------------------------------------------------------------- *)

type res =
    Open of Ast.anything * int * int * int * int
  | Closed of (Ast.anything * int * int * int * int) list

let mcode fn = function
    (term, Ast.PLUS(info)) ->
      let line = info.Ast.line in
      let lline = info.Ast.logical_line in
      [Open (fn term,line,line,lline,lline)]
  | _ -> [Closed []]

let mk_fullType x         = Ast.FullTypeTag x
let mk_baseType x         = Ast.BaseTypeTag x
let mk_structUnion x      = Ast.StructUnionTag x
let mk_sign x             = Ast.SignTag x
let mk_ident x            = Ast.IdentTag x
let mk_expression x       = Ast.ExpressionTag x
let mk_constant x         = Ast.ConstantTag x
let mk_unaryOp x          = Ast.UnaryOpTag x
let mk_assignOp x         = Ast.AssignOpTag x
let mk_fixOp x            = Ast.FixOpTag x
let mk_binaryOp x         = Ast.BinaryOpTag x
let mk_arithOp x          = Ast.ArithOpTag x
let mk_logicalOp x        = Ast.LogicalOpTag x
let mk_declaration x      = Ast.DeclarationTag x
let mk_storage x          = Ast.StorageTag x
let mk_rule_elem x        = Ast.Rule_elemTag x
let mk_const_vol x        = Ast.ConstVolTag x
let mk_token x            = Ast.Token x

let get_real_start = function
    Open (_,line,_,_,_) -> line
  | _ -> failwith "not possible"

let get_real_finish = function
    Open (_,_,line,_,_) -> line
  | _ -> failwith "not possible"

let get_start = function
    Open (_,_,_,line,_) -> line
  | _ -> failwith "not possible"

let get_finish = function
    Open (_,_,_,_,line) -> line
  | _ -> failwith "not possible"

let get_option fn = function
    None -> []
  | Some x -> [fn x]

(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
(* Step 1: coalesce + terms, record starting and ending line numbers *)

let rec close l =
  let rec loop = function
      [] -> []
    | Open(x,start,finish,lstart,lfinish)::rest ->
	(x,start,finish,lstart,lfinish)::(loop rest)
    | (Closed l)::rest -> l @ (loop rest) in
  Closed (loop l)

let test term subterms =
  if List.for_all (function Open(_,_,_,_,_) -> true | _ -> false) subterms
  then [Open(term,
	    get_real_start (List.hd subterms),
	    get_real_finish (List.hd (List.rev subterms)),
	    get_start (List.hd subterms),
	    get_finish (List.hd (List.rev subterms)))]
  else [close subterms]

(* --------------------------------------------------------------------- *)
(* Dots *)

let dots recursor k dotlist = [close (k dotlist)]

(* --------------------------------------------------------------------- *)
(* Identifier *)

let ident recursor k i = test (Ast.IdentTag i) (k i)

(* --------------------------------------------------------------------- *)
(* Expression *)

let expression recursor k = function
    Ast.DisjExpr(exps) ->
      [close (List.concat(List.map recursor.V.combiner_expression exps))]
  | Ast.Edots(_,_)    -> [Closed []] (* must be context *)
  | Ast.Ecircles(_,_) -> [Closed []] (* must be context *)
  | Ast.Estars(_,_)   -> [Closed []] (* must be context *)
  | Ast.OptExp(_) | Ast.UniqueExp(_) | Ast.MultiExp(_) -> failwith "impossible"
  | e -> test (Ast.ExpressionTag e) (k e)

(* --------------------------------------------------------------------- *)
(* Types *)

and fullType recursor k ft = test (Ast.FullTypeTag ft) (k ft)

and typeC recursor k t = k t

(* --------------------------------------------------------------------- *)
(* Variable declaration *)
(* Even if the Cocci program specifies a list of declarations, they are
   split out into multiple declarations of a single variable each. *)

let declaration recursor k d = test (Ast.DeclarationTag d) (k d)

(* --------------------------------------------------------------------- *)
(* Parameter *)

let parameterTypeDef recursor k = function
    Ast.Pdots(_)             -> [Closed []]
  | Ast.Pcircles(_)          -> [Closed []]
  | p -> test (Ast.ParameterTypeDefTag p) (k p)

(* --------------------------------------------------------------------- *)
(* Top-level code *)

let rec rule_elem recursor k re = test (Ast.Rule_elemTag re) (k re)

let rec statement recursor k = function
    Ast.Disj(stmt_dots_list) ->
      [close
	  (List.concat
	     (List.map recursor.V.combiner_statement_dots stmt_dots_list))]
  | Ast.Dots(_,_,_)    -> [Closed []]
  | Ast.Circles(_,_,_) -> [Closed []]
  | Ast.Stars(_,_,_)   -> [Closed []]
  | s -> test (Ast.StatementTag s) (k s)

let rec meta recursor k m = test (Ast.MetaTag m) (k m)

let top_level recursor k = function
    Ast.FILEINFO(_,_) -> [Closed []]
  | Ast.ERRORWORDS(exps) -> [Closed []]
  | t -> test (Ast.Code t) (k t)

let anything recursor k a = failwith "not called"

let collect_tokens =
  let recursor =
    V.combiner (@) []
      (mcode mk_token) (mcode mk_constant) (mcode mk_assignOp) (mcode mk_fixOp)
      (mcode mk_unaryOp) (mcode mk_binaryOp) (mcode mk_const_vol)
      (mcode mk_baseType) (mcode mk_sign) (mcode mk_structUnion)
      (mcode mk_storage) dots dots dots
      ident expression fullType typeC parameterTypeDef declaration
      rule_elem statement meta top_level anything in
  recursor.V.combiner_top_level

let rule code = List.concat(List.map collect_tokens code)

(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
(* Step 2: find neighbors *)

let rec find_neighbors = function
    [] -> []
  | (x1,real_start1,real_finish1,start1,finish1)::rest ->
      (match find_neighbors rest with
	((((x2,real_start2,real_finish2,start2,finish2)::
	   rest_inner)::rest_middle)::rest_outer)
	  as rest ->
	    if finish1 = start2
	    then
	      ((((x1,real_start1,real_finish1,start1,finish1)::
		 (x2,real_start2,real_finish2,start2,finish2)::rest_inner)::
		rest_middle)::
	       rest_outer)
	    else if finish1 + 1 = start2
	    then
	      (([(x1,real_start1,real_finish1,start1,finish1)]::
		((x2,real_start2,real_finish2,start2,finish2)::rest_inner)::
		rest_middle)::
	       rest_outer)
	    else [[(x1,real_start1,real_finish1,start1,finish1)]]::rest
      |	_ -> [[[(x1,real_start1,real_finish1,start1,finish1)]]])
      (* rest must be [] *)

(* --------------------------------------------------------------------- *)
(* --------------------------------------------------------------------- *)
(* Entry point *)

let plus ast =
  match close (rule ast) with
    Closed l -> find_neighbors l
  | _ -> failwith "impossible"
