module TH = Token_helpers

let names = ref ([] : (string * int ref) list)

(* ----------------------------------------------------------------------- *)
(* drop tokens representing the function header and the final close brace *)

let drop_header_toks toks_e =
  let remove t =
    if not (TH.is_comment_or_space t)
    then
      (TH.info_of_tok t).Ast_c.cocci_tag :=
	(Ast_cocci.MINUS(Ast_cocci.DontCarePos,[]),[]) in
  let rec drop_up_to_brace = function
      [] -> ()
    | ((Parser_c.TOBrace _) as t) :: _ -> remove t
    | x :: rest -> remove x; drop_up_to_brace rest in
  let drop_final_brace toks =
    match List.rev toks with
      ((Parser_c.TCBrace _) as t) :: _ -> remove t
    | _ -> failwith "unexpected end of function" in
  drop_up_to_brace toks_e;
  drop_final_brace toks_e

(* ----------------------------------------------------------------------- *)
(* remove coments from tokens *)

let strip_comments toks =
 let toks = List.filter (function x -> not (TH.is_just_comment x)) toks in
  List.iter
    (function t ->
      (TH.info_of_tok t).Ast_c.comments_tag :=
	{Ast_c.mbefore = []; Ast_c.mafter = [];})
    toks;
 toks

(* ----------------------------------------------------------------------- *)
(* Create rule to check for header include *)

let print_header_rule pr srcfile =
  match Str.split (Str.regexp "/") srcfile with
    [x] ->
      pr "@header@\n@@\n\n#include \""; pr x; pr "\"\n\n"; true
  | l ->
      let rec loop = function
	  [] -> false
	| [x] ->
	    pr "@header@\n@@\n\n#include \""; pr x; pr "\"\n\n"; true
	| "include"::(x::xs) ->
	    pr "@header@\n@@\n\n#include <";
	    let x =
	      if Str.string_match (Str.regexp "asm-") x 0 then "asm" else x in
	    pr (String.concat "/" (x::xs));
	    pr ">\n\n"; true
	| x::xs -> loop xs in
      loop l

(* ----------------------------------------------------------------------- *)
(* Print metavariable declarations *)

let rec print_typedef typedefs pr = function
    (Ast_c.TypeName(s,_),_) ->
      if not (List.mem s !typedefs)
      then (typedefs := s::!typedefs; pr "typedef "; pr s; pr ";\n")
  | (Ast_c.Pointer(_,ty),_) -> print_typedef typedefs pr ty
  | _ -> ()

let print_metavar pr typedefs = function
    ((_,Some param,(_,(Ast_c.Pointer(_,(Ast_c.BaseType(Ast_c.Void),_)),_))),_)
    ->
      pr "expression "; pr param
  | (((_,Some param,(_,ty)),il) : Ast_c.parameterType) ->
      print_typedef typedefs pr ty;
      Pretty_print_c.pp_param_gen
	(function x ->
	  let str = Ast_c.str_of_info x in
	  if not (List.mem str ["const";"volatile"])
	  then (pr str; pr " "))
	(function _ -> pr " ")
	((false,Some param,
	  (({Ast_c.const = false; Ast_c.volatile = false},[]),ty)),
	 il)
  | _ -> failwith "function must have named parameters"

let print_metavariables pr defn header_req =
  let {Ast_c.f_name = s; f_type = (_, (paramst, (b, iib))); } = defn in
  (if header_req
  then pr "@depends on header@\n"
  else pr "@@\n");
  (if b then failwith "not handling variable argument functions");
  let typedefs = ref ([] : string list) in
  (match paramst with
    [] | [(((_,_,(_,(Ast_c.BaseType(Ast_c.Void),_))),_),_)] -> ()
  | (first,_)::rest ->
      print_metavar pr typedefs first; pr ";\n";
      List.iter (function (x,_) -> print_metavar pr typedefs x; pr ";\n")
	rest);
  pr "@@\n\n"

(* ----------------------------------------------------------------------- *)
(* copy a file, adding - at the beginning of every line *)

let minus_file pr file =
  Common.with_open_infile file (function chan ->
    let rec loop _ =
      let l = input_line chan in
      pr "- "; pr l; pr "\n";
      loop() in
    try loop() with End_of_file -> ())

(* ----------------------------------------------------------------------- *)
(* Print call to the defined function *)

let print_param_name pr = function
    ((_,Some param,_),_) -> pr param
  | _ -> failwith "function must have named parameters"

let pp_def_gen pr defn isexp =
  let {Ast_c.f_name = s; f_type = (_, (paramst, (b, iib))); } = defn in
  pr s; pr "(";
  (if b then failwith "not handling variable argument functions");
  (match paramst with
    [] | [(((_,_,(_,(Ast_c.BaseType(Ast_c.Void),_))),_),_)] -> ()
  | (first,_)::rest ->
      print_param_name pr first;
      List.iter (function (x,_) -> pr ", "; print_param_name pr x) rest);
  pr ")"; if not isexp then pr ";"

(* ----------------------------------------------------------------------- *)
(* Entry point *)

let pp_program (e,(str, toks_e)) outdir srcfile isexp =
  match e with
    Ast_c.Definition(({Ast_c.f_name = name;} as defn),_) ->
      (* generate the - code *)
      drop_header_toks toks_e;
      let toks_e = strip_comments toks_e in
      let tmp_file = Common.new_temp_file "cocci_small_output" ".c" in
      Unparse_c.pp_program [((e,(str, toks_e)), Unparse_c.PPnormal)]
	tmp_file;
      let outfile = outdir ^ "/" ^ name in
      let outfile =
	try
	  let cell = List.assoc outfile !names in
	  let ct = !cell in
	  cell := ct + 1;
	  outfile ^ (string_of_int ct)
	with Not_found ->
	  let cell = ref 1 in names := (outfile,cell) :: !names; outfile in
      let outfile = outfile ^ ".cocci" in
      Common.with_open_outfile outfile (fun (pr,chan) ->
	let header_req = print_header_rule pr srcfile in
	print_metavariables pr defn header_req;
	minus_file pr tmp_file;
	pr "+ ";
	pp_def_gen pr defn isexp;
	pr "\n")
  | _ -> Common.pr2_once "warning: function expected"; ()
