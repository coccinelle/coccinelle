(* Function table management *)

type pos = { current_element : string;
	     file :string ;
	     line : int;
	     col : int;
	     line_end : int;
	     col_end : int; }

type param_type =
    Pos of pos list
  | Str of string
  | Type of Ast_c.fullType
  | Init of Ast_c.initialiser
  | InitList of Ast_c.initialiser Ast_c.wrap2 list
  | Int of int
  | Param of Ast_c.parameterType
  | ParamList of Ast_c.parameterType Ast_c.wrap2 list
  | Expr of Ast_c.expression
  | ExprList of Ast_c.argument Ast_c.wrap2 list
  | Decl of Ast_c.declaration
  | Field of Ast_c.field
  | FieldList of Ast_c.field list
  | Stmt of Ast_c.statement

let fcts : (string, param_type list -> string ref list -> unit) Hashtbl.t =
  Hashtbl.create 11 (* Use prime number *)

(* ---------------------------------------------------------------------- *)
(* Match management *)

let inc_match = ref true
let include_match x = inc_match := x

let exited = ref false
let exit _ = exited := true

let dir () = !Flag.dir

(* ---------------------------------------------------------------------- *)
(* org mode *)

let build_link p msg color =
  Printf.sprintf
    "[[view:%s::face=%s::linb=%d::colb=%d::cole=%d][%s]]"
    p.file color p.line p.col p.col_end msg

let print_todo ?color:(color="ovl-face1") ?msg:(msg="") p =
  let msg =
    if msg = ""
    then Printf.sprintf "%s::%d" p.file p.line
    else msg in
  Printf.printf "* TODO %s\n" (build_link p msg color)

let print_link ?color:(color="ovl-face2") ?msg:(msg="") p =
  let msg =
    if msg = ""
    then Printf.sprintf "%s::%d" p.file p.line
    else msg in
  Printf.printf "%s\n" (build_link p msg color)

let print_safe_todo ?color:(color="ovl-face1") ?msg:(msg="") p =
  let msg = String.concat "@(" (Str.split_delim (Str.regexp_string "[") msg) in
  let msg = String.concat ")" (Str.split_delim (Str.regexp_string "]") msg) in
  print_todo ~color:color ~msg:msg p

let print_safe_link ?color:(color="ovl-face2") ?msg:(msg="") p =
  let msg = String.concat "@(" (Str.split_delim (Str.regexp_string "[") msg) in
  let msg = String.concat ")" (Str.split_delim (Str.regexp_string "]") msg) in
  print_link ~color:color ~msg:msg p

(*
print_main, print_sec and print_secs
*)
let print_main ?color:(color="ovl-face1") msg ps =
  let p = List.hd ps in
  let oldmsgfmt =
    if msg == ""
    then Printf.sprintf "%s::%d" p.file p.line
    else Printf.sprintf "%s %s::%d" msg p.file p.line in
  print_todo ~color:color ~msg:oldmsgfmt p

let print_sec ?color:(color="ovl-face2") msg ps =
  print_link ~color:color ~msg:msg (List.hd ps)

let print_secs ?color:(color="ovl-face2") msg ps =
  List.iter (function i -> print_link ~color:color ~msg:msg i) ps

(*
pos transformations
*)

let basename_pos pos = { pos with file = Filename.basename (pos.file) }


(*
external analysis results interface
(in a separate module to not pollute the namespace)
*)

module Ana = struct
  type result = Externalanalysis.result
  type bound  = Externalanalysis.bound

  let show_bound = Externalanalysis.show_bound
  let show_result = Externalanalysis.show_result

  let load_results =
    Externalanalysis.load_external_results

  let find pos = 
    Externalanalysis.find_results pos.file (pos.line, pos.col) (pos.line_end, pos.col_end)

  let inter = Externalanalysis.intersect_results

  let satisfy f pos =
    Externalanalysis.satisfy f pos.file (pos.line, pos.col) (pos.line_end, pos.col_end)

  let satisfy1 f pos = 
    Externalanalysis.satisfy1 f pos.file (pos.line, pos.col) (pos.line_end, pos.col_end)

  let has_any pos =
    Externalanalysis.has_any_result pos.file (pos.line, pos.col) (pos.line_end, pos.col_end)

  let for_all p pos =
    Externalanalysis.for_all p pos.file (pos.line, pos.col) (pos.line_end, pos.col_end)

  let for_all1 p pos =
    Externalanalysis.for_all1 p pos.file (pos.line, pos.col) (pos.line_end, pos.col_end)

  let exists p pos =
    Externalanalysis.exists p pos.file (pos.line, pos.col) (pos.line_end, pos.col_end)

  let single_int = Externalanalysis.single_int
  let contains_int = Externalanalysis.contains_int

  let has_only_nul pos =
    Externalanalysis.has_only_nul pos.file (pos.line, pos.col) (pos.line_end, pos.col_end)

  let has_also_nul pos =
    Externalanalysis.has_also_nul pos.file (pos.line, pos.col) (pos.line_end, pos.col_end)

  let has_also_int c pos =
    Externalanalysis.has_also_int c pos.file (pos.line, pos.col) (pos.line_end, pos.col_end)

end
