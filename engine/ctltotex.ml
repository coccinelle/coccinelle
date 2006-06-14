(* module CTL = Ast_ctl *)

let prelude =
  "\\documentclass{article}\n"^
  "\\usepackage{fullpage}\n\n"^
  "\\newcommand{\\U}{\\,\\mbox{\\sf{U}}\\,}\n"^
  "\\newcommand{\\A}{\\mbox{\\sf{A}}}\n"^
  "\\newcommand{\\E}{\\mbox{\\sf{A}}}\n"^
  "\\newcommand{\\AX}{\\mbox{\\sf{AX}}}\n"^
  "\\newcommand{\\EX}{\\mbox{\\sf{EX}}}\n"^
  "\\newcommand{\\AF}{\\mbox{\\sf{AF}}}\n"^
  "\\newcommand{\\EF}{\\mbox{\\sf{EF}}}\n"^
  "\\newcommand{\\AG}{\\mbox{\\sf{AG}}}\n"^
  "\\newcommand{\\EG}{\\mbox{\\sf{EG}}}\n\n"^
  "\\newcommand{\\mita}[1]{\\mbox{\\it{{#1}}}}\n"^
  "\\newcommand{\\mtt}[1]{\\mbox{\\tt{{#1}}}}\n"^
  "\\newcommand{\\msf}[1]{\\mbox{\\sf{{#1}}}}\n"^
  "\\newcommand{\\mrm}[1]{\\mbox{\\rm{{#1}}}}\n"^
  "\\newcommand{\\mth}[1]{\\({#1}\\)}\n\n"^
  "\\newcommand{\\ttlb}{\\mbox{\\tt \\char'173}}\n"^
  "\\newcommand{\\ttrb}{\\mbox{\\tt \\char'175}}\n\n"^
  "\\begin{document}\n"

let postlude = "\\end{document}"

let check_ct ct res = if ct > 80 then (res^"\\\\",0) else (res,ct)
let texify s =
  let len = String.length s in
  let rec loop n =
    if n = len
    then ""
    else
      match String.get s n with
	'_' -> Printf.sprintf "\\_%s" (loop (n+1))
      | c -> Printf.sprintf "%c%s" c (loop (n+1)) in
  Printf.sprintf "\\mita{%s}" (loop 0)

let modif2c pv = function
    Ast_ctl.Modif(v) -> Printf.sprintf "_{%s}" (texify(pv v))
  | Ast_ctl.UnModif(v) -> Printf.sprintf "_{%s}" (texify(pv v))
  | Ast_ctl.Control -> ""

let rec ctl2c ct pp pv = function
    Ast_ctl.False -> ("\\msf{false}",5)
  | Ast_ctl.True -> ("\\msf{true}",4)
  | Ast_ctl.Pred(p,v) ->
      let res = pp p in
      let resv = modif2c pv v in
      (res^resv,ct+String.length res+String.length resv)
  | Ast_ctl.Not(f) ->
      let (res,ct) = wrap (ct+1) pp pv f in
      ("\\neg "^res,ct)
  | Ast_ctl.Exists(v,f) ->
      let res1 = texify(pv v) in
      let ct = ct + String.length res1 in
      let (res1,ct) = check_ct ct res1 in
      let (res2,ct) = existswrap (ct+1) pp pv f in
      ("\\exists "^res1^" . "^res2,ct)
  | Ast_ctl.And(f1,f2) ->
      let (res1,ct) = andwrap ct pp pv f1 in
      let (res1,ct) = check_ct ct res1 in
      let (res2,ct) = andwrap (ct+1) pp pv f2 in
      (res1^" \\wedge "^res2,ct)
  | Ast_ctl.Or(f1,f2) ->
      let (res1,ct) = orwrap ct pp pv f1 in
      let (res1,ct) = check_ct ct res1 in
      let (res2,ct) = orwrap (ct+1) pp pv f2 in
      (res1^" \\vee "^res2,ct)
  | Ast_ctl.Implies(f1,f2) ->
      let (res1,ct) = wrap ct pp pv f1 in
      let (res1,ct) = check_ct ct res1 in
      let (res2,ct) = wrap (ct+1) pp pv f2 in
      (res1^" \\rightarrow "^res2,ct)
  | Ast_ctl.AF(f) ->
      let (res,ct) = pathwrap (ct+2) pp pv f in ("\\AF"^res,ct)
  | Ast_ctl.AX(f) ->
      let (res,ct) = pathwrap (ct+2) pp pv f in ("\\AX"^res,ct)
  | Ast_ctl.AG(f) ->
      let (res,ct) = pathwrap (ct+2) pp pv f in ("\\AG"^res,ct)
  | Ast_ctl.AU(f1,f2) ->
      let (res1,ct) = existswrap (ct+2) pp pv f1 in
      let (res1,ct) = check_ct ct res1 in
      let (res2,ct) = existswrap (ct+3) pp pv f2 in
      ("\\A["^res1^" \\U "^res2^"]\n",ct)
  | Ast_ctl.EF(f) ->
      let (res,ct) = pathwrap (ct+2) pp pv f in ("\\EF"^res,ct)
  | Ast_ctl.EX(f) ->
      let (res,ct) = pathwrap (ct+2) pp pv f in ("\\EX"^res,ct)
  | Ast_ctl.EG(f) ->
      let (res,ct) = pathwrap (ct+2) pp pv f in ("\\EG"^res,ct)
  | Ast_ctl.EU(f1,f2) ->
      let (res1,ct) = existswrap (ct+2) pp pv f1 in
      let (res1,ct) = check_ct ct res1 in
      let (res2,ct) = existswrap (ct+3) pp pv f2 in
      ("\\E["^res1^" \\U "^res2^"]\n",ct)

and wrap ct pp pv x =
  match x with
    Ast_ctl.False | Ast_ctl.True | Ast_ctl.Pred(_) -> ctl2c ct pp pv x
  | _ ->
      let (res,ct) = ctl2c (ct+1) pp pv x in
      (Printf.sprintf "(%s)" res,ct+1)

and andwrap ct pp pv x =
  match x with
    Ast_ctl.And(_,_) | Ast_ctl.False | Ast_ctl.True | Ast_ctl.Pred(_) -> ctl2c ct pp pv x
  | _ ->
      let (res,ct) = ctl2c (ct+1) pp pv x in
      (Printf.sprintf "(%s)" res,ct+1)

and orwrap ct pp pv x =
  match x with
    Ast_ctl.Or(_,_) | Ast_ctl.False | Ast_ctl.True | Ast_ctl.Pred(_) -> ctl2c ct pp pv x
  | _ ->
      let (res,ct) = ctl2c (ct+1) pp pv x in
      (Printf.sprintf "(%s)" res,ct+1)

and pathwrap ct pp pv x =
  match x with
    Ast_ctl.AX(_) | Ast_ctl.AF(_) | Ast_ctl.AG(_) | Ast_ctl.AU(_,_)
  | Ast_ctl.EX(_) | Ast_ctl.EF(_) | Ast_ctl.EG(_) | Ast_ctl.EU(_,_) ->
      ctl2c ct pp pv x
  | _ ->
      let (res,ct) = ctl2c (ct+1) pp pv x in
      (Printf.sprintf "(%s)" res,ct+1)

and existswrap ct pp pv x =
  match x with
    Ast_ctl.AX(_) | Ast_ctl.AF(_) | Ast_ctl.AG(_) | Ast_ctl.AU(_,_) | Ast_ctl.Pred(_)
  | Ast_ctl.EX(_) | Ast_ctl.EF(_) | Ast_ctl.EG(_) | Ast_ctl.EU(_,_) | Ast_ctl.Exists(_,_)
  | Ast_ctl.True | Ast_ctl.False | Ast_ctl.Not(_) ->
      ctl2c ct pp pv x
  | _ ->
      let (res,ct) = ctl2c (ct+1) pp pv x in
      (Printf.sprintf "(%s)" res,ct+1)

let ctltotex rule pp pv ctls o =
  Printf.fprintf o "\\begin{quote}\\begin{verbatim}\n";
  Printf.fprintf o "%s\n" (Unparse_cocci.unparse_to_string rule);
  Printf.fprintf o "\\end{verbatim}\\end{quote}\n\n";
  List.iter
    (function ctl ->
      Printf.fprintf o "\\[\\begin{array}{l}\n";
      let (res,_) = ctl2c 0 pp pv ctl in
      Printf.fprintf o "%s\n" res)
    ctls;
  Printf.fprintf o "\\end{array}\\]\n\n"
  
let make_prelude o = Printf.fprintf o "%s\n" prelude
let make_postlude o = Printf.fprintf o "%s\n" postlude
