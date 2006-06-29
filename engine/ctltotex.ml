module CTL = Ast_ctl

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

let check_ct ct res = if ct > 60 then (res^"\\\\\\mbox{}",0) else (res,ct)
let texify s =
  let len = String.length s in
  let rec loop n =
    if n = len
    then ""
    else
      match String.get s n with
	'_' -> Printf.sprintf "\\_%s" (loop (n+1))
      | '{' -> Printf.sprintf "{\\ttlb}%s" (loop (n+1))
      | '}' -> Printf.sprintf "{\\ttrb}%s" (loop (n+1))
      | '>' -> Printf.sprintf "\\mth{>}%s" (loop (n+1))
      | c -> Printf.sprintf "%c%s" c (loop (n+1)) in
  (Printf.sprintf "\\mita{%s}" (loop 0),len)

let modif2c pv = function
    CTL.Modif(v) -> let (s,n) = texify(pv v) in (Printf.sprintf "_{%s}" s,n)
  | CTL.UnModif(v) -> let (s,n) = texify(pv v) in (Printf.sprintf "_{%s}" s,n)
  | CTL.Control -> ("",0)

let rec ctl2c ct pp pv = function
    CTL.False -> ("\\msf{false}",5)
  | CTL.True -> ("\\msf{true}",4)
  | CTL.Pred(p,v) ->
      let (res,n) = pp p in
      let (resv,n1) = modif2c pv v in
      (res^resv,ct+n+n1)
  | CTL.Not(f) ->
      let (res,ct) = wrap (ct+1) pp pv f in
      ("\\neg "^res,ct)
  | CTL.Exists(v,f) ->
      let (res1,len) = texify(pv v) in
      let ct = ct + len in
      let (res1,ct) = check_ct ct res1 in
      let (res2,ct) = existswrap (ct+1) pp pv f in
      ("\\exists "^res1^" . "^res2,ct)
  | CTL.And(f1,f2) ->
      let (res1,ct) = andwrap ct pp pv f1 in
      let (res1,ct) = check_ct ct res1 in
      let (res2,ct) = andwrap (ct+1) pp pv f2 in
      (res1^" \\wedge "^res2,ct)
  | CTL.Or(f1,f2) ->
      let (res1,ct) = orwrap ct pp pv f1 in
      let (res1,ct) = check_ct ct res1 in
      let (res2,ct) = orwrap (ct+1) pp pv f2 in
      (res1^" \\vee "^res2,ct)
  | CTL.Implies(f1,f2) ->
      let (res1,ct) = wrap ct pp pv f1 in
      let (res1,ct) = check_ct ct res1 in
      let (res2,ct) = wrap (ct+1) pp pv f2 in
      (res1^" \\rightarrow "^res2,ct)
  | CTL.AF(f) ->
      let (res,ct) = pathwrap (ct+2) pp pv f in ("\\AF"^res,ct)
  | CTL.AX(f) ->
      let (res,ct) = pathwrap (ct+2) pp pv f in ("\\AX"^res,ct)
  | CTL.AG(f) ->
      let (res,ct) = pathwrap (ct+2) pp pv f in ("\\AG"^res,ct)
  | CTL.AU(f1,f2) ->
      let (res1,ct) = existswrap (ct+2) pp pv f1 in
      let (res1,ct) = check_ct ct res1 in
      let (res2,ct) = existswrap (ct+3) pp pv f2 in
      ("\\A["^res1^" \\U "^res2^"]\n",ct)
  | CTL.EF(f) ->
      let (res,ct) = pathwrap (ct+2) pp pv f in ("\\EF"^res,ct)
  | CTL.EX(f) ->
      let (res,ct) = pathwrap (ct+2) pp pv f in ("\\EX"^res,ct)
  | CTL.EG(f) ->
      let (res,ct) = pathwrap (ct+2) pp pv f in ("\\EG"^res,ct)
  | CTL.EU(f1,f2) ->
      let (res1,ct) = existswrap (ct+2) pp pv f1 in
      let (res1,ct) = check_ct ct res1 in
      let (res2,ct) = existswrap (ct+3) pp pv f2 in
      ("\\E["^res1^" \\U "^res2^"]\n",ct)
  | CTL.Ref(v) -> texify(pv v)
  | CTL.Let(v,f1,f2) ->
      let (v,ct) = texify (pv v) in
      let (res1,ct) = letwrap (ct+5) pp pv f1 in
      let (res1,ct) = check_ct ct res1 in
      let (res2,ct) = letwrap (ct+3) pp pv f2 in
      (Printf.sprintf "\\mita{\\sf{let}} \\, %s = %s \\mita{\\sf{in}} %s\n"
	 v res1 res2, ct)

and wrap ct pp pv x =
  match x with
    CTL.False | CTL.True | CTL.Pred(_) -> ctl2c ct pp pv x
  | _ ->
      let (res,ct) = ctl2c (ct+1) pp pv x in
      (Printf.sprintf "(%s)" res,ct+1)

and andwrap ct pp pv x =
  match x with
    CTL.And(_,_) | CTL.False | CTL.True | CTL.Pred(_) -> ctl2c ct pp pv x
  | _ ->
      let (res,ct) = ctl2c (ct+1) pp pv x in
      (Printf.sprintf "(%s)" res,ct+1)

and orwrap ct pp pv x =
  match x with
    CTL.Or(_,_) | CTL.False | CTL.True | CTL.Pred(_) -> ctl2c ct pp pv x
  | _ ->
      let (res,ct) = ctl2c (ct+1) pp pv x in
      (Printf.sprintf "(%s)" res,ct+1)

and pathwrap ct pp pv x =
  match x with
    CTL.AX(_) | CTL.AF(_) | CTL.AG(_) | CTL.AU(_,_)
  | CTL.EX(_) | CTL.EF(_) | CTL.EG(_) | CTL.EU(_,_) ->
      ctl2c ct pp pv x
  | _ ->
      let (res,ct) = ctl2c (ct+1) pp pv x in
      (Printf.sprintf "(%s)" res,ct+1)

and existswrap ct pp pv x =
  match x with
    CTL.AX(_) | CTL.AF(_) | CTL.AG(_) | CTL.AU(_,_) | CTL.Pred(_)
  | CTL.EX(_) | CTL.EF(_) | CTL.EG(_) | CTL.EU(_,_) | CTL.Exists(_,_)
  | CTL.True | CTL.False | CTL.Not(_) ->
      ctl2c ct pp pv x
  | _ ->
      let (res,ct) = ctl2c (ct+1) pp pv x in
      (Printf.sprintf "(%s)" res,ct+1)

and letwrap ct pp pv x =
  match x with
    CTL.Let(_,_,_) ->
      let (res,ct) = ctl2c (ct+1) pp pv x in (Printf.sprintf "(%s)" res,ct+1)
  | _ -> ctl2c ct pp pv x

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

(* ----------------------------------------------------------------------- *)

let pred2c = function
    Lib_engine.TrueBranch -> ("\\msf{TrueBranch}",10)
  | Lib_engine.FalseBranch -> ("\\msf{FalseBranch}",11)
  | Lib_engine.After -> ("\\msf{After}",5)
  | Lib_engine.Return -> ("\\msf{Return}",5)
  | Lib_engine.Paren(s) -> ("\\msf{Paren}("^s^")",7+(String.length s))
  | Lib_engine.Match(re) ->
      let s = Unparse_cocci.rule_elem_to_string re in
      let (s,len) = texify s in
      (Printf.sprintf "%s" s,len)

let totex out_file rules ctls =
  let o = open_out out_file in
  make_prelude o;
  List.iter2
    (function ast_list ->
      function ctls ->
	ctltotex ast_list pred2c (function x -> x) ctls o)
    rules ctls;
  make_postlude o;
  close_out o

