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
  "\\begin{document}\n"

let postlude = "\\end{document}"

let rec ctl2c pp pv = function
    CTL.False -> "\\msf{false}"
  | CTL.True -> "\\msf{true}"
  | CTL.Pred(p) -> pp p
  | CTL.Not(f) -> "\\neg "^(wrap pp pv f)
  | CTL.Exists(v,f) -> "\\exists "^(pv v)^" . "^(ctl2c pp pv f)
  | CTL.And(f1,f2) -> (wrap pp pv f1)^" \\wedge "^(wrap pp pv f2)
  | CTL.Or(f1,f2) -> (wrap pp pv f1)^" \\vee "^(wrap pp pv f2)
  | CTL.Implies(f1,f2) -> (wrap pp pv f1)^" \\rightarrow\n"^(wrap pp pv f2)
  | CTL.AF(f) -> "\\AF"^(pathwrap pp pv f)
  | CTL.AX(f) -> "\\AX"^(pathwrap pp pv f)
  | CTL.AG(f) -> "\\AG"^(pathwrap pp pv f)
  | CTL.AU(f1,f2) -> "\\A["^(ctl2c pp pv f1)^" \\U "^(ctl2c pp pv f2)^"]\n"
  | CTL.EF(f) -> "\\EF"^(pathwrap pp pv f)
  | CTL.EX(f) -> "\\EX"^(pathwrap pp pv f)
  | CTL.EG(f) -> "\\EG"^(pathwrap pp pv f)
  | CTL.EU(f1,f2) -> "\\E["^(ctl2c pp pv f1)^" \\U "^(ctl2c pp pv f2)^"]\n"

and wrap pp pv x =
  match x with
    CTL.False | CTL.True | CTL.Pred(_) -> ctl2c pp pv x
  | _ -> Printf.sprintf "(%s)" (ctl2c pp pv x)

and pathwrap pp pv x =
  match x with
    CTL.AX(_) | CTL.AF(_) | CTL.AG(_) | CTL.AU(_,_)
  | CTL.EX(_) | CTL.EF(_) | CTL.EG(_) | CTL.EU(_,_) ->
      ctl2c pp pv x
  | _ -> Printf.sprintf "(%s)" (ctl2c pp pv x)

let ctltotex rule pp pv ctls o =
  Printf.fprintf o "\\begin{quote}\\begin{verbatim}\n";
  Printf.fprintf o "%s\n" (Unparse_cocci.unparse_to_string rule);
  Printf.fprintf o "\\end{verbatim}\\end{quote}\n\n";
  List.iter
    (function ctl ->
      Printf.fprintf o "\\[\n";
      Printf.fprintf o "%s\n" (ctl2c pp pv ctl))
    ctls;
  Printf.fprintf o "\\]\n\n"
  
let make_prelude o = Printf.fprintf o "%s\n" prelude
let make_postlude o = Printf.fprintf o "%s\n" postlude
