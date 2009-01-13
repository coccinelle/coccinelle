%{

%}

%token EOF EOL
%token TSTAR TSLASH TCOLON TPLUS TEQUAL
%token TVERT TDASH TLAB TRAB
%token TVIEW TFACE TLINB TCOLB TCOLE TORG TCONFIG
%token TSTODO TTODO
%token<int> TInt
%token<string> TId

%start main
%type <Ast_org.orgs> main

%%

(* Misc.getpos(2) *)

main:
 bugs=list(bug) list(tail) EOF                     { bugs }

tail:
   TSTAR TORG TCONFIG EOL                          {}
 | TDASH TPLUS TSTODO TCOLON TTODO TVERT list(TId) {}

bug:
   TSTAR s=status l=link EOL { (s, l) }
(* | TSTAR s=status l=link EOL subs=list(sbug) { (s, l)} *)

status:
   TTODO                             { Ast_org.TODO }
 | s=TId                             { Ast_org.Unknow(s) }

link:
 TLAB TLAB TVIEW TCOLON p=path ops=list(option) TRAB TLAB t=text TRAB TRAB
                                     { (p, ops, t) }

path:
    p=list(path_elt)                 { "/" ^ (String.concat "/" p) }

path_elt:
    TSLASH e=TId                     { e }

option:
    TCOLON TCOLON TLINB TEQUAL v=TInt   { Ast_org.Line v }
 |  TCOLON TCOLON TCOLB TEQUAL v=TInt   { Ast_org.ColB v }
 |  TCOLON TCOLON TCOLE TEQUAL v=TInt   { Ast_org.ColE v }
 |  TCOLON TCOLON TFACE TEQUAL TId      { Ast_org.Face   }

text:
    i=TId                               { (i,0)    }
 |  p=path TCOLON TCOLON line=TInt      { (p,line) }

sbug:
    TSTAR sb=bug EOL {}
 |  l=link       EOL {}
