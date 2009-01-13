%{

%}

%token EOF EOL
%token TMINUS
%token<string> TInt TId

%start main
%type <Ast_bugs.bugs> main

%%

main:
 bugs=list(bug) EOF                  { bugs }

bug:
   id=TId EOL vs=list(ver)             { (id, "", vs, Misc.getpos(1)) }
 | id=TId TMINUS cs=list(complement) EOL vs=list(ver)
    { (id, String.concat " " cs, vs, Misc.getpos(1)) }

complement:
    TId                              { $1 }
 |  TInt                             { $1 }

ver:
 i=TInt EOL                          { int_of_string(i) }
