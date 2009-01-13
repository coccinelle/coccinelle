%{

%}

%token EOF EOL
%token TTrue TFalse TSC
%token<string> TInt TId

%start main
%type <Ast_exist.matrix> main

%%

main:
 TId TSC vs=list(ver) EOL es=list(entry) EOF { (vs,es) }

ver:
 i=TInt TSC                          { int_of_string(i) }

entry:
 id=TId TSC states=list(state) EOL   { (id,states, Misc.getpos(1)) }

state:
   TTrue TSC                         { Ast_exist.True (Misc.getpos(1)) }
 | TFalse TSC                        { Ast_exist.False (Misc.getpos(1)) }
