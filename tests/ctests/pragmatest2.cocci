@rr@
expression i;
@@

f(i)

@script:ocaml ss@
i << rr.i;
exp;
@@

exp := make_ident i

@@
identifier ss.exp;
@@

-g(exp);







@r@
pragmainfo i;
@@

#pragma xxx i

@script:ocaml s@
i << r.i;
exp;
@@

exp := make_ident(List.hd(String.split_on_char ' ' i))

@@
identifier s.exp;
@@

-f(exp);
