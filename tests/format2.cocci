@r@
format d =~ ".x$";
position p;
@@

foo@p("...%@d@...")

@@
position r.p;
@@

-foo@p(...);

@script:ocaml@
d << r.d;
@@
Printf.printf "format string is %s\n" d
