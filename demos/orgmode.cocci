@r@
position p1, p2;
identifier f;
expression E;
@@

f@p1(E@p2)

@ script:python @
p1 << r.p1;
p2 << r.p2;
f  << r.f;
@@

cocci.print_main (p1)
cocci.print_sec (p2)

print ""

cocci.print_main (p1, "foo")
cocci.print_sec (p2, "foo")

print ""

cocci.print_main (p1, "foo", "ovl-face3")
cocci.print_sec (p2, "foo", "ovl-face4")
