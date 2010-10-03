@initialize:python@

from coccilib.org import print_todo, print_link

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

print_todo (p1[0])
print_link (p2[0])

print ""

cocci.print_main ("foo", p1)
cocci.print_sec ("foo", p2)
cocci.print_secs ("foo", p2)

print ""

cocci.print_main ("foo", p1, "ovl-face3")
cocci.print_sec ("foo", p2, "ovl-face4")
cocci.print_secs ("foo", p2, "ovl-face4")
