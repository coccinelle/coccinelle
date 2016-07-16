@initialize:python@
@@

def past_line_4(p, other):
    return int(p[0].line) > 4

@r@
expression e;
@@

f(e)

@@
position p : script:python(r.e) { past_line_4(p, e) };
expression r.e;
@@

g(
-e@p
+27
 )
