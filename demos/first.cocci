@initialize:python@

first = 0

@first_fct@
type T;
identifier f;
position p;
@@

T f@p (...) {...}

@script:python@
p << first_fct.p;
@@

if p[0].line > first && first == 0:
   first = p[0].line
else:
   cocci.include_match(false)

@@
type T;
identifier f;
position first_fct.p;
@@

+#include <foo.h>

T f@p (...) {...}
