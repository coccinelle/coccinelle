@r_init@
expression E;
identifier id;
position p;
@@

E = id@p();

@script:python@
id << r_init.id;
@@

import re

print "COCCI: Analysing %s" % id
m = re.search('_new$', id.ident)
if m != None:
	print "COCCI: %s matchs '_new$'" % id
else:
	print "COCCI: %s discarded" % id
	cocci.include_match(False)


@r_do@
expression E;
identifier id;
position r_init.p;
@@

E = id@p();
+ if (E == NULL)
+   goto err;
