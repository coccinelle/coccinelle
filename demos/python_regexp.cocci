@initialize:python@
import re
m = re.compile('_new$')

@r_init@
expression E;
identifier id;
position p;
@@

E = id@p();

@script:python@
id << r_init.id;
@@

print "COCCI: Analyzing %s" % id
if m.search(id.ident) != None:
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
