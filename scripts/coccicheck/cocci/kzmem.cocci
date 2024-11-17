// Remove useless call to memset for zeroing memory
//  after a kzalloc call.
//
// Confidence:
// Copyright: (C) Gilles Muller, Julia Lawall, EMN, DIKU.  GPLv2.
// URL: https://coccinelle.gitlabpages.inria.fr/website/
// Options:

virtual org,patch,diff

@depends on patch && !org && !diff@
expression x;
statement S;
@@

x = kzalloc(...);
if (x == NULL) S
... when != x
-memset(x,0,...);

@depends on !patch && !org && diff@
expression x;
statement S;
@@

x = kzalloc(...);
if (x == NULL) S
... when != x
*memset(x,0,...);

@r depends on !patch && org && !diff@
expression x;
statement S;
position p;
@@

x = kzalloc(...);
if (x == NULL) S
... when != x
memset@p(x,0,...);

@script:python depends on org@
p << r.p;
@@

msg="%s:%s" % (p[0].file, p[0].line)
cocci.print_main(msg, p)
