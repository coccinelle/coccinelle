//
// Use kzalloc rather than kcalloc(1,...)
//
// Confidence:
// Copyright: (C) Gilles Muller, Julia Lawall, EMN, DIKU.  GPLv2.
// URL: http://coccinelle.lip6.fr/
// Options:

virtual org,patch

@depends on patch && !org@
@@

- kcalloc(1,
+ kzalloc(
          ...)

@r depends on !patch && org@
position p;
@@

 kcalloc@p(1,...)

@script:python depends on org@
p << r.p;
@@

msg="%s:%s" % (p[0].file, p[0].line)
cocci.print_main(msg, p)
