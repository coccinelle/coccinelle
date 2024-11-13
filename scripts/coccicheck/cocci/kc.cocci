//
// Use kzalloc rather than kcalloc(1,...)
//
// Confidence:
// Copyright: (C) Gilles Muller, Julia Lawall, EMN, DIKU.  GPLv2.
// URL: https://coccinelle.gitlabpages.inria.fr/website//
// Options:

virtual org,patch,diff

@depends on patch && !org && !diff@
@@

- kcalloc(1,
+ kzalloc(
          ...)

@depends on !patch && !org && diff@
position p;
@@

*kcalloc@p(1,...)

@r depends on !patch && org && !diff@
position p;
@@

 kcalloc@p(1,...)

@script:python depends on org@
p << r.p;
@@

msg="%s:%s" % (p[0].file, p[0].line)
cocci.print_main(msg, p)
