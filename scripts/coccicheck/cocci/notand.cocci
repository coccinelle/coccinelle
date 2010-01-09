// !x&y combines boolean negation with bitwise and
//
// Confidence: High
// Copyright: (C) Gilles Muller, Julia Lawall, EMN, DIKU.  GPLv2.
// URL: http://www.emn.fr/x-info/coccinelle/rules/notand.html
// Options: -macro_file_builtins ../cocci/notand.h

virtual org,diff

@r@
expression E;
constant C;
position p;
@@

(
  !E & !C
|
  !@p E & C
)

@script:python depends on org@
p << r.p;
@@

cocci.print_main("",p)

@depends on diff@
expression E;
position r.p;
@@

* !@p E
