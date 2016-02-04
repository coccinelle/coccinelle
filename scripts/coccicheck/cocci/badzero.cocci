// A pointer should not be compared to zero
//
// Confidence: High
// Copyright: (C) Gilles Muller, Julia Lawall, EMN, DIKU.  GPLv2.
// URL: http://coccinelle.lip6.fr/rules/badzero.html
// Options:

virtual org,diff

@r disable is_zero,isnt_zero @
expression *E;
position p;
@@

(
  E@p == 0
|
  E@p != 0
|
  0 == E@p
|
  0 != E@p
)

@script:python depends on org@
p << r.p;
@@

cocci.print_main("",p)

@depends on diff@
position r.p;
expression E;
@@

*E@p
