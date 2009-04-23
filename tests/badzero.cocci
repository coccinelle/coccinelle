// A pointer should not be compared to NULL
//
// Confidence: High
// Copyright: (C) Gilles Muller, Julia Lawall, EMN, DIKU.  GPLv2.
// URL: http://www.emn.fr/x-info/coccinelle/rules/badzero.html
// Options:

@r disable is_zero,isnt_zero @
expression *E;
position p;
@@

(
- E@p == 0
+ 27
|
- E@p != 0
+ 27
|
- 0 == E@p
+ 27
|
- 0 != E@p
+ 27
)

