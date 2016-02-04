// Use the macro ARRAY_SIZE when possible
//
// Confidence: High
// Copyright: (C) Gilles Muller, Julia Lawall, EMN, INRIA, DIKU.  GPLv2.
// URL: http://coccinelle.lip6.fr/rules/array.html
// Options: -I ... -all_includes can give more complete results
virtual org
virtual patch

@i@
@@

#include <linux/kernel.h>

/////////////////////////////////////
/////////////////////////////////////
@depends on i && patch && !org@
type T;
T[] E;
@@

- (sizeof(E)/sizeof(*E))
+ ARRAY_SIZE(E)

@depends on i && patch && !org@
type T;
T[] E;
@@

- (sizeof(E)/sizeof(E[...]))
+ ARRAY_SIZE(E)

@depends on i && patch && !org@
type T;
T[] E;
@@

- (sizeof(E)/sizeof(T))
+ ARRAY_SIZE(E)

@n_patch depends on patch && !org@
identifier AS,E;
@@

- #define AS(E) ARRAY_SIZE(E)

@ depends on patch && !org@
expression E;
identifier n_patch.AS;
@@

- AS(E)
+ ARRAY_SIZE(E)


/////////////////////////////////////
/////////////////////////////////////
@arr_ptr depends on i && !patch && org@
type T;
T[] E;
position p;
@@

 (sizeof(E@p)/sizeof(*E))

@arr_tab depends on i && !patch && org@
type T;
T[] E;
position p;
@@

 (sizeof(E@p)/sizeof(E[...]))

@arr_typ depends on i && !patch && org@
type T;
T[] E;
position p;
@@

 (sizeof(E@p)/sizeof(T))

@n_org depends on !patch && org@
identifier AS,E;
@@

#define AS(E) ARRAY_SIZE(E)

@arr_def depends on !patch && org@
expression E;
identifier n_org.AS;
position p;
@@

AS@p(E)

@script:python@
p << arr_ptr.p;
e << arr_ptr.E;
@@
cocci.print_main(e,p)

@script:python@
p << arr_tab.p;
e << arr_tab.E;
@@
cocci.print_main(e,p)

@script:python@
p << arr_typ.p;
e << arr_typ.E;
@@
cocci.print_main(e,p)

@script:python@
p << arr_def.p;
e << arr_def.E;
@@
cocci.print_main(e,p)
