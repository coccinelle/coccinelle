//
// sizeof argument must be return the size of the
// data, no the size of the pointer to it.
//

virtual org, patch

@ depends on patch && !org@
type T;
T *x;
expression E;
@@

memset(x, E, sizeof(
+ *
 x))

@r depends on !patch && org@
type T;
T *x;
expression E;
position p;
@@

memset(x, E, sizeof(x@p))

@script:python@
x << r.x;
p << r.p;
@@

cocci.print_main(x, p)