//
// sizeof argument must be return the size of the
// data, no the size of the pointer to it.
//

virtual org, patch, diff

@ depends on patch && !org && !diff@
type T;
T *x;
expression E;
@@

memset(x, E, sizeof(
+ *
 x))

@ depends on !patch && !org && diff@
type T;
T *x;
expression E;
@@

*memset(x, E, sizeof(x))

@r depends on !patch && org && !diff@
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