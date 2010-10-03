//
// sizeof argument must be return the size of the
// data, no the size of the pointer to it.
//

virtual org, patch, diff

@ depends on patch && !org && !diff expression@
expression *x;
@@

x =
 <+...
-sizeof(x)
+sizeof(*x)
...+>

@ depends on !patch && !org && diff expression@
expression *x;
@@

x =
 <+...
*sizeof(x)
...+>

@r depends on !patch && org && !diff expression @
expression *x;
position p;
@@

x = <+... sizeof(x@p) ...+>

@script:python@
x << r.x;
p << r.p;
@@

cocci.print_main(x, p)