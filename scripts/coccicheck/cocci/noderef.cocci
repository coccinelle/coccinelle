//
// sizeof argument must be return the size of the
// data, no the size of the pointer to it.
//

virtual org, patch

@ depends on patch && !org expression@
expression *x;
@@

x =
 <+...
-sizeof(x)
+sizeof(*x)
...+>

@r depends on !patch && org expression @
expression *x;
position p;
@@

x = <+... sizeof(x@p) ...+>

@script:python@
x << r.x;
p << r.p;
@@

cocci.print_main(x, p)