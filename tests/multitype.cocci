@ rule1 @
type T;
T *E;
identifier fld;
@@

f(E->fld)

@@
rule1.T *E;
@@

- g(E)
+ g(E, NULL)
