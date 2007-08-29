@ rule3 @
identifier I;
type T;
expression E;
@@

T I[E];

@@
identifier rule3.I;
expression str;
type rule3.T;
@@

- MODULE_PARM(I,str);

