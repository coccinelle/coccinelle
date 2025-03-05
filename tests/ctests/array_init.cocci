@ rule3 @
identifier I;
type T;
expression E;
@@

T I[E];

@@
identifier rule3.I;
expression str;
declarer name MODULE_PARM;
@@

- MODULE_PARM(I,str);

