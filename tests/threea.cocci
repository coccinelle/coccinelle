@ rule1 @
expression E;
@@

f(E);

//@ rule2 extends rule1 @
//@@
//
//- h(E);

@ rule3 extends rule1 @
@@

- q(E);
