@ rule1 @
expression E;
@@

f(E);

@@
expression rule1.E;
@@

- g(E);

@ rule2 @
expression E;
@@

- h(E);

@@
expression rule2.E;
@@

- i(E);

