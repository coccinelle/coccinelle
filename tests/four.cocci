@ rule1 @
expression E;
@@

f(E);

@ rule2 @
expression rule1.E;
@@

- g(E);

//----------------------------------------------------------------------------

@ rule3 @
expression E;
@@

- h(E);

@ rule4 @
expression rule3.E;
@@

- i(E);

