@rule1@
expression E;
position p1,p2,p3,p4;
@@

* f(E@p1, xxx, E@p2);
  ...
* f(E@p3, xxx, E@p4);

@@
expression rule1.E;
position p1,p2;
@@

* g(E@p1, xxx, E);
  ...
* g(E, xxx, E@p2);

@@
expression rule1.E;
position p1,p2;
@@

* h(E, xxx, E@p1);
  ...
* h(E@p2, xxx, E);
