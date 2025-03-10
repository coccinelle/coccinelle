@x disable all@
position p;
expression E;
expression A;
statement S1, S2;
@@

if@p (foo(E,A)) S1 else S2

// here the incoming environment is p = 1,2 E = 6 and p = 3 E = 7
@disable all@
position p1 != x.p;
expression x.E;
expression B;
statement S1, S2;
@@

- if@p1 (foo(E,B)) S1 else S2

@y disable all@
position p;
expression E;
expression A;
statement S1, S2;
@@

if@p (bar(E,A)) S1 else S2

// here the incoming environment is p = 4,5 E = 6 and p = 6 E = 7
@disable all@
position p1 != y.p;
expression y.E;
expression C;
statement S1, S2;
@@

- if@p1 (bar(C,E)) S1 else S2
