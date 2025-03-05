@print@
constant char [] c;
expression E1;
position p;
identifier f;
@@

f(...,c,...,E1@p,...)

@free@
expression E;
position p1;
@@

kfree@p1(E)

@r exists@
expression free.E, subE<=free.E, E2;
iterator iter;
statement S;
position free.p1,p2!=print.p;
@@

kfree@p1(E)
...
(
 iter(subE,...) S // no use
|
 subE = E2 // no use
|
 subE++ // no use
|
 subE-- // no use
|
 &subE // no use
|
- E@p2 // bad use
+ NULL
)
