@x@
position pc,pm,pi;
expression E;
@@

if@pc (E@pm) return i@pi;

@@
position x.pc,x.pm,x.pi;
expression E;
@@

if@pc (E@pm)
- return i@pi;
+ return 3;
