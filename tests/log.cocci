@@
char [] c;
expression f;
statement S1,S2;
format list d;
@@

 ... when != if(...) S1 else S2
(
 f(...,"%@d@"@c,...);
|
-f(...,c,...);
)
