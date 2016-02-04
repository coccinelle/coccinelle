@r@
expression a,b,c;
@@

(
foo(a,c);
|
bar(b,c);
)

@@
expression r.a,r.b,r.c;
@@

- xxx(\(a\|b\),c);
+ yyy();
