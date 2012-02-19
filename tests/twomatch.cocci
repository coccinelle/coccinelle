@r@
expression a,b;
@@

(
foo(a);
|
bar(b);
)

@@
expression r.a,r.b;
@@

- xxx(\(a\|b\));
+ yyy();
