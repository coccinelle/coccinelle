@@
expression e;
@@

+ #ifdef FOO
+ call(e);
+ #endif
  foo(e);

@@
expression e;
@@

  bar(e);
+ #ifdef BAR
+ call(e);
+ #endif
