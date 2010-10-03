@ rule1 @
expression X;
@@

(
 f(X);
|
 g(1);
)

@@
expression rule1.X;
@@

- h(X);
+ hh(X);


@@
expression rule1.X;
@@
- h2(X);
+ hh22(X);

@@
@@

- foo(1);
+ bar(1);


