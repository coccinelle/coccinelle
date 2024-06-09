@@
identifier i1;
type T1;
@@
        T1 i1;
+       vector<f(1, 2)> d;

@@
identifier i1;
type T1;
typedef xxx;
@@
        T1 i1;
+       xxx<g(3, 4)> d;

@@
symbol a,b;
identifier foo;
@@

- foo<12,a> (3, 4);
+ bar<102,b> (30, 40);

@type@
type x;
typedef abc;
@@

- x<g(...)>
+ abc<2 + 2>
