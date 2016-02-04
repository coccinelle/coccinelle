@ rule1 @
type T1, T2;
@@

typedef T1 { ... } T2;

@@
type rule1.T1, rule1.T2;
@@

- T2
+ T1

@ rule2 @
type T1, T2;
@@

typedef T1 T2;

@@
type rule2.T1, rule2.T2;
@@

- T2
+ T1
