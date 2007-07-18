//@ rule1 @
//expression E;
//@@
//
// xxx(E);


@@
type T;
//expression rule1.E;
expression E;
@@

(
- f((T) E)
+ foo()
|
- f(E)
+ bar()
)