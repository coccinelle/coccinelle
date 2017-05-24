@@
type T != { int, char };
identifier x;
@@
-   T x;
+   T *x;

@r1@
type T;
identifier x;
@@
 void g(...) {
       ...
       T x;
       ...
 }

@r2@
type T;
identifier x;
@@
 void h(...) {
       ...
       T x;
       ...
 }

/*Rule given by Michael Stefaniuc*/
@depends on r2@
type T = {r1.T, r2.T};
@@
 T *foo =
-          xyz
+          abc
 ;
