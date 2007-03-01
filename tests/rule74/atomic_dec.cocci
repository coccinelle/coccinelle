// The first rule is supposed to transform code into the form treated by the
// next rule.  Some code uses only the second rule.
// I have no idea what the above comment means... - jll
@@
identifier Rdev;
type T,T1;
identifier A;
expression C;
identifier V;
identifier B;
@@

(
   V = mddev_to_conf(A);
|
   T V = mddev_to_conf(A);
)
<...
(
  T1 *Rdev = V->B[C].rdev;
|
  Rdev = V->B[C].rdev;
)
  <...
- atomic_dec(&Rdev->nr_pending)
+ rdev_dec_pending(Rdev,A)
  ...>
(                                  // this is needed because the code above
? T1 *Rdev = V->B[C].rdev;         // is in a loop
|                                  // this problem was very hard to find
? Rdev = V->B[C].rdev;             // and the solution is very inelegant
)
...>

@@
identifier Rdev;
type T;
expression M,C;
identifier B;
@@

(
  T *Rdev = (mddev_to_conf(M))->B[C].rdev;
|
  Rdev = (mddev_to_conf(M))->B[C].rdev;
)
  <...
- atomic_dec(&Rdev->nr_pending)
+ rdev_dec_pending(Rdev,V)
  ...>

@@
identifier Rdev;
type T;
expression C;
identifier V;
identifier B;
@@

(
  T *Rdev = V->B[C].rdev;
|
  Rdev = V->B[C].rdev;
)
  <...
- atomic_dec(&Rdev->nr_pending)
+ rdev_dec_pending(Rdev,V->mddev)
  ...>

@@
expression C;
identifier A;
identifier V;
type T;
identifier B;
@@

(
   V = mddev_to_conf(A);
|
   T V = mddev_to_conf(A);
)
   <...
-  atomic_dec(&V->B[C].rdev->nr_pending)
+  rdev_dec_pending(V->B[C].rdev,A)
   ...>

@@
expression M;
identifier B;
expression C;
@@

-  atomic_dec(&(mddev_to_conf(M))->B[C].rdev->nr_pending)
+  rdev_dec_pending((mddev_to_conf(M))->B[C].rdev,M)

@@
identifier X;
identifier D;
expression E;
@@

-  atomic_dec(&X->D[E].rdev->nr_pending)
+  rdev_dec_pending(X->D[E].rdev,X->mddev)
