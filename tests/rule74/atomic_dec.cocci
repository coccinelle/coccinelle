@@
expression C, E;
identifier A;
identifier X;
identifier V;
type T;
identifier B,D;
identifier fn;
@@

fn(...) {
(
   ...
(
   V = mddev_to_conf(A);
|
   T V = mddev_to_conf(A);
)
   ...
-  atomic_dec(&V->B[C].rdev->nr_pending)
+  rdev_dec_pending(V->B[C].rdev,A)
   ...
|
   ...
-  atomic_dec(&(mddev_to_conf(A))->B[C].rdev->nr_pending)
+  rdev_dec_pending((mddev_to_conf(A))->B[C].rdev,A)
   ...
|
   ... when != \(X = mddev_to_conf(A); \| T X = mddev_to_conf(A);\)
-  atomic_dec(&X->D[E].rdev->nr_pending)
+  rdev_dec_pending(X->D[E].rdev,X->mddev)
   ...
)
}
