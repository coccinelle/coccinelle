@@
expression A, B, C, D, E;
identifier X;
@@

(
-  atomic_dec(&(mddev_to_conf(A))->B[C].rdev->nr_pending)
+  rdev_dec_pending((mddev_to_conf(A))->B[C].rdev,A)
|
-  atomic_dec(&X->D[E].rdev->nr_pending)
+  rdev_dec_pending(X->D[E].rdev,X->mddev)
)
