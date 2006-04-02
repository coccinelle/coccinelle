@@
expression A, C, E;
identifier B, D, X, Y;
@@

(
   X = mddev_to_conf(A);
   ...
-  atomic_dec(&X->B[C].rdev->nr_pending)
+  rdev_dec_pending(X->B[C].rdev,A)
   ...
|
   ...
-  atomic_dec(&Y->D[E].rdev->nr_pending)
+  rdev_dec_pending(Y->D[E].rdev,Y->mddev)
   ...
)
