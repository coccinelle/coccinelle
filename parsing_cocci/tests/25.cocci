@@
expression A, C, E;
mdk_rdev_t *rdev;
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
-  atomic_dec(&rdev->nr_pending)
+  rdev_dec_pending(rdev,Y->mddev)
   ...
)

@@
expression F;
@@

error words = [atomic_dec(&F->nr_pending)]
