@@
expression A, C, E;
identifier X;
identifier V;
identifier B,D;
@@


(
   V = mddev_to_conf(A);
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
   ...
-  atomic_dec(&X->D[E].rdev->nr_pending)
+  rdev_dec_pending(X->D[E].rdev,X->mddev)
   ...
)
