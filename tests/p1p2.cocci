@@
typedef Error;
type T;
identifier FN;
parameter P1;
@@
 T
+__attribute__((nonnull(1)))
 FN(P1, Error **errp);

@@
typedef Error;
type T;
identifier FN;
parameter P1;
parameter P2;
@@
 T
+__attribute__((nonnull(2)))
 FN(P1, P2, Error **errp);
