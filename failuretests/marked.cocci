// Transform only assignments to a "struct item *", so that a file matches
// only if the types in the big.h that the test generates are known.

@@
struct item *P;
expression GFP;
@@

-	P = kmalloc(sizeof(*P), GFP)
+	P = kmalloc_obj(*P, GFP)
