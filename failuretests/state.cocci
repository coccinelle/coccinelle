// Transform both files.  The first includes a header that the test
// generates to be slow to process, so that a timeout can strike while
// inferring types in it.

@@
expression VAR, GFP;
@@

-	VAR = kmalloc(sizeof(*VAR), GFP)
+	VAR = kmalloc_obj(*VAR, GFP)
