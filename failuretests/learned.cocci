// learned/first.c and learned/second.c both use the unknown loop macro
// scoped_cond_lock, with a statement among its arguments.  The first file
// teaches spatch that it is a loop macro; the second must still parse.

@@
expression VAR, GFP;
@@

-	VAR = kmalloc(sizeof(*VAR), GFP)
+	VAR = kmalloc_obj(*VAR, GFP)
