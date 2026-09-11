// The finalize rule runs after the last file has been handled, and uses
// more CPU time than the --timeout that the test gives.  The timeout counts
// only user time, so check the clock rarely, as it takes a system call.

@@
expression VAR, GFP;
@@

-	VAR = kmalloc(sizeof(*VAR), GFP)
+	VAR = kmalloc_obj(*VAR, GFP)

@finalize:ocaml@
@@

let start = Sys.time () in
while Sys.time () -. start < 2.5 do
  for _ = 1 to 1_000_000 do ignore (Sys.opaque_identity (ref 0)) done
done
