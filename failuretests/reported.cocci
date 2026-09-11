// Transform every function, then fail on purpose for the one in
// reported/fails.c, so that spatch has to abandon that file.

@r@
identifier fn;
expression VAR, GFP;
@@

fn(...)
{
<...
-	VAR = kmalloc(sizeof(*VAR), GFP)
+	VAR = kmalloc_obj(*VAR, GFP)
...>
}

@script:ocaml@
fn << r.fn;
@@

if fn = "fail_here" then failwith "injected failure"
