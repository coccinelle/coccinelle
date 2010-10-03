@ non_delayed_fn disable all @
type local_type;
local_type *device;
identifier fld, fn;
@@

	INIT_WORK(&device->fld,
-		 fn, device
+		 fn
	);

@ rule7a disable all @
identifier dataq, non_delayed_fn.fn, non_delayed_fn.fld;
type non_delayed_fn.local_type;
@@

  fn (
-     void *dataq
+     struct work_struct *workq
     ) {
    <...
-   dataq
+   container_of(workq,local_type,fld)
    ...>
  }
