// Not delayed
@ non_delayed_fn @
type local_type, T,T1;
local_type *device;
identifier fld, fn;
@@

-	INIT_WORK(&device->fld, (T)fn, (T1)device);
+	INIT_WORK(&device->fld, fn);
