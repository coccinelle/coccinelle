@ non_delayed_fn @
type local_type, T;
local_type *device;
identifier fld, fn;
@@

-	INIT_WORK(&device->fld, fn, device);
+	INIT_WORK(&device->fld, fn);
