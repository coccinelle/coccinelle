@ device_arg disable all @
type T1;
type local_type;
local_type *device;
identifier fld, fn;
@@


	INIT_WORK(&device->fld,
-		 (T1)fn, device
+		 fn
	);
