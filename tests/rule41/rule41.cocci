@@
identifier I;
expression fops_val, minor_val, num_minor_val;
@@

struct usb_driver I = {
-	fops:		fops_val,
-	minor:		minor_val,
-	num_minors:	num_minor_val
};

@@
identifier retval;
statement S;
expression E1, E2
@@

	retval = usb_register_dev(
-                                 &I
+                                 fops_val, minor_val
                                  , E1, E2);
	if (retval) {
-		if (retval != -ENODEV) {
			...
			return ...;
-		}
-		...
	}

@@
expression E1, E2;
@@

  usb_deregister_dev(
-                    &I,
                     E1, E2);
