@@
{struct input_dev *, struct input_device_id *, struct gameport *} E;
@@

(
  E->
-    idbus
+    id.bustype
|
  E->
-    idvendor
+    id.vendor
|
  E->
-    idproduct
+    id.product
|
  E->
-    idversion
+    id.version
)

@@
{struct input_dev, struct input_device_id, struct gameport} E;
@@

(
  E.
-   idbus
+   id.bustype
|
  E.
-   idvendor
+   id.vendor
|
  E.
-   idproduct
+   id.product
|
  E.
-   idversion
+   id.version
)

@@
identifier I;
expression E1, E2, E3, E4;
@@

static struct input_dev I = {
-	idbus:		E1,
-	idvendor:	E2,
-	idproduct:	E3,
-	idversion:	E4,
};

@@
@@

	input_register_device(&I);
+	I.id.bustype	=E1;
+	I.id.vendor	=E2;
+	I.id.product	=E3;
+	I.id.version	=E4;
