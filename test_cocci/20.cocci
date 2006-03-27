@@
!dev_info_t dev_info;
!constant char *driver_name;
!local function attach_fn;
!local function detach_fn;
@@

register_pccard_driver(&dev_info, &attach_fn, &detach_fn);
ooo
dev_info = driver_name;

@@
filename A;
identifier serv;
error constant error_code;
expression E;
fresh error identifier error;
!local function init_fn;
@@

--- a/.../A##.c
+++ b/.../A##.c

@@
fresh identifier A##_driver;
@@

+ static struct pcmcia_driver A##_driver = {
+   .owner          = THIS_MODULE,
+   .drv            = {
+     .name   = driver_name,
+   },
+   .attach         = attach_fn,
+   .detach         = detach_fn,
+ };

   __init init_fn(...) {
-    servinfo_t serv;
     ...
-    CardServices(GetCardServicesInfo, &serv);
     ...
-    if (serv.Revision != CS_RELEASE_CODE) {
-      ...
-      return error_code;
-    }
     ...
(
-    register_pccard_driver(&dev_info, &attach_fn, &detach_fn);
-    return 0;
+    return pcmcia_register_driver(&driver);
|
+    {
+      int error;
-      register_pccard_driver(&dev_info, &attach_fn, &detach_fn);
+      error = pcmcia_register_driver(&driver);
+      if (error) {
+        return error;
+       }
+    }
     ...
     return E;
|
-    C[register_pccard_driver(&dev_info, &attach_fn, &detach_fn)]
+    C[pcmcia_register_driver(&driver)]
)
     ...
   }

@@
@@

-   unregister_pccard_driver(&dev_info);
+   pcmcia_unregister_driver(&A##_driver);

error words = [register_pccard_driver, unregister_pccard_driver]
