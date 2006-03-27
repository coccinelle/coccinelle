@@
fresh identifier agp_driver_struct;
type T;
function fn;
identifier dev;
@@

+ static struct agp_driver_struct = {
+   .owner = THIS_MODULE;
+ }

! T fn (struct pci_dev *dev, ...) {
    ...
-   agp_register_driver(dev);
+   agp_driver_struct.dev = dev;
+   agp_register_driver(&agp_driver_struct);
    ...
  }
  <...
!-  agp_unregister_driver();
+   agp_unregister_driver(&agp_driver_struct);
  ...>

error words = [agp_register_driver, agp_unregister_driver]
