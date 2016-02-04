@ rule1 @
fresh identifier agp_driver_struct;
function fn;
identifier ent,dev;
@@

+ static struct agp_driver agp_driver_struct = {
+   .owner = THIS_MODULE,
+ };
fn (struct pci_dev *dev, struct pci_device_id *ent) {
    ...
(
-   agp_register_driver(dev);
+   agp_driver_struct.dev = dev;
+   agp_register_driver(&agp_driver_struct);
|
    if (...) { // a non-error pathm, but looks like an error path
      ...
-     agp_register_driver(dev);
+     agp_driver_struct.dev = dev;
+     agp_register_driver(&agp_driver_struct);
      ...
      return 0;
    }
)
    ...
  }

@ rule2 extends rule1 @
@@
-   agp_unregister_driver();
+   agp_unregister_driver(&agp_driver_struct);
