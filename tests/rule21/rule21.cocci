@@
fresh identifier agp_driver_struct;
function fn;
identifier ent,dev;
@@

// and for fn, want: fn (struct pci_dev *dev, ...) {
+ static struct agp_driver agp_driver_struct = {
+   .owner = THIS_MODULE,
+ };
fn (struct pci_dev *dev, struct pci_device_id *ent) {
    ...
-   agp_register_driver(dev);
+   agp_driver_struct.dev = dev;
+   agp_register_driver(&agp_driver_struct);
    ...
  }

@@ @@
-   agp_unregister_driver();
+   agp_unregister_driver(&agp_driver_struct);
