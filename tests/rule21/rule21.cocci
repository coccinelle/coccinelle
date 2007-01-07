@@
//fresh identifier agp_driver_struct;
function fn;
type T;
identifier ent,dev;
@@

// want: + static struct agp_driver agp_driver_struct = {
// and for fn, want: fn (struct pci_dev *dev, ...) {
+ static struct agp_driver agp_driver_struct = {
+   .owner = THIS_MODULE,
+ };
T fn (struct pci_dev *dev, struct pci_device_id *ent) {
    ...
-   agp_register_driver(dev);
+   agp_driver_struct.dev = dev;
+   agp_register_driver(&agp_driver_struct);
    ...
  }

@@ @@
-   agp_unregister_driver();
+   agp_unregister_driver(&agp_driver_struct);


