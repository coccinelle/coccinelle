@@
//fresh identifier agp_driver_struct;
function fn;
type T;
identifier dev;
@@

// want: + static struct agp_driver agp_driver_struct = {
+ struct agp_driver agp_driver_struct = {
+   .owner = THIS_MODULE,
+ };

//want: fn (struct pci_dev *dev, ...) {
T fn (struct pci_dev *dev) {
    ...
-   agp_register_driver(dev);
+   agp_driver_struct.dev = dev;
+   agp_register_driver(&agp_driver_struct);
    ...
  }

//@@ @@
//-   agp_unregister_driver();
//+   agp_unregister_driver(&agp_driver_struct);


