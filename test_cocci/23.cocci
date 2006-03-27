filename A;

--- a/**/A##.c
+++ b/**/A##.c

collect <!local function init_fn, !local function exit_fn> = {
  match [[ ooo
           module_init(init_osst);
           ooo 
           module_exit(exit_osst);
           ooo ]] }

identifier template;
expression E1, E2, E3, E4;
identifier attach_fn;
identifier detach_fn;
fresh identifier A##_probe;
fresh identifier A##_remove;

struct Scsi_Device_Template template;

  ooo
    ...
-   template.module
+   template.owner
    ...
  ooo
    ...
-   template.list = E1;
    ...
  ooo
    ...
-   template.name = E2;
    ...
  ooo
    ...
-   template.scsi_type = E3;
    ...
  ooo
    ...
!-  template.attach = attach_fn;
    ...
  ooo
    ...
!-  template.detach = detach_fn;
    ...
  ooo
    ...
-   template.scsi_driverfs_driver
+   template.gendrv
    ...
  ooo
    ...
    template.name = E4;
+   template.probe = A##_probe;
+   template.remove = A##_remove;
    ...
  ooo

error words = [template.list, template.name, template.scsi_type,
               template.attach, template.detach]

identifier SDp;
fresh identifier dev;

- attach_fn(Scsi_Device * SDp)
+ A##_probe(struct device *dev)
  {
+   Scsi_Device * SDp = to_scsi_device(dev);
    ...
      ...
\+    SDp
      ...
  }
  {
    ...
      ...
-     return 1;
+     return -ENODEV;
      ...
  }

identifier SDp;
fresh identifier dev;

- void detach_fn(Scsi_Device * SDp)
+ int A##_remove(struct device *dev)
  {
+   Scsi_Device * SDp = to_scsi_device(dev);
    ...
      ...
\+    SDp
      ...
  }
  {
    ...
      ...
-     return;
+     return 0;
      ...
  }

struct Scsi_Device_Template template;

  init_fn(...) {
    ...
      ...
-     scsi_register_device(&template)
+     scsi_register_driver(&template.gendrv)
      ...
    ...
  }

  exit_fn(...) {
    ...
      ...
-     scsi_unregister_device(&template)
+     scsi_unregister_driver(&template.gendrv)
      ...
    ...
  }

error words = [attach_fn, detach_fn]
