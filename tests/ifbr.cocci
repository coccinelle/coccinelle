@@
expression test;
expression E;
@@

  if (test) {
    ...
(
+   pci_dev_put();
    return;
|
+   pci_dev_put();
    return ret;
)
  }
