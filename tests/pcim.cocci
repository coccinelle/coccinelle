@@
expression E1,E2,E3;
@@
- pci_map_single(E1,
+ dma_map_single(
+    &E1->dev,
     E2, E3,
-    PCI_DMA_FROMDEVICE)
+    DMA_FROM_DEVICE)