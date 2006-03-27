@@
expression X;
@@

- scsi_malloc(X)
(
+ kmalloc(X,GFP_KERNEL)
|
+ kmalloc(X,GFP_DMA)
|
+ kmalloc(X,GFP_KERNEL|GFP_DMA)
)

@@
expression Y, Z;
@@

- scsi_free(Y, Z)
+ kfree(Y)
