@@ expression A, B, C, D; @@
(
- pci_free_consistent(A,B,C,D)
+ dma_free_coherent(&A->dev,B,C,D)
|
- pci_alloc_consistent(A,B,C)
+ dma_alloc_coherent(&A->dev,B,C,GFP_KERNEL)
)
