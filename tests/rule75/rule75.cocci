@@
expression A, B;
statement S;
@@

- if (!pci_dma_supported(A,B)) S
- pci_set_consistent_dma_mask(A,B);
+ if (pci_set_dma_mask(A,B) < 0 || pci_set_consistent_dma_mask(A,B) < 0) S

@@
expression A, B;
statement S;
@@

- if (pci_set_consistent_dma_mask(A,B) < 0) S
+ if (pci_set_dma_mask(A,B) < 0 || pci_set_consistent_dma_mask(A,B) < 0) S

@@
expression A, B;
@@
// no if, so just drop it
- pci_set_consistent_dma_mask(A,B);

// TODO: 
//-          pci_set_dma_mask(pci, 0x0fffffff);
//+          pci_set_consistent_dma_mask(pci, 0x0fffffff);
