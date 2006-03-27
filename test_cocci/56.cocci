@@
statement S1, S2;
@@

(
- if (pci_present()) S1 else S2
+ S1
|
- pci_present()
+ 1
)
