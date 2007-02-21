@@
statement S;
@@

- if (pci_present() == 0) S

@@
expression E;
statement S1, S2;
@@

  if (
-     pci_present() != 0 && (E)
+     E
     ) S1 else S2

@@
expression E;
statement S1, S2;
@@

  if (
-     pci_present() != 0 && E
+     E
     ) S1 else S2

@@
expression E;
statement S1, S2;
@@

  if (
-     (E) && pci_present() != 0
+     E
     ) S1 else S2

@@
expression E;
statement S1, S2;
@@

  if (
-     E && pci_present() != 0
+     E
     ) S1 else S2
@@
type T;
identifier x;
statement S;
@@

(
- if (pci_present() != 0)
  {
    T x;
    ...
  }
- else S
|
- if (pci_present() != 0)
  {
    T x;
    ...
  }
|
- if (pci_present() != 0) {
    ...
-  }
- else S
|
- if (pci_present() != 0) {
    ...
-  }
|
- if (pci_present() != 0)
  S
- else S
|
- if (pci_present() != 0)
  S
)
