@@
statement S;
@@

- if (pci_present()) S

@@
expression E;
statement S1, S2;
@@

- if (pci_present() && E)
+ if (E)
  S1 else S2

@@
expression E;
statement S1, S2;
@@

- if (E && pci_present())
+ if (E)
  S1 else S2

@@
type T;
identifier x;
statement S1, S2;
@@

(
- if (pci_present())
   {
    <...
\+  T x;
    ...>
  }
- else S2
|
- if (pci_present()) {
    ...
-  }
- else S2
|
- if (pci_present())
  S
- else S2
)
