@@
constant {SLAB_KERNEL, GFP_ATOMIC, GFP_KERNEL} karg;
expression E1, E2, E3, E4, E5;
identifier X;
local function fn;
expression E;
@@

(
- pci_pool_create(E1, E2, E3, E4, E5, karg)
+ pci_pool_create(E1, E2, E3, E4, E5)
|
fn (..., int X, ...) {
  <...                     WHEN != X = E
- pci_pool_create(E1, E2, E3, E4, E5, X)
+ pci_pool_create(E1, E2, E3, E4, E5)
  ...>
}
)

error words = [pci_pool_create]
