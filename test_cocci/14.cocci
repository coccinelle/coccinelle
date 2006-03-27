@@
local function probe;
struct pci_driver s;
@@

s.probe = probe;


@@
local function fn1, fn2;
expression list args1, args2;
parameter list params1;
statement X, Y;
statement list Z;
struct agp_bridge_data agp_bridge;
expression E;
@@

  probe(...) {
    <ooo WHEN != 3 //agp_bridge.capndx = E;
        toto;
//-   if (fn1(args1) == 0) X else Y
//+   Z[args1/params1]
//+   if (g(args2[args1/params1])) X else Y
    ooo>
  }
  ooo
  fn1(params1) {
    Z
    return fn2(args2);
  }

@@
fresh identifier cap_ptr;
expression dev, E;
statement X, Y;
@@

  probe(...) {
+   u8 cap_ptr = 0;
    <ooo WHEN != agp_bridge.capndx = E;
(
-   agp_bridge.dev = dev;
|
-   if (pci_find_capability(dev,PCI_CAP_ID_AGP)==0) X else Y
+   cap_ptr = pci_find_capability(dev,PCI_CAP_ID_AGP);
+   if (cap_ptr==0) X else Y
)
    ooo>
  }

@@
expression dev, E;
@@

  probe(...) {
(
    ...
    agp_bridge.capndx = E;
    ...
|
    ...
+   agp_bridge.dev = dev;
+   agp_bridge.capndx = cap_ptr;
+   pci_read_config_dword(agp_bridge.dev,agp_bridge.capndx+4, &agp_bridge.mode);
    ...
    agp_register_driver(dev)
    ...
  }
)
