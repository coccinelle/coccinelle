//----------------------------------------------------------------------
// Rule 82
//----------------------------------------------------------------------

//----------------------------------------------------------------------
@@
expression E1, E2;
@@

- pci_save_state(E1,E2)
+ pci_save_state(E1)


//----------------------------------------------------------------------
@@
expression E1, E2;
@@

- pci_restore_state(E1,E2)
+ pci_restore_state(E1)
