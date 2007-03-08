void main(int i)
{

   printk("Allocating %u for %u Exchanges ", 
 	  (ULONG)sizeof(FC_EXCHANGES), TACH_MAX_XID);

   pci_set_dma_mask(ha->pdev, (dma_addr_t) ~ 0ULL);


}
