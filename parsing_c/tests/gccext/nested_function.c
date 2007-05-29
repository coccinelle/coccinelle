
static int __init amb_talk (amb_dev * dev) {
  adap_talk_block a;
  unsigned char pool;
  unsigned long timeout;
  
  u32 x (void * addr) {
    return cpu_to_be32 (virt_to_bus (addr));
  }
}



static int __init amb_probe (void) {
  struct pci_dev * pci_dev;
  int devs;
  
   void __init do_pci_device (void) {
    amb_dev * dev;
   }
}


/*
nested function 
BAD:!!!!! static void OUT4500( struct airo_info *ai, u16 reg, u16 val ) {
bad: 	if (test_bit(FLAG_MPI,&ai->flags))
bad: 		reg <<= 1;
bad: 	if ( !do8bitIO )
bad: 		outw( val, ai->dev->base_addr + reg );
bad: 	else {
bad: 		outb( val & 0xff, ai->dev->base_addr + reg );
bad: 		outb( val >> 8, ai->dev->base_addr + reg + 1 );
bad: 	}
bad: }

nested function
bad: static int __init amb_probe (void) {
bad:   struct pci_dev * pci_dev;
bad:   int devs;
bad:   
BAD:!!!!!   void __init do_pci_device (void) {
bad:     amb_dev * dev;

*/
