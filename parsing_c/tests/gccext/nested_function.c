
static int __init amb_talk (amb_dev * dev) {
  adap_talk_block a;
  unsigned char pool;
  unsigned long timeout;
  
  u32 x (void * addr) {
    return cpu_to_be32 (virt_to_bus (addr));
  }
}


