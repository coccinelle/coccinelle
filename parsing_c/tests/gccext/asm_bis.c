
//HANDLING: /tmp/linux-2.6.13/drivers/md/raid6mmx.c

void main(int i) {
  __asm__ volatile( "stwbrx %0,0,%1" : : "r" (x), "r" (a) : "memory");

  
  asm ("pushfl; popfl");
  asm("pxor %mm5,%mm5");

  __asm__ volatile( "stwbrx %0,0,%1" : : "r" (x), "r" (a) : "memory");
  return;
}

