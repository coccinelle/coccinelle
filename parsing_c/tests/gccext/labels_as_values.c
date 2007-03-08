
void main(int i)
{


   __asm__ __volatile__ ("movec.l  %%vbr,%%a0\n\t"
                         "move.l   8(%%a0),%0\n\t"
                         "move.l   %1,8(%%a0)\n\t"
                         : "=&r" (save_buserr)
                             : "r" (&&scsi_bus_error)
                         : "a0" );
   return 1;

scsi_bus_error:
   return 0;

}
