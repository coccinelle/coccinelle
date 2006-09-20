// Test program for CPQFCTS ioctl calls
// build with:
// gcc -o cpqioctl cpqioctl.c
// ld -o cpqioctl /lib/crt0.o cpqioctl.o -lc

#include <stdio.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <fcntl.h>
#include <unistd.h>
#include <linux/types.h>
#include "../../include/scsi/scsi.h"
#include "cpqfcTSioctl.h"

typedef struct scsi_fctargaddress {
        unsigned long host_port_id;
        unsigned char host_wwn[8];
} Scsi_FCTargAddress;

int main(int argc, char **argv) {

 int fd, i;
 Scsi_FCTargAddress targ;
 int uselect=0;



  if ( argc < 2 ) {
    printf("usage: cpqioctl <Devfile>\n");
    exit(1);
  }

  if ( (fd = open(argv[1], O_RDONLY)) == -1) {
    perror("open");
    exit(1);
  }

  if ( ioctl(fd, SCSI_IOCTL_FC_TARGET_ADDRESS, &targ) ) {
    perror("ioctl");
    exit(1);
  }


  printf("portid: %08x. wwn: ", targ.host_port_id);

  for (i=0;i<8;i++) printf(" %02x", targ.host_wwn[i]);
  printf("\n");

 while( uselect != 27 ) // not ESC key
  {
    printf("\n IOCTL \n");
    printf( "1. Get PCI info\n");
    printf( "2. Send Passthru\n");
    printf( " ==> ");
    scanf("%c", &uselect);

    switch( uselect  )
    {
      case '1':
      {
        cciss_pci_info_struct pciinfo;
	
  	if( ioctl( fd, CCPQFCTS_GETPCIINFO ,&pciinfo ))
          perror("ioctl");
	else
          printf( "\nPCI bus %d, dev_fn %d, board_id %Xh\n",
            pciinfo.bus, pciinfo.dev_fn, pciinfo.board_id);
      }

    }
  }


  close(fd);
  return 0;
} 
