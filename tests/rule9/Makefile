CEDESCRIPTION="\fname{proc_info}"
SP=rule9.cocci
#SP=rule9_plos_updated.cocci

SOURCES = cciss_scsi.c \
          sbp2.c \
          53c700.c \
          acornscsi.c \
          arxescsi.c \
          cumana_2.c \
          eesox.c \
          powertec.c \
          cpqfcTSinit.c \
          eata_pio.c \
          fcal.c \
          g_NCR5380.c \
          in2000.c \
          ncr53c8xx.c \
          nsp32.c \
          nsp_cs.c \
          sym53c8xx.c \
          sym_glue.c \
          scsiglue.c 

#SOURCES = scsiglue.c 53c700.c

include ../generic_makefile


#you have first to checkout to good branch in kernels/git/history/  !!
#git checkout -b before_proc_info 5476e4625bb98b3e6b1c233a429140f6e624acf1^

wholekernel:
	time find ~/kernels/git/history/sound ~/kernels/git/history/drivers  -name "*.c" -exec sh run_one {} \;

#drivers/usb/storage
#time find ~/kernels/git/history/ -name "*.c" -exec sh run_one {} \;
