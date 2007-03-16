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


patch_size: 
	~/coccinelle/scripts/patch_from_c_and_res.pl $(SOURCES)