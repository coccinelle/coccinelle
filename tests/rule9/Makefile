SPATCH=../../spatch
ISO=../../standard.iso
ARGS=-iso_file $(ISO) -cocci_file $(SP) -compare_with_expected

SP=rule9.cocci

SOURCES = cciss_scsi.c \
          sbpc2.c \
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

SOURCES = scsiglue.c 53c700.c


OBJS = $(SOURCES:.c=.ok)

all: $(OBJS)
	ls *ok *failed

.SUFFIXES:
.SUFFIXES: .c .ok

.c.ok: $(SPATCH) $(SP)
	/bin/rm -f $@ $(<:.c=.failed)
	($(SPATCH) $(ARGS) $< && touch $(<:.c=.ok)) || (diff -u -b /tmp/output.c $(<:.c=.res) > $(<:.c=.failed))

