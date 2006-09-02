SPATCH=../../spatch
SP=rule9.cocci

SOURCES = rule9_ver10_cciss_scsi.c \
          rule9_ver11_sbpc2.c \
          rule9_ver12_53c700.c \
          rule9_ver13_acornscsi.c \
          rule9_ver14_arxescsi.c \
          rule9_ver15_cumana_2.c \
          rule9_ver16_eesox.c \
          rule9_ver17_powertec.c \
          rule9_ver18_cpqfcTSinit.c \
          rule9_ver19_eata_pio.c \
          rule9_ver20_fcal.c \
          rule9_ver21_g_NCR5380.c \
          rule9_ver22_in2000.c \
          rule9_ver23_ncr53c8xx.c \
          rule9_ver24_nsp32.c \
          rule9_ver25_nsp_cs.c \
          rule9_ver26_sym53c8xx.c \
          rule9_ver27_sym_glue.c \
          rule9_ver28_scsiglue.c 


OBJS = $(SOURCES:.c=.ok)

all: $(OBJS)
	ls *ok *failed

.SUFFIXES:
.SUFFIXES: .c .ok

.c.ok: $(SPATCH) $(SP)
	/bin/rm -f $@ $(<:.c=.failed)
	test -e $(SPATCH) && \
	(time $(SPATCH) -compare_with_expected -cocci_file $(SP) $< && \
	diff -b -q /tmp/output.c $(<:.c=.res) && touch $@) || \
	touch $(<:.c=.failed)
