SPATCH=../../spatch
SP=schedev.cocci
ARGS=-iso_file ../../standard.iso -compare_with_expected -cocci_file $(SP)

SOURCES = amd7930_fn.c \
          config.c \
          hfc_2bds0.c \
          hfc_pci.c \
          hfc_sx.c \
          icc.c \
          ipacx.c \
          isac.c \
          jade.c \
          w6692.c

OBJS = $(SOURCES:.c=.ok)

all: $(OBJS)
	ls *ok *failed

%.ok: %.c $(SPATCH) $(SP)
	/bin/rm -f $(<:.c=.ok) $(<:.c=.failed) /tmp/output.c
	time $(SPATCH) $(ARGS) $< && touch $(<:.c=.ok) || touch $(<:.c=.failed)
	test -e /tmp/output.c && \
	(diff -b -B -w /tmp/output.c $(<:.c=.res) || true)
