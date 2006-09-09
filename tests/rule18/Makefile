SPATCH=../../spatch
SP=schedev.cocci
ARGS=-iso_file ../../standard.iso -compare_with_expected -cocci_file $(SP)

SOURCES = avm_pci.c \
          config.c \
          diva.c \
          elsa_ser.c \
          hfc_2bds0.c \
          hfc_2bs0.c \
          hfc_pci.c \
          hfc_sx.c \
          hscx.c \
          hscx_irq.c \
          ipacx.c \
          isar.c \
          jade_irq.c \
          netjet.c \
          st5481_b.c \
          w6692.c

OBJS = $(SOURCES:.c=.ok)

all: $(OBJS)
	ls *ok *failed

%.ok: %.c $(SPATCH) $(SP)
	/bin/rm -f $(<:.c=.ok) $(<:.c=.failed) /tmp/output.c
	time $(SPATCH) $(ARGS) $< && touch $(<:.c=.ok) || touch $(<:.c=.failed)
	test -e /tmp/output.c && \
	(diff -b -B -w /tmp/output.c $(<:.c=.res) || true)
