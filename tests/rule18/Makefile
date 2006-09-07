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

.SUFFIXES:
.SUFFIXES: .c .ok

.c.ok: $(SPATCH) $(SP)
	/bin/rm -f $@ $(<:.c=.failed)
	test -e $(SPATCH) && \
	(time $(SPATCH) $(ARGS) $< && \
	diff -b -B /tmp/output.c $(<:.c=.res) && touch $@) || \
	touch $(<:.c=.failed)
