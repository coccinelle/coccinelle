SPATCH=../../spatch
SP=schedev.cocci

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

.SUFFIXES:
.SUFFIXES: .c .ok

.c.ok: $(SPATCH) $(SP)
	/bin/rm -f $@ $(<:.c=.failed)
	test -e $(SPATCH) && \
	(time $(SPATCH) -compare_with_expected -cocci_file $(SP) $< && \
	diff -b -B /tmp/output.c $(<:.c=.res) && touch $@) || \
	touch $(<:.c=.failed)
