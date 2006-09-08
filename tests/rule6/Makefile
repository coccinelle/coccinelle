SPATCH=../../spatch
SP=mem_map.cocci
ARGS=-iso_file ../../standard.iso -compare_with_expected -cocci_file $(SP)

SOURCES = 

OBJS = $(SOURCES:.c=.ok)

all: $(OBJS)
	ls *ok *failed

.SUFFIXES:
.SUFFIXES: .c .ok

.c.ok: $(SPATCH) $(SP)
	/bin/rm -f $@ $(<:.c=.failed) /tmp/output.c
	test -e $(SPATCH) && \
	(time $(SPATCH) $(ARGS) $< && touch $@ || touch $(<:.c=.failed))
	test -e /tmp/output.c && \
	(diff -b -B -w /tmp/output.c $(<:.c=.res) || true)
