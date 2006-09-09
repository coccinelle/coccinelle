SPATCH=../../spatch
SP=mem_map.cocci
ARGS=-iso_file ../../standard.iso -compare_with_expected -cocci_file $(SP)

SOURCES = 

OBJS = $(SOURCES:.c=.ok)

all: $(OBJS)
	ls *ok *failed

%.ok: %.c $(SPATCH) $(SP)
	/bin/rm -f $(<:.c=.ok) $(<:.c=.failed) /tmp/output.c
	time $(SPATCH) $(ARGS) $< && touch $(<:.c=.ok) || touch $(<:.c=.failed)
	test -e /tmp/output.c && \
	(diff -b -B -w /tmp/output.c $(<:.c=.res) || true)
