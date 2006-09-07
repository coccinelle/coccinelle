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
	/bin/rm -f $@ $(<:.c=.failed)
	test -e $(SPATCH) && \
	(time $(SPATCH) $(ARGS) $< && \
	diff -b -B /tmp/output.c $(<:.c=.res) && touch $@) || \
	touch $(<:.c=.failed)
