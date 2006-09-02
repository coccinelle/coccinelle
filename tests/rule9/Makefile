SPATCH=../../spatch
SP=rule9.cocci

SOURCES = 

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
