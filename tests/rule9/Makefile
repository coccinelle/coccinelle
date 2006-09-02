SPATCH=../../spatch
SP=rule9.cocci

SOURCES = 

OBJS = $(SOURCES:.c=.ok)

all: $(OBJS)

.SUFFIXES:
.SUFFIXES: .c .ok

.c.ok: $(SPATCH) $(SP)
	/bin/rm -f $@
	time $(SPATCH) -compare_with_expected -cocci_file $(SP) $<
	test -e $(SPATCH) && \
	(diff -b -q /tmp/output.c $(<:.c=.res) && touch $@) || echo failed
