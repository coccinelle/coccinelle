SPATCH=../../spatch
SP=?

#pad: note that many .c contain modifications not relevant for rule83.
# the micro-patch is not that clean :(
# see the file.gitinfo to select the good C files.

SOURCES = ?

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
