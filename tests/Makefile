SPATCH=../spatch
ISO=../standard.iso
ARGS=-iso_file $(ISO) -cocci_file $(SP) -compare_with_expected

SP=stm10.cocci

SOURCES = stm10.c stm10_ver1.c


OBJS = $(SOURCES:.c=.ok)

all: $(OBJS)
	ls *ok *failed

#pad: does not work because the old suffix rule does not allow additionnal
#dependencies (at the right of ':'). Cf make info manual.
#.SUFFIXES: .c .ok
#.c.ok: $(SPATCH) $(SP)

%.ok: %.c $(SPATCH) $(SP)
	/bin/rm -f $@ $(<:.c=.failed)
	($(SPATCH) $(ARGS) $< && touch $(<:.c=.ok)) || (diff -u -b /tmp/output.c $(<:.c=.res) > $(<:.c=.failed)) || echo done

