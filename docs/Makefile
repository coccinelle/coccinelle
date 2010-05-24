SUBDIR=manual

.PHONY: all clean distclean spatch.1

include ../Makefile.config

all: spatch.1
	make -C $(SUBDIR)

spatch.1: spatch.1.in
	sed "s|SHAREDIR|$(SHAREDIR)|g" $^ > $@

clean:
	make -C $(SUBDIR) clean
	rm -f spatch.1

distclean:
	make -C $(SUBDIR) distclean
	rm -f spatch.1
