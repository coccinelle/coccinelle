SUBDIR=manual

.PHONY: all clean distclean spatch.1

-include ../Makefile.config

all:: spatch.1

spatch.1: spatch.1.in
	sed "s|SHAREDIR|$(SHAREDIR)|g" $^ > $@

clean distclean::
	rm -f spatch.1

all clean distclean::
	make -C $(SUBDIR) $@
