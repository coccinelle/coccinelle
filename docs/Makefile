SUBDIR=manual

.PHONY: all clean distclean spatch.1

include ../Makefile.config

all:: spatch.1

distclean::
	rm -f spatch.1

all clean distclean::
	$(MAKE) -C $(SUBDIR) $@
