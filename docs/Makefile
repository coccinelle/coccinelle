SUBDIR=manual

.PHONY: all clean distclean spatch.1

ifneq ($(MAKECMDGOALS),distclean)
include ../Makefile.config
endif

all: spatch.1

distclean:
	$(MAKE) -C manual distclean
	rm -f spatch.1
	rm -rf ./html
	rm -rf ./man

all clean:
	$(MAKE) -C $(SUBDIR) $@
