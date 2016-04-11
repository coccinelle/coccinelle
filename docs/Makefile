SUBDIR=manual

.PHONY: all clean distclean spatch.1

ifneq ($(MAKECMDGOALS),distclean)
include ../Makefile.config
-include ../Makefile.local
endif

all: spatch.1
	$(MAKE) -C $(SUBDIR) $@

distclean:
	$(MAKE) -C manual distclean
	rm -f spatch.1
	rm -rf ./html
	rm -rf ./man

clean:
	$(MAKE) -C $(SUBDIR) $@

include ../Makefile.common
