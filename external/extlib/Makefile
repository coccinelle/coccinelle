# This Makefile serves as a wrapper to bundle the
# extlib module without modifications.

include ../../Makefile.config

VERSION=1.5.2
EXTLIB=extlib-${VERSION}

DEPS=$(EXTLIB)/*.ml $(EXTLIB)/*.mli $(EXTLIB)/META

MAKE_CMD=$(MAKE) -C $(EXTLIB)

all: extLib.cma META
all.opt: extLib.cmxa META

clean:
	rm -f extLib.cma extLib.cmxa extLib.a META

distclean: clean

depend:

.PHONEY: all all.opt clean distclean depend


extLib.cma: $(DEPS)
	$(MAKE_CMD) all
	cp "${EXTLIB}/extLib.cma" ./extLib.cma
	$(MAKE_CMD) clean
	@echo "build extLib.cma"

extLib.cmxa: $(DEPS)
	$(MAKE_CMD) opt
	cp "${EXTLIB}/extLib.cmxa" ./extLib.cmxa
	cp "${EXTLIB}/extLib.a" ./extLib.a
	$(MAKE_CMD) clean
	@echo "build extLib.cmxa"

META: $(DEPS)
	cp "${EXTLIB}/META" ./META
