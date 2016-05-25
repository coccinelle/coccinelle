# Bundles the original menhirLib package without modifications.
# This Makefile integrates its build process with coccinelle.

ifneq ($(MAKECMDGOALS),distclean)
include ../../Makefile.config
-include ../../Makefile.local
endif

VERSION=20160504
MENHIRDIR=menhir-$(VERSION)
MENHIRMARKER=$(MENHIRDIR)/.marker

all all.opt: .prepare menhir

# prepares the source bundle for building.
.prepare: $(MENHIRMARKER)
	touch .prepare

clean:
	rm -rf menhir menhirLib.* share
	- make -C $(MENHIRDIR) clean

distclean: clean
	rm -rf $(MENHIRDIR)
	rm -f .prepare

$(MENHIRMARKER): $(MENHIRDIR).tar.gz
	$(TAR) xfvz $<
	touch $@

$(MENHIRDIR).tar.gz:
	@echo "$@ not found. Please download it and drop it in this directory ($(pwd))."
	@false

.PHONY: all all.opt clean distclean depend

depend: $(MENHIRMARKER) menhir

menhir:
	unset OCAMLLIB && make -C $(MENHIRDIR) PREFIX=$(CURDIR) all
	cp $(MENHIRDIR)/src/_stage2/menhir.native  menhir
	cp $(MENHIRDIR)/src/_stage2/menhirLib.* .
	mkdir -p share/menhir/
	cp $(MENHIRDIR)/src/standard.mly share/menhir/

include ../../Makefile.common
