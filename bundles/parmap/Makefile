# This Makefile serves as a wrapper to bundle the
# parmap package without modifications.

ifneq ($(MAKECMDGOALS),distclean)
include ../../Makefile.config
endif

OCAMLCFLAGS ?= -g
OPTFLAGS ?= -g

PARMAP_VERSION=1.0-rc5-patched
PARMAP_DIR=parmap-$(PARMAP_VERSION)
PARMAP_ARCHIVE=parmap-$(PARMAP_VERSION).tar.gz
PARMAP_MARKER=$(PARMAP_DIR)/.marker

all all.opt: $(PARMAP_MARKER)
	cd $(PARMAP_DIR) && \
	  $(MAKE) && \
	  cd ..
	mv $(PARMAP_DIR)/_build/* .

clean:
	cd $(PARMAP_DIR) && \
	  $(MAKE) clean && \
	  cd ..
	rm -f *.a *.o *.ml* *.c *.h *.so _* *.cm* *.annot *.clib *build  ocamlc.where

distclean:
	rm -fr $(PARMAP_DIR)

depend: $(PARMAP_MARKER)

.PHONY: all all.opt clean disclean depend

$(PARMAP_MARKER): $(PARMAP_ARCHIVE)
	$(TAR) xfz $<
	cd $(PARMAP_DIR) && \
	  ./configure && \
	  cd ..
	touch $@

$(PARMAP_ARCHIVE):
	@echo "$@ not found. Please download it and drop it in this directory ($(pwd))."
	@false
