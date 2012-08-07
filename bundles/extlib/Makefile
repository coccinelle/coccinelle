# This Makefile serves as a wrapper to bundle the
# extlib package without modifications.

# the extlib package is a conventional package that
# requires a plain compilation of its .ml files and
# then packed into an archive file.

ifneq ($(MAKECMDGOALS),distclean)
include ../../Makefile.config
endif

OCAMLCFLAGS ?= -g
OPTFLAGS ?= -g

VERSION=1.5.2
EXTLIB=extlib-$(VERSION)
EXTMARKER=$(EXTLIB)/.marker

all: $(EXTMARKER)
	@$(MAKE) all-build
all-build: $(EXTMARKER) extLib.cma META

all.opt: $(EXTMARKER)
	@$(MAKE) all-opt-build
all-opt-build: $(EXTMARKER) extLib.cmxa META

clean:
	rm -f .depend META extLib.cm[ioxa] extLib.[ao] \
		$(EXTLIB)/*.cm[ioxa] extLib.cmxa \
		.mli-marker

distclean: clean
	rm -rf $(EXTLIB)

depend: $(EXTMARKER)

$(EXTMARKER): $(EXTLIB).tar.gz hashtable-ocaml4-compat.patch
	$(TAR) xfvz $<
	$(PATCH) -d $(EXTLIB) -p1 < ./hashtable-ocaml4-compat.patch
	touch $@

$(EXTLIB).tar.gz:
	@echo "$@ not found. Please download it and drop it in this directory ($(pwd))."
	@false

.PHONEY: all all.opt all-build all-opt-build clean distclean depend

# SOURCES
MODULES = \
 enum bitSet dynArray extArray extHashtbl extList extString global IO option \
 pMap std uChar uTF8 base64 unzip refList optParse dllist

MLI=$(MODULES:%=$(EXTLIB)/%.mli)
SRC=$(MODULES:%=$(EXTLIB)/%.ml) $(EXTLIB)/extLib.ml

# note: the interface files must be build at must once when compiling both 'all' and 'all.opt'
# otherwise errors such as 'module X makes inconsistent assumptions about interface Y' may
# arise. The dependencies on this marker establishes this sharing.
.mli-marker: $(MLI) $(SRC)
	$(OCAMLC) $(OCAMLCFLAGS) -c -I $(EXTLIB) $(MLI)
	touch .mli-marker

extLib.cma: .mli-marker $(SRC)
	$(OCAMLC) $(OCAMLCFLAGS) -a -I $(EXTLIB) -o extLib.cma $(MLI) $(SRC)

extLib.cmxa: .mli-marker $(SRC)
	$(OCAMLOPT) $(OPTFLAGS) -a -I $(EXTLIB) -o extLib.cmxa $(SRC)

META: $(EXTLIB)/META
	cp $< $@

include ../../Makefile.common
