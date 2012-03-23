# This Makefile serves as a wrapper to bundle the
# extlib package without modifications.

# the extlib package is a conventional package that
# requires a plain compilation of its .ml files and
# then packed into an archive file.

include ../../Makefile.config

VERSION=1.5.2
EXTLIB=extlib-$(VERSION)

all: extLib.cma META
all.opt: extLib.cmxa META

clean:
	rm -f .depend META extLib.cm[ioxa] extLib.[ao] \
		$(EXTLIB)/*.cm[ioxa]

distclean: clean

depend:

.PHONEY: all all.opt clean distclean depend

# SOURCES
MODULES = \
 enum bitSet dynArray extArray extHashtbl extList extString global IO option \
 pMap std uChar uTF8 base64 unzip refList optParse dllist

MLI=$(MODULES:%=$(EXTLIB)/%.mli)
SRC=$(MODULES:%=$(EXTLIB)/%.ml) $(EXTLIB)/extLib.ml

extLib.cma: $(MLI) $(SRC)
	$(OCAMLC) -a -custom -thread -I $(EXTLIB) -o extLib.cma $(MLI) $(SRC)

extLib.cmxa: $(MLI) $(SRC)
	$(OCAMLC) -a -thread -I $(EXTLIB) -o extLib.cmxa $(SRC)

META: $(EXTLIB)/META
	cp $< $@
