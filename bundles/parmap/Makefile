# This Makefile serves as a wrapper to bundle the
# parmap package without modifications.

PACKAGE_DIR=parmap
MODULES=bytearray parmap_utils setcore parmap
OBJS=$(MODULES)
INTERFACES=$(OBJS)
C_OBJS=bytearray setcore
ARCHIVE=parmap
LIBRARY=parmap
SRC_DIR=$(PACKAGE_DIR)

OCAMLCCFLAGS+=-ccopt -D_GNU_SOURCE

include ../Makefile.bundles

.prepare:
	cd $(SRC_DIR) && ./configure
	touch $@

distclean: clean
	rm -rf $(PACKAGE_DIR)
	rm -f .prepare
