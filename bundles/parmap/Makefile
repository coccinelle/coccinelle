# This Makefile serves as a wrapper to bundle the
# parmap package without modifications.

PARMAP_VERSION=1.0-rc7.1

PACKAGE_DIR=parmap.$(PARMAP_VERSION)
TARBALL=parmap.1.0-rc7.1+opam.tar.gz
MODULES=bytearray parmap_utils setcore parmap
OBJS=$(MODULES)
INTERFACES=$(OBJS)
C_OBJS=bytearray setcore
ARCHIVE=parmap
LIBRARY=parmap
SRC_DIR=$(PACKAGE_DIR)

OCAMLCCFLAGS+=-ccopt -D_GNU_SOURCE

include ../Makefile.bundles

.prepare: $(MARKER)
	cd $(SRC_DIR) && ./configure
	touch $@
