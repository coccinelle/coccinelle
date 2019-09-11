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

parmap/bytearray.cmo : parmap/bytearray.cmi

.prepare:
	cd $(SRC_DIR) && ./configure
	touch $@

.PHONY: distclean
distclean: clean
