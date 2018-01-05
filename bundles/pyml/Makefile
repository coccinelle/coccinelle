# This Makefile serves as a wrapper to bundle the
# pyml module without modifications.

PYML_VERSION=current

PACKAGE_DIR=pyml-$(PYML_VERSION)
TARBALL=
MODULES=pyml_arch pyutils pytypes pywrappers py pycaml
PRODUCTS=pyml_arch
OBJS=$(MODULES)
INTERFACES=pywrappers py pycaml
C_OBJS=pyml
ARCHIVE=pyml
LIBRARY=pyml
SRC_DIR=$(PACKAGE_DIR)
OCAMLFLAGS+=-I ../stdcompat

ARCH:=$(shell uname)

ifeq ($(ARCH),Linux)
	PYML_ARCH=pyml_arch_linux.ml
else ifeq ($(ARCH),Darwin)
	PYML_ARCH=pyml_arch_darwin.ml
else ifeq ($(findstring CYGWIN,$(ARCH)),CYGWIN)
	PYML_ARCH=pyml_arch_cygwin.ml
else
	$(error Unsupported OS $(ARCH)
endif

include ../Makefile.bundles

distclean: clean
	rm -f .prepare

$(SRC_DIR)/pywrappers.ml \
$(SRC_DIR)/pyml_wrappers.inc: \
		$(MARKER) $(SRC_DIR)/generate
	cd $(SRC_DIR) && ./generate

$(SRC_DIR)/pyml_wrappers.inc: $(SRC_DIR)/pywrappers.ml

.prepare: $(SRC_DIR)/pywrappers.ml $(SRC_DIR)/pywrappers.mli
	touch .prepare

$(SRC_DIR)/pyml_stubs.o: $(SRC_DIR)/pyml_stubs.c $(SRC_DIR)/pyml_wrappers.inc

$(SRC_DIR)/pyml_arch.mli \
$(SRC_DIR)/$(PYML_ARCH) \
$(SRC_DIR)/generate.ml \
$(SRC_DIR)/pytypes.mli: $(MARKER)

$(SRC_DIR)/pywrappers.mli: \
	$(SRC_DIR)/pywrappers.ml $(SRC_DIR)/pytypes.cmi $(SRC_DIR)/pyml_arch.cmi
	$(OCAMLC_CMD) -i $< >$@

$(SRC_DIR)/pyml_arch.cmx: $(SRC_DIR)/pyml_arch.ml $(SRC_DIR)/pyml_arch.cmi
	$(OCAMLOPT_CMD) -c $< -o $@

$(SRC_DIR)/pyml_arch.cmo: $(SRC_DIR)/pyml_arch.ml $(SRC_DIR)/pyml_arch.cmi
	$(OCAMLC_CMD) -c $< -o $@

$(SRC_DIR)/generate: $(SRC_DIR)/generate.cmx
	$(OCAMLOPT) $(OCAMLFLAGS) stdcompat.cmx $^ -o $@

$(SRC_DIR)/generate.cmx: $(SRC_DIR)/generate.ml
	$(OCAMLOPT_CMD) -c $< -o $@

$(SRC_DIR)/pyml_arch.ml: $(SRC_DIR)/$(PYML_ARCH)
	cp $< $@
