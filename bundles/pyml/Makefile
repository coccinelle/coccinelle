# This Makefile serves as a wrapper to bundle the
# pyml module without modifications.

PYML_VERSION=current

PACKAGE_DIR := pyml-$(PYML_VERSION)
TARBALL=
MODULES=pyml_arch pyutils pytypes pywrappers py pycaml
OBJS=$(MODULES)
INTERFACES=pywrappers py pycaml
C_OBJS=pyml
ARCHIVE=pyml
LIBRARY=pyml
SIDEPRODUCTS=
SRC_DIR := $(PACKAGE_DIR)
OTHERS := $(SRC_DIR)/pyml_arch.ml
OCAMLFLAGS+=-I $(STDCOMPATDIR)
CLEANOTHERS=pyml-current/generate pyml-current/generate.cmi \
	pyml-current/generate.cmx pyml-current/generate.cmo \
	pyml-current/generate.cmt pyml-current/generate.cmti \
	pyml-current/generate.o pyml-current/pyml.a \
	pyml-current/pyml.cma pyml-current/pyml.cmxa \
	pyml-current/dllpyml_stubs.so
GENERATED_SOURCE_FILES := $(SRC_DIR)/pywrappers.ml $(SRC_DIR)/pywrappers.mli
OCAMLCCFLAGS+=-ccopt -I$(STDCOMPATDIR)

C_COMPILER := $(shell ocamlc -config | grep '^native_c_compiler:' | cut -d ' ' -f 2)

include ../Makefile.bundles

ifeq ($(MAKECMDGOALS),all.opt)
	NATIVE := yes
else
	NATIVE := no
endif

include ../../Makefile.package

ifeq ($(NATIVE),yes)
	OCAMLCOPT := $(OCAMLOPT_CMD)
	CMOX := cmx
	CMAX := cmxa
	LINK_BEST := $(LINK_NATIVE)
else
	OCAMLCOPT := $(OCAMLC_CMD)
	CMOX := cmo
	CMAX := cma
	LINK_BEST := $(LINK_BYTECODE)
endif

ifneq ($(OCAMLATLEAST50),no)
OCAMLFLAGS += -I +unix
endif

distclean: clean
	rm -f .prepare

$(SRC_DIR)/pywrappers.ml \
$(SRC_DIR)/pyml_wrappers.inc: \
		$(MARKER) $(SRC_DIR)/generate
	cd $(SRC_DIR) && LD_LIBRARY_PATH=$(STDCOMPATDIR):$(LD_LIBRARY_PATH) ./generate

$(SRC_DIR)/pyml_wrappers.inc: $(SRC_DIR)/pywrappers.ml

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

$(SRC_DIR)/pyutils.cmo $(SRC_DIR)/pyutils.cmx : $(SRC_DIR)/pyutils.cmi

$(SRC_DIR)/generate : $(SRC_DIR)/pyutils.$(CMOX) $(SRC_DIR)/generate.$(CMOX)
	$(OCAMLC_CMD) $(OCAMLLDFLAGS) $(LINK_BEST) stdcompat.$(CMAX) unix.$(CMAX) $^ -o $@

$(SRC_DIR)/generate.cmx: $(SRC_DIR)/generate.ml
	$(OCAMLOPT_CMD) -c $< -o $@

$(SRC_DIR)/generate.cmo: $(SRC_DIR)/generate.ml
	$(OCAMLC_CMD) -c $< -o $@

$(SRC_DIR)/pyml_arch.ml : $(SRC_DIR)/pyml_arch.ml.c
	$(C_COMPILER) -E $< | sed '/^#/d' >$@
