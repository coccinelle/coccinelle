# This Makefile serves as a wrapper to bundle the
# extlib module without modifications.

ifneq ($(MAKECMDGOALS),distclean)
include ../../Makefile.config
endif

OCAMLCFLAGS ?= -g
OPTFLAGS ?= -g

# note: the extra linker flags are needed so that the produced shared
# library gets dynamically linked to libpcre.
EXTRACFLAGS=$(PCRE_CFLAGS:%=-ccopt %) $(PCRE_LIBS:%=-ccopt %)
EXTRALNKFLAGS=$(PCRE_LIBS:%=-ldopt %)

OCAMLC_CMD=$(OCAMLC) $(OCAMLCFLAGS) $(EXTRACFLAGS) -cc "${CC} ${CFLAGS}"
OCAMLOPT_CMD=$(OCAMLOPT) $(OPTFLAGS) $(EXTRACFLAGS)
OCAMLMKLIB_CMD=$(OCAMLMKLIB) $(EXTRALNKFLAGS)

PCRE_VERSION=6.2.5
PCRE_DIR=pcre-ocaml-release-$(PCRE_VERSION)
PCRE_LIB=$(PCRE_DIR)/lib
PCRE_MARKER=$(PCRE_DIR)/.marker

all: $(PCRE_MARKER)
	@$(MAKE) all-build
all-build: $(PCRE_MARKER) pcre.cma META
all.opt: $(PCRE_MARKER)
	@$(MAKE) all-opt-build
all-opt-build: $(PCRE_MARKER) pcre.cmxa META

# prepares the source bundle for building.
.prepare: $(PCRE_MARKER)
	touch .prepare

clean:
	rm -f dllpcre_stubs.so libpcre_stubs.a pcre_stubs.o \
		pcre.cma pcre.cmi pcre.cmo META \
		pcre.a pcre.cmx pcre.cmxa pcre.o pcre.annot

distclean: clean
	rm -rf $(PCRE_DIR)
	rm -f .prepare

depend: $(PCRE_MARKER)

.PHONY: all all.opt clean distclean depend all-build all-opt-build

# prepare the pcre directory
$(PCRE_MARKER): $(PCRE_DIR).tar.gz
	$(TAR) xfvz $<
	touch $@

$(PCRE_DIR).tar.gz:
	@echo "$@ not found. Please download it and drop it in this directory ($(pwd))."
	@false

# handle the building of pcre ourselves

pcre.cmi: $(PCRE_LIB)/pcre.mli
	$(OCAMLC_CMD) -c -o $@ $^

pcre.cmo: $(PCRE_LIB)/pcre.ml pcre.cmi
	$(OCAMLC_CMD) -c -o $@ $<

pcre.cma: pcre.cmo dllpcre_stubs.so
	$(OCAMLC_CMD) -a -o $@ $<

pcre.cmx: $(PCRE_LIB)/pcre.ml pcre.cmi
	$(OCAMLOPT_CMD) -c -o $@ $<

pcre.cmxa: pcre.cmx dllpcre_stubs.so
	$(OCAMLOPT_CMD) -a -o pcre.cmxa $<

pcre_stubs.o: $(PCRE_LIB)/pcre_stubs.c
	$(OCAMLC_CMD) -ccopt -shared -ccopt -fPIC $<

dllpcre_stubs.so: pcre_stubs.o
	$(OCAMLMKLIB_CMD) -o pcre_stubs $<

META: $(PCRE_LIB)/META
	cp "${PCRE_LIB}/META" ./META

include ../../Makefile.common
