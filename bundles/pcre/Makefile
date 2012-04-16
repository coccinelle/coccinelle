# This Makefile serves as a wrapper to bundle the
# extlib module without modifications.

ifneq ($(MAKECMDGOALS),distclean)
include ../../Makefile.config
endif

OCAMLCFLAGS ?= -g -dtypes
OPTFLAGS ?= -g -dtypes

OCAMLC_CMD=$(OCAMLC) $(OCAMLCFLAGS) -thread $(PCRE_CFLAGS:%=-ccopt %) $(PCRE_LIBS:%=-ccopt %) -cc "${CC} ${CFLAGS}"
OCAMLOPT_CMD=$(OCAMLOPT) $(OPTFLAGS) -thread $(PCRE_CFLAGS:%=-ccopt %) $(PCRE_LIBS:%=-ccopt %)
OCAMLMKLIB_CMD=$(OCAMLMKLIB) -custom

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

clean:
	rm -f dllpcre_stubs.so libpcre_stubs.a pcre_stubs.o \
		pcre.cma pcre.cmi pcre.cmo META \
		pcre.a pcre.cmx pcre.cmxa pcre.o

distclean: clean
	rm -rf $(PCRE_DIR)

depend: $(PCRE_MARKER)

.PHONEY: all all.opt clean distclean depend all-build all-opt-build

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

pcre.cma: pcre.cmo libpcre_stubs.a
	$(OCAMLC_CMD) -a -o $@ $<

pcre.cmx: $(PCRE_LIB)/pcre.ml pcre.cmi
	$(OCAMLOPT_CMD) -c -o $@ $<

pcre.cmxa: pcre.cmx libpcre_stubs.a
	$(OCAMLOPT_CMD) -a -o pcre.cmxa $<

pcre_stubs.o: $(PCRE_LIB)/pcre_stubs.c
	$(OCAMLC_CMD) $<

libpcre_stubs.a: pcre_stubs.o
	$(OCAMLMKLIB_CMD) -oc pcre_stubs $<

META: $(PCRE_LIB)/META
	cp "${PCRE_LIB}/META" ./META
