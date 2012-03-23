# This Makefile serves as a wrapper to bundle the
# extlib module without modifications.

include ../../Makefile.config

SHARE_FLAGS=-shared -fpic
OCAMLC_CMD=$(OCAMLC) -thread -ccopt "${PCRE_CFLAGS}" -cclib "${PCRE_LIBS}" -cc "${CC} ${CFLAGS} ${SHARE_FLAGS}"
OCAMLOPT_CMD=$(OCAMLOPT) -thread -ccopt "${PCRE_CFLAGS}" -cclib "${PCRE_LIBS}"
OCAMLMKLIB_CMD=$(OCAMLMKLIB)

PCRE_VERSION=6.2.5
PCRE_LIB=pcre-ocaml-release-$(PCRE_VERSION)/lib

all: pcre.cma META
all.opt: pcre.cmxa META

clean:
	rm -f dllpcre_stubs.so libpcre_stubs.a pcre_stubs.o \
		pcre.cma pcre.cmi pcre.cmo META \
		pcre.a pcre.cmx pcre.cmxa pcre.o

distclean: clean

depend:

.PHONEY: all all.opt clean distclean depend


# handle the building of pcre ourselves

pcre.cmi: $(PCRE_LIB)/pcre.mli
	$(OCAMLC_CMD) -c -o $@ $^

pcre.cmo: $(PCRE_LIB)/pcre.ml pcre.cmi
	$(OCAMLC_CMD) -c -o $@ $<

pcre.cma: pcre.cmo dllpcre_stubs.so
	$(OCAMLC_CMD) -a -custom -linkall -dllib -lpcre_stubs -o $@ $<

pcre.cmx: $(PCRE_LIB)/pcre.ml pcre.cmi
	$(OCAMLOPT_CMD) -c -o $@ $<

pcre.cmxa: pcre.cmx dllpcre_stubs.so
	$(OCAMLOPT_CMD) -a -cclib -lpcre_stubs -o pcre.cmxa $<

pcre_stubs.o: $(PCRE_LIB)/pcre_stubs.c
	$(OCAMLC_CMD) $<

dllpcre_stubs.so: pcre_stubs.o
	$(OCAMLMKLIB_CMD) -o pcre_stubs $<

META: $(PCRE_LIB)/META
	cp "${PCRE_LIB}/META" ./META
