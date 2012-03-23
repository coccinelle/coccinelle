# The original type conv package is bundled without modifications.
# This Makefile specifies how to build the package for integration
# with coccinelle.

include ../../Makefile.config

OCAMLC_CMD=$(OCAMLC) -thread -I "${PATH_camlp4}"
OCAMLOPT_CMD=$(OCAMLOPT) -thread -I "${PATH_camlp4}"
CAMLP4_CMD=$(CAMLP4O) pa_extend.cmo q_MLast.cmo

TYCONV_VERSION=3.0.4
TYCONV_LIB=type-conv-$(TYCONV_VERSION)/lib

all: pa_type_conv.cma META
all.opt: all pa_type_conv.cmxa META
  # the standard package does not build an optimized version apparently

clean:
	rm -f pa_type_conv.cma pa_type_conv.cmxa pa_type_conv.o \
		pa_type_conv.cmi META pa_type_conv.a pa_type_conv.cmo pa_type_conv.cmx

distclean: clean

depend:

.PHONEY: all all.opt clean distclean depend


# handle the building of type-conv ourselves

pa_type_conv.cmi: $(TYCONV_LIB)/pa_type_conv.mli
	$(OCAMLC_CMD) -c -o $@ $^

pa_type_conv.cmo: $(TYCONV_LIB)/pa_type_conv.ml pa_type_conv.cmi
	$(OCAMLC_CMD) -pp "${CAMLP4_CMD}" -c -o $@ $<

pa_type_conv.cma: pa_type_conv.cmo
	$(OCAMLC_CMD) -a -custom -o $@ $^

pa_type_conv.cmx: $(TYCONV_LIB)/pa_type_conv.ml pa_type_conv.cmi
	$(OCAMLOPT_CMD) -pp "${CAMLP4_CMD}" -c -o $@ $<

pa_type_conv.cmxa: pa_type_conv.cmx
	$(OCAMLOPT_CMD) -a -o pa_type_conv.cmxa $<

META: $(TYCONV_LIB)/META
	cp "${TYCONV_LIB}/META" ./META
