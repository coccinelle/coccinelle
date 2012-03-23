# This Makefile serves as a wrapper to bundle the
# extlib module without modifications.

include ../../Makefile.config

OCAMLC_CMD=$(OCAMLC) -thread -I "${PATH_camlp4}"
OCAMLOPT_CMD=$(OCAMLOPT) -thread
CAMLP4_CMD=$(CAMLP4O) pa_extend.cmo q_MLast.cmo

# -parser Camlp4QuotationCommon -parser Camlp4QuotationExpander

VERSION=3.0.4
TYCONVLIB=type-conv-${VERSION}/lib

all: pa_type_conv.cma META
all.opt: all pa_type_conv.cmxa
  # the standard package does not build an optimized version apparently

clean:
	rm -f pa_type_conv.cma pa_type_conv.cmxa pa_type_conv.o \
		pa_type_conv.cmi

distclean: clean

depend:

.PHONEY: all all.opt clean distclean depend


# handle the building of type-conv ourselves

pa_type_conv.cmi: $(TYCONVLIB)/pa_type_conv.mli
	$(OCAMLC_CMD) -c -o $@ $^

pa_type_conv.cmo: $(TYCONVLIB)/pa_type_conv.ml pa_type_conv.cmi
	$(OCAMLC_CMD) -pp "${CAMLP4_CMD}" -c -o $@ $<

pa_type_conv.cma: pa_type_conv.cmo
	$(OCAMLC_CMD) -a -custom -o $@ $^

pa_type_conv.cmx: $(TYCONVLIB)/pa_type_conv.ml pa_type_conv.cmi
	$(OCAMLOPT_CMD) -c -o $@ $^

pa_type_conv.cmxa: pa_type_conv.cmx
	$(OCAMLOPT_CMD) -a -o pa_type_conv.cmxa $<

META: $(TYCONVLIB)/META
	cp "${TYCONVLIB}/META" ./META
