ifneq ($(MAKECMDGOALS),distclean)
include ../../Makefile.config
endif

OCAMLCFLAGS ?= -g -dtypes
OPTFLAGS ?= -g -dtypes

PYCAMLDIR=chemoelectric-pycaml-8614105
PYCAMLFILE=$(PYCAMLDIR).tar.gz
PYCAMLMARKER=$(PYCAMLDIR)/.marker

export PYMAJOR=$(shell echo ${PYTHON_VERSION} | sed -e 's/\..*//')
OCAMLFLAGS = -pp "camlp4o -parser Camlp4MacroParser -D PYMAJOR${PYMAJOR}"
OCAMLC_CMD=$(OCAMLC) $(OCAMLCFLAGS) $(OCAMLFLAGS) -g -thread $(PYTHON_CFLAGS:%=-ccopt %) $(PYTHON_LIBS:%=-ccopt %) -cc "${CC} ${CFLAGS}" -I $(PYCAMLDIR)
OCAMLOPT_CMD=$(OCAMLOPT) $(OPTFLAGS) $(OCAMLFLAGS) -g -thread $(PYTHON_CFLAGS:%=-ccopt %) $(PYTHON_LIBS:%=-ccopt %) -I $(PYCAMLDIR)
OCAMLMKLIB_CMD=$(OCAMLMKLIB) -custom -linkall
OCAMLDEP_CMD=$(OCAMLDEP) $(OCAMLFLAGS)


all: $(PYCAMLMARKER)
	@$(MAKE) all-build
all-build: $(PYCAMLMARKER) pycaml.cmi pycaml.cma

$(PYCAMLDIR)/pycaml.mli: $(PYCAMLDIR)/pycaml.ml
	$(OCAMLC_CMD) -i $< > $(PYCAMLDIR)/pycaml.mli

$(PYCAMLDIR)/pycaml.cmo: $(PYCAMLDIR)/pycaml.ml $(PYCAMLDIR)/pycaml.cmi
	$(OCAMLC_CMD) -c $< -o $@

$(PYCAMLDIR)/pycaml.cmi: $(PYCAMLDIR)/pycaml.mli
	$(OCAMLC_CMD) -c $< -o $@

# the cmi file apparently needs to be present next to the cma file
pycaml.cmi: $(PYCAMLDIR)/pycaml.cmi
	cp -f $< $@

pycaml_stubs.o: $(PYCAMLDIR)/pycaml_stubs.c
	$(OCAMLC_CMD) $< -o $@

libpycaml_stubs.a: pycaml_stubs.o
	$(OCAMLMKLIB_CMD) -oc pycaml_stubs $<

pycaml.cma: $(PYCAMLDIR)/pycaml.cmo libpycaml_stubs.a $(PYCAMLDIR)/pycaml.cmi
	$(OCAMLC_CMD) -I $(PYCAMLDIR) -a -o pycaml.cma $<

all.opt: $(PYCAMLMARKER)
	@$(MAKE) all-opt-build
all-opt-build: $(PYCAMLMARKER) pycaml.cmi pycaml.cmxa

$(PYCAMLDIR)/pycaml.cmx: $(PYCAMLDIR)/pycaml.ml $(PYCAMLDIR)/pycaml.cmi
	$(OCAMLOPT_CMD) -c $<

pycaml.cmxa: $(PYCAMLDIR)/pycaml.cmx libpycaml_stubs.a
	$(OCAMLOPT_CMD) -I $(PYCAMLDIR) -a -o pycaml.cmxa $<

clean:
	rm -f $(PYCAMLDIR)/pycaml.mli pycaml.cm[aixo] $(PYCAMLDIR)/pycaml.cm[aixo] \
		pycaml.[ao] libpycaml_stubs.a pycaml_stubs.o dllpycaml_stubs.so \
		pycaml_ml.o pycaml.cmxa

distclean: clean
	rm -rf $(PYCAMLDIR)

.PHONEY: all all.opt all-build all-opt-build distclean clean depend

# construct the directories
depend: $(PYCAMLMARKER)

$(PYCAMLMARKER): $(PYCAMLFILE)
	$(TAR) xfvz $<
	$(PATCH) -d "${PYCAMLDIR}" -p1 < ./python3-compat-fix.patch
	touch $@

$(PYCAMLFILE):
	@echo "$@ not found. Please download it and drop it in this directory ($(pwd))."
	@false
