include ../../Makefile.config

OCAMLC_CMD=$(OCAMLC) -thread -ccopt "${PYTHON_CFLAGS}" -cclib "${PYTHON_LIBS}"
OCAMLOPT_CMD=$(OCAMLOPT) -thread -ccopt "${PYTHON_CFLAGS}" -cclib "${PYTHON_LIBS}"
OCAMLMKLIB_CMD=$(OCAMLMKLIB)

all: pycaml.cma

pycaml.mli: pycaml.ml
	$(OCAMLC_CMD) -i $< > pycaml.mli

pycaml.cmo: pycaml.ml pycaml.cmi
	$(OCAMLC_CMD) -c $<

pycaml.cmi: pycaml.mli
	$(OCAMLC_CMD) -c $<

pycaml_ml.o: pycaml_ml.c
	$(OCAMLC_CMD) $<

dllpycaml_stubs.so: pycaml_ml.o
	$(OCAMLMKLIB_CMD) -o pycaml_stubs $<

pycaml.cma: dllpycaml_stubs.so pycaml.cmi pycaml.cmo
	$(OCAMLC_CMD) -a -custom -linkall -o pycaml.cma pycaml.cmo -dllib -lpycaml_stubs

all.opt: pycaml.cmxa

pycaml.cmx: pycaml.ml pycaml.cmi
	$(OCAMLOPT_CMD) -c $<

pycaml.cmxa: dllpycaml_stubs.so pycaml.cmi pycaml.cmx
	$(OCAMLOPT_CMD) -a -o pycaml.cmxa pycaml.cmx -cclib -lpycaml_stubs

clean:
	rm -f pycaml.mli pycaml.cmi pycaml.cmo pycaml pycaml_ml.o \
		pycaml.cmx pycaml.cmxa pycaml.cma \
		libpycaml_stubs.a dllpycaml_stubs.o

distclean: clean

depend:
