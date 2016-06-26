OCAMLFIND=ocamlfind

HAVE_OCAMLFIND=$(shell \
	if $(OCAMLFIND) query -help >/dev/null 2>&1; then \
		echo yes; \
	else \
		echo no; \
	fi \
)

OCAMLC=$(shell \
	if [ "$(HAVE_OCAMLFIND)" = yes ]; then \
		echo $(OCAMLFIND) ocamlc; \
	elif ocamlc.opt -version >/dev/null 2>&1; then \
		echo ocamlc.opt; \
	else \
		echo ocamlc; \
	fi \
)
OCAMLOPT=$(shell \
	if [ "$(HAVE_OCAMLFIND)" = yes ]; then \
		echo $(OCAMLFIND) ocamlopt; \
	elif ocamlopt.opt -version >/dev/null 2>&1; then \
		echo ocamlopt.opt; \
	else \
		echo ocamlopt; \
	fi \
)
OCAMLMKLIB=$(shell \
	if [ "$(HAVE_OCAMLFIND)" = yes ]; then \
		echo $(OCAMLFIND) ocamlmklib; \
	else \
		echo ocamlmklib; \
	fi \
)
OCAMLDEP=$(shell \
	if [ "$(HAVE_OCAMLFIND)" = yes ]; then \
		echo $(OCAMLFIND) ocamldep; \
	else \
		echo ocamldep; \
	fi \
)
OCAMLDOC=$(shell \
	if [ "$(HAVE_OCAMLFIND)" = yes ]; then \
		echo $(OCAMLFIND) ocamldoc; \
	else \
		echo ocamldoc; \
	fi \
)

MODULES=pyml_compat pytypes pywrappers py pycaml_compat
GENERATED=pyml_dlsyms.inc pyml_wrappers.inc pyml.h

VERSION=$(shell date "+%Y%m%d")

OCAMLLIBFLAGS=-cclib "-L. -lpyml_stubs"

OCAMLLIBFLAGSNATIVE=$(OCAMLLIBFLAGS)
OCAMLLIBFLAGSBYTECODE=-custom $(OCAMLLIBFLAGS)

OCAMLVERSION=$(shell $(OCAMLC) -version)

PYML_COMPAT=$(shell \
	if [ "$(OCAMLVERSION)" "<" 4.00.0 ]; then \
		echo pyml_compat312.ml; \
	elif [ "$(OCAMLVERSION)" "<" 4.03.0 ]; then \
		echo pyml_compat400.ml; \
	else \
		echo pyml_compat403.ml; \
	fi \
)

all: py.cmi pycaml_compat.cmi pyml.cma pyml.cmxa pyml.cmxs doc

.PHONY: all tests tests.bytecode clean install

tests: pyml_tests
	./pyml_tests

tests.bytecode: pyml_tests.bytecode
	./pyml_tests.bytecode

doc: py.mli pycaml_compat.mli pywrappers.ml
	mkdir -p $@
	$(OCAMLDOC) -html -d $@ $^
	touch $@

install:
	$(OCAMLFIND) install pyml \
		py.mli \
		py.cmi pytypes.cmi pywrappers.cmi pycaml_compat.cmi \
		py.cmx pytypes.cmx pywrappers.cmx pycaml_compat.cmx \
		pyml.cma pyml.cmxa pyml.cmxs pyml.a \
		libpyml_stubs.a dllpyml_stubs.so \
		META

ifneq ($(MAKECMDGOALS),clean)
-include .depend
endif

.depend: $(MODULES:=.ml) $(MODULES:=.mli) pyml_tests.ml
	$(OCAMLDEP) $^ >$@

clean:
	rm -f py.cmi py.cmx py.cmo py.a py.o
	rm -f pyml.cma pyml.cmxa
	rm -f pytypes.cmi pytypes.cmo pytypes.cmx pytypes.o
	rm -f pywrappers.mli pywrappers.ml pywrappers.cmi pywrappers.cmo
	rm -f pywrappers.cmx pywrappers.o
	rm -f pyml_stubs.o
	rm -f generate.cmi generate.cmx
	rm -f pyml_compat.ml pyml_compat.cmi pyml_compat.cmo pyml_compat.cmx
	rm -f pyml_compat.o
	rm -f pycaml_compat.cmi pycaml_compat.cmx pycaml_compat.cmo
	rm -f pycaml_compat.a pycaml_compat.o
	rm -f pyml_tests.cmi pyml_tests.cmx pyml_tests.cmo pyml_tests.a
	rm -f pyml_tests.o
	rm -f $(GENERATED)
	rm -f .depend
	rm -rf doc

tarball:
	mkdir pyml-$(VERSION)/
	cp Makefile pyml_compat312.ml pyml_compat400.ml pyml_compat403.ml \
		pyml_compat.mli \
		generate.ml py.ml py.mli pyml_stubs.c pytypes.ml pytypes.mli \
		pyml_tests.ml test.py \
		pycaml_compat.ml pycaml_compat.mli README LICENSE META \
		pyml-$(VERSION)/
	rm -f pyml-$(VERSION).tar.gz
	tar -czf pyml-$(VERSION).tar.gz pyml-$(VERSION)/
	rm -rf pyml-$(VERSION)/

generate: pyml_compat.cmx generate.cmx
	$(OCAMLOPT) $^ -o $@

generate.cmx: generate.ml pyml_compat.cmi pyml_compat.cmx

pywrappers.ml pyml_wrappers.inc: generate
	./generate

pyml_wrappers.inc: pywrappers.ml

pywrappers.mli: pywrappers.ml pytypes.cmi
	$(OCAMLC) -i $< >$@

pyml_tests: py.cmi pyml.cmxa pyml_tests.cmx
	$(OCAMLOPT) $(OCAMLLDFLAGS) unix.cmxa pyml.cmxa pyml_tests.cmx -o $@

pyml_tests.bytecode: py.cmi pyml.cma pyml_tests.cmo
	$(OCAMLC) unix.cma pyml.cma pyml_tests.cmo -o $@

pyml_compat.ml: $(PYML_COMPAT)
	cp $< $@

pyml_compat.cmx: pyml_compat.cmi

%.cmi: %.mli
	$(OCAMLC) $(OCAMLCFLAGS) -c $< -o $@

%.cmo: %.ml
	$(OCAMLC) $(OCAMLCFLAGS) -c $< -o $@

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLCFLAGS) -c $< -o $@

pyml_stubs.o: pyml_stubs.c pyml_wrappers.inc
	$(OCAMLC) -c $< -o $@

pyml.cma: $(MODULES:=.cmo) libpyml_stubs.a
	$(OCAMLC) $(OCAMLLIBFLAGSBYTECODE) -a $(MODULES:=.cmo) -o $@

pyml.cmxa: $(MODULES:=.cmx) libpyml_stubs.a
	$(OCAMLOPT) $(OCAMLLIBFLAGSNATIVE) -a $(MODULES:=.cmx) -o $@

pyml.cmxs: $(MODULES:=.cmx) libpyml_stubs.a
	$(OCAMLOPT) $(OCAMLLIBFLAGSNATIVE) -shared $(MODULES:=.cmx) -o $@

libpyml_stubs.a: pyml_stubs.o
	$(OCAMLMKLIB) -o pyml_stubs $<
