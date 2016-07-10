OCAMLFIND=ocamlfind

HAVE_OCAMLFIND:=$(shell \
	if $(OCAMLFIND) query -help >/dev/null 2>&1; then \
		echo yes; \
	else \
		echo no; \
	fi \
)

ifneq ($(HAVE_OCAMLFIND),no)
	OCAMLC=$(OCAMLFIND) ocamlc
	ifneq ($(HAVE_OCAMLOPT),no)
		OCAMLOPTEXE=$(OCAMLFIND) ocamlopt
	endif
	OCAMLMKLIB=$(OCAMLFIND) ocamlmklib
	OCAMLDEP=$(OCAMLFIND) ocamldep
	OCAMLDOC=$(OCAMLFIND) ocamldoc
else
	OCAMLC:=$(shell \
		if ocamlc.opt -version >/dev/null 2>&1; then \
			echo ocamlc.opt; \
		elif ocamlc -version >/dev/null 2>&1; then \
			echo ocamlc; \
		fi \
	)
	ifeq ($(OCAMLC),)
		$(error There is no OCaml compiler available in path)
	endif
	ifneq ($(HAVE_OCAMLOPT),no)
		OCAMLOPTEXE:=$(shell \
			if ocamlopt.opt -version >/dev/null 2>&1; then \
				echo ocamlopt.opt; \
			elif ocamlopt -version >/dev/null 2>&1; then \
				echo ocamlopt; \
			fi \
		)
	endif
	OCAMLMKLIB=ocamlmklib
	OCAMLDEP=ocamldep
	OCAMLDOC=ocamldoc
endif

ifeq ($(OCAMLOPTEXE),)
	OCAMLOPT=$(error There is no optimizing OCaml compiler available)
	OCAMLCOPT=$(OCAMLC)
	CMOX=cmo
	CMAX=cma
	ALLOPT=
	TESTSOPT=
else
	OCAMLOPT=$(OCAMLOPTEXE)
	OCAMLCOPT=$(OCAMLOPT)
	CMOX=cmx
	CMAX=cmxa
	ALLOPT=all.native
	TESTSOPT=tests.native
endif

MODULES=pyml_compat pytypes pywrappers py pycaml

VERSION:=$(shell date "+%Y%m%d")

OCAMLLIBFLAGS=-cclib "-L. -lpyml_stubs"

OCAMLLIBFLAGSNATIVE=$(OCAMLLIBFLAGS)
OCAMLLIBFLAGSBYTECODE=-custom $(OCAMLLIBFLAGS)

OCAMLVERSION:=$(shell $(OCAMLC) -version)

PYML_COMPAT:=$(shell \
	if [ "$(OCAMLVERSION)" "<" 4.00.0 ]; then \
		echo pyml_compat312.ml; \
	elif [ "$(OCAMLVERSION)" "<" 4.03.0 ]; then \
		echo pyml_compat400.ml; \
	else \
		echo pyml_compat403.ml; \
	fi \
)

.PHONY: all
all: all.bytecode $(ALLOPT)
	@echo The py.ml library is compiled.
	@echo Run \`make doc\' to build the documentation.
	@echo Run \`make tests\' to check the test suite.
ifneq ($(HAVE_OCAMLFIND),no)
	@echo Run \`make install\' to install the library via ocamlfind.;
endif

.PHONY: help
help:
	@echo make [all] : build the library
	@echo make all.bytecode : build only the bytecode library
	@echo make all.native : build only the native library
	@echo make doc : build the documentation
	@echo make install : install the library via ocamlfind
	@echo make clean : remove all the generated files
	@echo make tests : compile and run the test suite
	@echo make tests.bytecode : run only the bytecode version of the tests
	@echo make tests.native : run only the native version of the tests
	@echo make HAVE_OCAMLFIND=no : disable ocamlfind
	@echo make HAVE_OCAMLOPT=no : disable ocamlopt
	@echo "make OCAMLC|OCAMLOPT|OCAMLMKLIB|OCAMLDEP|OCAMLDOC=... :"
	@echo "  set paths to OCaml tools"
	@echo make OCAMLCFLAGS=... : set flags to OCaml compiler for compiling
	@echo make OCAMLLDFLAGS=... : set flags to OCaml compiler for linking
	@echo make OCAMLLIBFLAGS=... :
	@echo "  set flags to OCaml compiler for building the library"

.PHONY: all.bytecode
all.bytecode: pyml.cma

.PHONY: all.native
all.native: pyml.cmxa pyml.cmxs

.PHONY: tests
tests: tests.bytecode $(TESTSOPT)

.PHONY: tests.bytecode
tests.bytecode: pyml_tests.bytecode
	./pyml_tests.bytecode

.PHONY: tests.native
tests.native: pyml_tests.native
	./pyml_tests.native

.PHONY: install
install:
ifeq ($(HAVE_OCAMLFIND),no)
	$(error ocamlfind is needed for 'make install')
endif
	$(OCAMLFIND) install pyml \
		py.mli \
		py.cmi pytypes.cmi pywrappers.cmi pycaml.cmi \
		py.cmx pytypes.cmx pywrappers.cmx pycaml.cmx \
		pyml.cma pyml.cmxa pyml.cmxs pyml.a \
		libpyml_stubs.a dllpyml_stubs.so \
		META

.PHONY: clean
clean:
	for module in $(MODULES) generate pyml_tests; do \
		rm -f $$module.cmi $$module.cmo $$module.cmx $$module.a \
			$$module.o; \
	done
	rm -f pyml.cma pyml.cmxa pyml.cmxs pyml.a
	rm -f pywrappers.mli pywrappers.ml pyml_dlsyms.inc pyml_wrappers.inc
	rm -f pyml.h
	rm -f pyml_stubs.o dllpyml_stubs.so libpyml_stubs.a
	rm -f pyml_compat.ml
	rm -f generate pyml_tests pyml_tests.bytecode
	rm -f .depend
	rm -rf doc

.PHONY: tarball
tarball:
	git archive --format=tar.gz --prefix=pyml-$(VERSION)/ HEAD \
		>pyml-$(VERSION).tar.gz

doc: py.mli pycaml.mli pywrappers.ml
	mkdir -p $@
	$(OCAMLDOC) -html -d $@ $^
	touch $@

ifneq ($(MAKECMDGOALS),clean)
-include .depend
endif

.depend: $(MODULES:=.ml) $(MODULES:=.mli) pyml_tests.ml
	$(OCAMLDEP) $^ >$@

generate: pyml_compat.$(CMOX) generate.$(CMOX)
	$(OCAMLCOPT) $^ -o $@

generate.cmo: generate.ml pyml_compat.cmo

generate.cmx: generate.ml pyml_compat.cmx

pyml_compat.cmo pyml_compat.cmx: pyml_compat.cmi

pywrappers.ml pyml_wrappers.inc: generate
	./generate

pyml_wrappers.inc: pywrappers.ml

pywrappers.mli: pywrappers.ml pytypes.cmi
	$(OCAMLC) -i $< >$@

pyml_tests.native: py.cmi pyml.cmxa pyml_tests.cmx
	$(OCAMLOPT) $(OCAMLLDFLAGS) unix.cmxa pyml.cmxa \
		pyml_tests.cmx -o $@

pyml_tests.bytecode: py.cmi pyml.cma pyml_tests.cmo
	$(OCAMLC) $(OCAMLLDFLAGS) unix.cma pyml.cma pyml_tests.cmo -o $@

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
