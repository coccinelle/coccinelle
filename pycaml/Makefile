#pad: was called Makefile.deb-pycaml before

OCAMLMAKEFILE = OCamlMakefile
PY_PREFIX = $(shell python getprefix.py)
PY_VERSION = $(shell python getversion.py)
CLIBS = python$(PY_VERSION) pthread dl util m c

SOURCES = pycaml.ml pycaml_ml.c
RESULT = pycaml
THREADS = yes
NO_CUSTOM = NO_CUSTOM
CFLAGS = -fPIC

LIBDIRS = $(PY_PREFIX)/lib/python$(PY_VERSION)/config
INCDIRS = $(PY_PREFIX)/include/python$(PY_VERSION)
OCAMLLDFLAGS = -linkall

all.opt: native-code-library byte-code-library pycaml.customtop
	cp -f dllpycaml_stubs.so ../

all: byte-code-library pycaml.customtop
	cp -f dllpycaml_stubs.so ../

pycaml.customtop: pycaml.cma
	ocamlmktop -o pycaml.customtop pycaml.cma

clean:: 
	rm -f pycaml.customtop

depend: # added by julia, undoubtedly wrong

-include $(OCAMLMAKEFILE)
