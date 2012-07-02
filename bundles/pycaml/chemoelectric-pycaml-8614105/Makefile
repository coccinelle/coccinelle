OCAMLMAKEFILE = /usr/include/OCamlMakefile

PYTHON_INCLUDE_PATH = "/usr/include/python${PYVER}"
PYTHON_LIBRARY_PATH = "/usr/$(get_libdir)/python${PYVER}"

CLIBS   = $(PYCAML_CLIBS) "pthread" "dl" $(UTIL_CLIBS) "m" "c"
PACKS   = unix str
SOURCES = pycaml.ml pycaml.mli pycaml_stubs.c
RESULT  = pycaml${PYVER_PACK}
THREADS = yes
LDFLAGS = -lpython${PYVER}
CFLAGS  = -g -O2 -fPIC -Wall
OCAMLFLAGS = -pp "camlp4o -parser Camlp4MacroParser -D PYMAJOR`echo ${PYVER} | sed -e 's/\\..*//'`"
OCAMLDEP = ocamldep ${OCAMLFLAGS}

INCDIRS=$(PYTHON_INCLUDE_PATH)
LIBDIRS=$(PYTHON_LIBRARY_PATH)

all: pycaml_stubs.h native-code-library byte-code-library META

META: META.in
	sed -e "s/@PYVER_PACK@/${PYVER_PACK}/g" < META.in > META

mrproper: clean
	rm -rf *~ *.cmi *.cmo *.a *.cma *.cmxa doc *.so deps

deps: META.in pycaml.ml pycaml.mli pycaml_stubs.c pycaml_stubs.h
	touch deps

.PHONY: mrproper

include $(OCAMLMAKEFILE)
