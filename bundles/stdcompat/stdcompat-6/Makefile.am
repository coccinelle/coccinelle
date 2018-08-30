ACLOCAL_AMFLAGS = -I m4

mypkgdir=$(pkglibdir)

EXTRA_DIST=stdcompat.opam dune stdcompat.ml stdcompat__native.ml_byte \
	stdcompat__native.ml_native stdcompat_tests.ml stdcompat__native.mli

MODULES = stdcompat__init.ml stdcompat__root.ml stdcompat__seq.ml \
	stdcompat__tools.ml \
	stdcompat__pervasives.ml stdcompat__arg.ml stdcompat__lazy.ml \
	stdcompat__char.ml stdcompat__uchar.ml stdcompat__buffer.ml \
	stdcompat__string.ml stdcompat__stringLabels.ml stdcompat__bytes.ml \
	stdcompat__bytesLabels.ml stdcompat__list.ml stdcompat__listLabels.ml \
	stdcompat__stack.ml stdcompat__hashtbl_ext.ml \
	stdcompat__hashtbl.ml stdcompat__set.ml \
	stdcompat__map.ml stdcompat__weak.ml stdcompat__sys.ml \
	stdcompat__stream.ml stdcompat__digest.ml stdcompat__nativeint.ml \
	stdcompat__int64.ml stdcompat__int32.ml stdcompat__filename.ml \
	stdcompat__array.ml stdcompat__arrayLabels.ml \
	stdcompat__float.ml stdcompat__queue.ml stdcompat__ephemeron.ml \
	stdcompat__spacetime.ml stdcompat__moreLabels.ml stdcompat__lexing.ml \
	stdcompat__option.ml stdcompat__result.ml

mypkg_SCRIPTS = META stdcompat.cmi stdcompat__native.cmi $(MODULES:.ml=.cmi)

if OCAML_SUPPORTS_BYTECODE
mypkg_SCRIPTS += stdcompat.cma
endif

if OCAML_SUPPORTS_NATIVE
mypkg_SCRIPTS += stdcompat.cmxa stdcompat.a stdcompat.cmx \
	stdcompat__native.cmx $(MODULES:.ml=.cmx)
endif

if OCAML_SUPPORTS_SHARED
mypkg_SCRIPTS += stdcompat.cmxs
endif

PACKAGES = $(RESULT_PKG) $(SEQ_PKG) $(UCHAR_PKG)
OCAMLFLAGS = $(PACKAGES:%=-package %)
OCAMLCFLAGS = $(OCAMLFLAGS)
OCAMLLDFLAGS = $(OCAMLFLAGS)

if OCAML_SUPPORTS_NO_ALIAS_DEPS
OCAMLFLAGS += -no-alias-deps
endif

SUFFIXES = .ml .mli .cmi .cmo .cmx .cmxa .cmxs

.DEFAULT_GOAL = all

.mli.cmi :
	$(OCAMLBEST) -c $(OCAMLFLAGS) $< -o $@

.ml.cmo :
	$(OCAMLC) -c $(OCAMLFLAGS) $< -o $@

.ml.cmx :
	$(OCAMLOPT) -c $(OCAMLFLAGS) $< -o $@

stdcompat.cmi : stdcompat.cmo

stdcompat.cma : stdcompat__native.cmo $(MODULES:.ml=.cmo) stdcompat.cmo
	$(OCAMLC) $(OCAMLLDFLAGS) -a $^ -o $@

stdcompat.cmxa : stdcompat__native.cmx $(MODULES:.ml=.cmx) stdcompat.cmx
	$(OCAMLOPT) $(OCAMLLDFLAGS) -a $^ -o $@

stdcompat.a : stdcompat.cmxa

stdcompat.cmxs : stdcompat__native.cmx $(MODULES:.ml=.cmx) stdcompat.cmx
	$(OCAMLOPT) $(OCAMLLDFLAGS) -shared $^ -o $@

stdcompat__native.cmo : stdcompat__native.ml_byte
	$(OCAMLC) $(OCAMLCFLAGS) -c -impl stdcompat__native.ml_byte

stdcompat__native.cmx : stdcompat__native.ml_native
	$(OCAMLOPT) $(OCAMLCFLAGS) -c -impl stdcompat__native.ml_native

.PHONY : depend
depend : .depend

.depend : $(MODULES) $(MODULES:.ml=.mli) stdcompat.ml stdcompat__native.ml_byte
	cp stdcompat__native.ml_byte stdcompat__native.ml
	$(OCAMLDEP) stdcompat__native.ml $^ >$@ || rm -f .depend

.PHONY : tests
tests : stdcompat_tests
	./stdcompat_tests

stdcompat_tests: stdcompat.$(CMAX)
	$(OCAMLBEST) stdcompat.$(CMAX) stdcompat_tests.ml -o $@

@INCLUDE_DEPEND@

CLEANFILES = *.cmi *.cmo *.cmx *.o *.a stdcompat.cma stdcompat.cmxa \
	stdcompat__native.ml \
	stdcompat.cmxs stdcompat_tests $(bin_SCRIPTS) *~
