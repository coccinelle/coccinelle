ACLOCAL_AMFLAGS = -I m4

mypkgdir=$(pkglibdir)

EXTRA_DIST=stdcompat.opam dune stdcompat.ml stdcompat__native.ml_byte \
	stdcompat__native.ml_native stdcompat_tests.ml stdcompat__native.mli

mypkg_SCRIPTS = META stdcompat.cmi \
	stdcompat__native.cmi stdcompat__init.cmi stdcompat__root.cmi \
	stdcompat__seq.cmi stdcompat__tools.cmi \
	stdcompat__pervasives.cmi stdcompat__arg.cmi stdcompat__lazy.cmi \
	stdcompat__char.cmi stdcompat__uchar.cmi stdcompat__buffer.cmi \
	stdcompat__string.cmi stdcompat__stringLabels.cmi stdcompat__bytes.cmi \
	stdcompat__bytesLabels.cmi stdcompat__list.cmi \
	stdcompat__listLabels.cmi \
	stdcompat__stack.cmi stdcompat__hashtbl_ext.cmi \
	stdcompat__hashtbl.cmi stdcompat__set.cmi \
	stdcompat__map.cmi stdcompat__weak.cmi stdcompat__sys.cmi \
	stdcompat__stream.cmi stdcompat__digest.cmi stdcompat__nativeint.cmi \
	stdcompat__int64.cmi stdcompat__int32.cmi stdcompat__filename.cmi \
	stdcompat__array.cmi stdcompat__arrayLabels.cmi \
	stdcompat__float.cmi stdcompat__queue.cmi stdcompat__ephemeron.cmi \
	stdcompat__spacetime.cmi stdcompat__moreLabels.cmi

if OCAML_SUPPORTS_BYTECODE
mypkg_SCRIPTS += stdcompat.cma
endif

if OCAML_SUPPORTS_NATIVE
mypkg_SCRIPTS += stdcompat.cmxa stdcompat.a stdcompat.cmx \
	stdcompat__native.cmx stdcompat__init.cmx stdcompat__root.cmx \
	stdcompat__seq.cmx stdcompat__tools.cmx \
	stdcompat__pervasives.cmx stdcompat__arg.cmx stdcompat__lazy.cmx \
	stdcompat__char.cmx stdcompat__uchar.cmx stdcompat__buffer.cmx \
	stdcompat__string.cmx stdcompat__stringLabels.cmx stdcompat__bytes.cmx \
	stdcompat__bytesLabels.cmx stdcompat__list.cmx \
	stdcompat__listLabels.cmx \
	stdcompat__stack.cmx stdcompat__hashtbl_ext.cmx \
	stdcompat__hashtbl.cmx stdcompat__set.cmx \
	stdcompat__map.cmx stdcompat__weak.cmx stdcompat__sys.cmx \
	stdcompat__stream.cmx stdcompat__digest.cmx stdcompat__nativeint.cmx \
	stdcompat__int64.cmx stdcompat__int32.cmx stdcompat__filename.cmx \
	stdcompat__array.cmx stdcompat__arrayLabels.cmx \
	stdcompat__float.cmx stdcompat__queue.cmx stdcompat__ephemeron.cmx \
	stdcompat__spacetime.cmx stdcompat__moreLabels.cmx
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
	stdcompat__spacetime.ml stdcompat__moreLabels.ml

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
	cp stdcompat__native.ml_byte stdcompat__native.ml
	$(OCAMLC) $(OCAMLCFLAGS) -c stdcompat__native.ml

stdcompat__native.cmx : stdcompat__native.ml_native stdcompat__native.cmo
	cp stdcompat__native.ml_native stdcompat__native.ml
	$(OCAMLOPT) $(OCAMLCFLAGS) -c stdcompat__native.ml

.PHONY : depend
depend : .depend

.depend : $(MODULES) $(MODULES:.ml=.mli) stdcompat.ml
	cp stdcompat__native.ml_byte stdcompat__native.ml
	$(OCAMLDEP) stdcompat__native.ml $^ >$@ || rm -f .depend

.PHONY : tests
tests : stdcompat_tests
	./stdcompat_tests

stdcompat_tests: stdcompat.$(CMAX)
	$(OCAMLBEST) stdcompat.$(CMAX) stdcompat_tests.ml -o $@

@INCLUDE_DEPEND@

CLEANFILES = *.cmi *.cmo *.cmx *.o *.a stdcompat.cma stdcompat.cmxa \
	stdcompat.cmxs stdcompat_tests $(bin_SCRIPTS) *~
