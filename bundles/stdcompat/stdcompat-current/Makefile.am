ACLOCAL_AMFLAGS = -I m4

mypkgdir=$(pkglibdir)

EXTRA_DIST=stdcompat.opam dune stdcompat.ml stdcompat__native.ml_byte \
	stdcompat__native.ml_native stdcompat_tests.ml stdcompat__native.mli \
	stdcompat__pervasives.mli stdcompat__arg.mli stdcompat__lazy.mli \
	stdcompat__char.mli stdcompat__uchar.mli stdcompat__sys.mli stdcompat__buffer.mli \
	stdcompat__string.mli stdcompat__stringLabels.mli stdcompat__bytes.mli \
	stdcompat__bytesLabels.mli stdcompat__list.mli stdcompat__listLabels.mli \
	stdcompat__stack.mli \
	stdcompat__hashtbl.mli stdcompat__set.mli \
	stdcompat__map.mli stdcompat__weak.mli \
	stdcompat__stream.mli stdcompat__digest.mli stdcompat__nativeint.mli \
	stdcompat__int.mli \
	stdcompat__int64.mli stdcompat__int32.mli stdcompat__filename.mli \
	stdcompat__array.mli stdcompat__arrayLabels.mli \
	stdcompat__float.mli stdcompat__queue.mli stdcompat__ephemeron.mli \
	stdcompat__spacetime.mli stdcompat__moreLabels.mli stdcompat__lexing.mli \
	stdcompat__option.mli stdcompat__printexc.mli stdcompat__result.mli stdcompat__bool.mli \
	stdcompat__fun.mli stdcompat__format.mli stdcompat__printf.mli stdcompat__stdlib.mli \
	stdcompat__seq.mli stdcompat__printexc.mli

MODULES = stdcompat__init.ml stdcompat__root.ml stdcompat__seq_s.ml \
	stdcompat__seq.ml stdcompat__tools.ml \
	stdcompat__pervasives_s.ml stdcompat__arg_s.ml stdcompat__lazy_s.ml \
	stdcompat__char_s.ml stdcompat__uchar_s.ml stdcompat__buffer_s.ml \
	stdcompat__string_s.ml stdcompat__stringLabels_s.ml stdcompat__bytes_s.ml \
	stdcompat__bytesLabels_s.ml stdcompat__list_s.ml stdcompat__listLabels_s.ml \
	stdcompat__stack_s.ml \
	stdcompat__hashtbl_s.ml stdcompat__set_s.ml \
	stdcompat__map_s.ml stdcompat__weak_s.ml stdcompat__sys_s.ml \
	stdcompat__stream_s.ml stdcompat__digest_s.ml stdcompat__nativeint_s.ml \
	stdcompat__int_s.ml \
	stdcompat__int64_s.ml stdcompat__int32_s.ml stdcompat__filename_s.ml \
	stdcompat__array_s.ml stdcompat__arrayLabels_s.ml \
	stdcompat__float_s.ml stdcompat__queue_s.ml stdcompat__ephemeron_s.ml \
	stdcompat__spacetime_s.ml stdcompat__moreLabels_s.ml stdcompat__lexing_s.ml \
	stdcompat__option_s.ml stdcompat__result_s.ml stdcompat__bool_s.ml \
	stdcompat__fun_s.ml stdcompat__stdlib_s.ml stdcompat__printf_s.ml \
	stdcompat__printexc_s.ml \
	stdcompat__pervasives.ml stdcompat__arg.ml stdcompat__lazy.ml \
	stdcompat__char.ml stdcompat__uchar.ml stdcompat__sys.ml stdcompat__buffer.ml \
	stdcompat__string.ml stdcompat__stringLabels.ml stdcompat__bytes.ml \
	stdcompat__bytesLabels.ml stdcompat__list.ml stdcompat__listLabels.ml \
	stdcompat__stack.ml stdcompat__hashtbl_ext.ml \
	stdcompat__hashtbl.ml stdcompat__set.ml \
	stdcompat__map.ml stdcompat__weak.ml \
	stdcompat__stream.ml stdcompat__digest.ml stdcompat__nativeint.ml \
	stdcompat__int.ml \
	stdcompat__int64.ml stdcompat__int32.ml stdcompat__filename.ml \
	stdcompat__array.ml stdcompat__arrayLabels.ml \
	stdcompat__float.ml stdcompat__queue.ml stdcompat__ephemeron.ml \
	stdcompat__spacetime.ml stdcompat__moreLabels.ml stdcompat__lexing.ml \
	stdcompat__option.ml stdcompat__printexc.ml stdcompat__result.ml stdcompat__bool.ml \
	stdcompat__fun.ml stdcompat__format.ml stdcompat__printf.ml stdcompat__stdlib.ml \
	stdcompat.ml

MODULES_native = stdcompat__native.ml $(MODULES)

mypkg_SCRIPTS = META $(MODULES_native) $(MODULES_native:.ml=.cmi) \
	libstdcompat__stubs.a

if OCAML_SUPPORTS_BYTECODE
mypkg_SCRIPTS += stdcompat.cma
endif

if OCAML_SUPPORTS_NATIVE
mypkg_SCRIPTS += stdcompat.cmxa stdcompat.a $(MODULES_native:.ml=.cmx)
endif

if OCAML_SUPPORTS_SHARED
mypkg_SCRIPTS += stdcompat.cmxs
endif

PACKAGES = $(RESULT_PKG) $(SEQ_PKG) $(UCHAR_PKG)
OCAMLFLAGS = $(PACKAGES:%=-package %)
OCAMLCFLAGS = $(OCAMLFLAGS) -I @srcdir@
OCAMLLDFLAGS = $(OCAMLFLAGS)
OCAMLLIBFLAGS = -cclib "-L. -lstdcompat__stubs"

if OCAML_SUPPORTS_BIN_ANNOT
OCAMLFLAGS += -bin-annot
mypkg_SCRIPTS += $(MODULES_native:.ml=.cmt)
endif

if OCAML_SUPPORTS_NO_ALIAS_DEPS
OCAMLFLAGS += -no-alias-deps
endif

SUFFIXES = .ml .mli .cmi .cmo .cmx .cmxa .cmxs .cmt

.DEFAULT_GOAL = all

.mli.cmi :
	$(OCAMLBEST) -c $(OCAMLCFLAGS) $< -o $@

.ml.cmo :
	$(OCAMLC) -c $(OCAMLCFLAGS) $< -o $@

.ml.cmx :
	$(OCAMLOPT) -c $(OCAMLCFLAGS) $< -o $@

stdcompat__stubs.o : stdcompat__stubs.c
	$(OCAMLC) -c -o $@ $<

libstdcompat__stubs.a : stdcompat__stubs.o
	$(OCAMLMKLIB) -o $(basename $<) $<

stdcompat.cma : $(MODULES_native:.ml=.cmo) libstdcompat__stubs.a
	$(OCAMLC) $(OCAMLLDFLAGS) -a $(MODULES_native:.ml=.cmo) $(OCAMLLIBFLAGS) -o $@

stdcompat.cmxa : $(MODULES_native:.ml=.cmx) libstdcompat__stubs.a
	$(OCAMLOPT) $(OCAMLLDFLAGS) -a $(MODULES_native:.ml=.cmx) $(OCAMLLIBFLAGS) -o $@

stdcompat.a : stdcompat.cmxa

stdcompat.cmxs : $(MODULES_native:.ml=.cmx) libstdcompat__stubs.a
	$(OCAMLOPT) $(OCAMLLDFLAGS) -shared $(MODULES_native:.ml=.cmx) $(OCAMLLIBFLAGS) -o $@

stdcompat__native.cmo : stdcompat__native.ml_byte
	$(OCAMLC) $(OCAMLCFLAGS) -c -impl stdcompat__native.ml_byte

stdcompat__native.cmx : stdcompat__native.ml_native
	$(OCAMLOPT) $(OCAMLCFLAGS) -c -impl stdcompat__native.ml_native

.PHONY : depend
depend : .depend

.depend : $(MODULES) $(MODULES_native:.ml=.mli) stdcompat.ml stdcompat__native.ml_byte
	cp stdcompat__native.ml_byte stdcompat__native.ml
	$(OCAMLDEP) $^ >$@ || rm -f .depend

.PHONY : tests
tests : stdcompat_tests
	./stdcompat_tests

stdcompat_tests: stdcompat.$(CMAX) stdcompat_tests.ml
	$(OCAMLBEST) stdcompat.$(CMAX) stdcompat_tests.ml -o $@

@INCLUDE_DEPEND@

CLEANFILES = *.cmi *.cmo *.cmx *.cmt *.cmti *.o *.a stdcompat.cma \
	stdcompat.cmxa stdcompat__native.ml \
	stdcompat.cmxs stdcompat_tests $(bin_SCRIPTS) .depend
