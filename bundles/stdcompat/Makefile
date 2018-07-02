USE_MAGIC := true
OCAMLFIND := ocamlfind

OCAMLFIND_AVAILABLE := $(shell \
	if $(OCAMLFIND) query -help >/dev/null 2>&1; then \
		echo true; \
	else \
		echo false; \
	fi \
)

ifeq ($(OCAMLFIND_AVAILABLE),true)
	OCAMLOPT := $(OCAMLFIND) ocamlopt
	OCAMLC := $(OCAMLFIND) ocamlc
	OCAMLDOC := $(OCAMLFIND) ocamldoc
	RESULT_PKG_AVAILABLE := $(shell \
	if $(OCAMLFIND) query result >/dev/null 2>&1; then \
		echo true; \
	else \
		echo false; \
	fi \
)
	SEQ_PKG_AVAILABLE := $(shell \
	if $(OCAMLFIND) query seq >/dev/null 2>&1; then \
		echo true; \
	else \
		echo false; \
	fi \
)
else
	OCAMLOPT := $(shell \
		if ocamlopt.opt -version >/dev/null 2>&1; then \
			echo ocamlopt.opt; \
		elif ocamlopt -version >/dev/null 2>&1; then \
			echo ocamlopt; \
		fi \
	) $(OCAMLFLAGS)

	OCAMLC := $(shell \
		if ocamlc.opt -version >/dev/null 2>&1; then \
			echo ocamlc.opt; \
		elif ocamlc -version >/dev/null 2>&1; then \
			echo ocamlc; \
		fi \
	) $(OCAMLFLAGS)

	OCAMLDOC := ocamldoc $(OCAMLFLAGS)

	RESULT_PKG_AVAILABLE := false
	SEQ_PKG_AVAILABLE := false
endif

ifeq ($(OCAMLC),)
$(error There is no OCaml compiler available in PATH)
endif

ifeq ($(OCAMLOPT),)
	OCAMLOPT_AVAILABLE := false
else
	OCAMLOPT_AVAILABLE := $(shell \
		if $(OCAMLOPT) -version >/dev/null 2>&1; then \
			echo true; \
		else \
			echo false; \
		fi \
	)
endif

OCAML_VERSION := $(shell $(OCAMLC) -version | cut -c 1-6)

ifeq (4.07.0,$(word 1,$(sort 4.07.0 $(OCAML_VERSION))))
	USE_SEQ_PKG := false
else
	USE_SEQ_PKG := $(SEQ_PKG_AVAILABLE)
endif

ifeq (4.03.0,$(word 1,$(sort 4.03.0 $(OCAML_VERSION))))
	USE_RESULT_PKG := false
else
	USE_RESULT_PKG := $(RESULT_PKG_AVAILABLE)
endif

ifeq ($(USE_SEQ_PKG),true)
	OCAMLFLAGS += -package seq
endif

ifeq ($(USE_RESULT_PKG),true)
	OCAMLFLAGS += -package result
endif

ifeq ($(OCAMLFIND_AVAILABLE),true)
	OCAMLFLAGS_TESTS += -linkpkg
endif

CPPO := cppo
CPP := cpp

CPPO_AVAILABLE := $(shell \
	if $(CPPO) -version >/dev/null 2>&1; then \
		echo true; \
	else \
		echo false; \
	fi \
)

LITTLE_ENDIAN := $(shell \
	echo -n I | hexdump -o | awk '{ print substr($$2,6,1); exit }' \
)

ifeq ($(CPPO_AVAILABLE),true)
	PP := $(CPPO) -V 'OCAML:$(OCAML_VERSION)'
	PP_NATIVE_ARGS := -D OCAMLNATIVE
	ifeq ($(LITTLE_ENDIAN),0)
		PP += -D BIGENDIAN
	endif
	ifeq ($(USE_SEQ_PKG),true)
		PP += -D 'HAS_SEQ_PKG'
		REQUIRES += seq
	endif
	ifeq ($(USE_RESULT_PKG),true)
		PP += -D 'HAS_RESULT_PKG'
		REQUIRES += result
	endif
	ifeq ($(USE_MAGIC),true)
		PP += -D 'USE_MAGIC'
	endif
else
	OCAML_VERSION_STRIPPED := $(subst .,,$(OCAML_VERSION))
	PP := sed -e '/^\#/s/(\(.\),\(..\),\(.\))/\1\2\3/' | \
		$(CPP) -DOCAML_VERSION=$(OCAML_VERSION_STRIPPED) -undef -w -
	PP_NATIVE_ARGS := -DOCAMLNATIVE
	ifeq ($(LITTLE_ENDIAN),0)
		PP += -DBIGENDIAN
	endif
	ifeq ($(USE_SEQ_PKG),true)
		PP += -DHAS_SEQ_PKG
		REQUIRES += seq
	endif
	ifeq ($(USE_RESULT_PKG),true)
		PP += -DHAS_RESULT_PKG
		REQUIRES += result
	endif
	ifeq ($(USE_MAGIC),true)
		PP += -DUSE_MAGIC
	endif
endif

PP_INTF := $(PP)
PP_BYTECODE := $(PP)
PP_NATIVE := $(PP) $(PP_NATIVE_ARGS)

ifeq ($(CPPO_AVAILABLE),true)
  PP_META += $(PP) -D 'REQUIRES "$(REQUIRES)"'
else
  PP_META += $(PP) -D 'REQUIRES="$(REQUIRES)"'
endif

.PHONY : all
all : bytecode $(patsubst %,native,$(filter true,$(OCAMLOPT_AVAILABLE))) doc

.PHONY : bytecode
bytecode : stdcompat.cma

.PHONY : native
native : stdcompat.cmxa stdcompat.cmxs

.PHONY : clean
clean :
	rm -f META stdcompat.ml_byte stdcompat.ml_native stdcompat.mli \
		stdcompat.cmi stdcompat.cmt stdcompat.cmti \
		stdcompat.cmo stdcompat.cmx stdcompat.o \
		stdcompat.cma stdcompat.cmxs stdcompat.cmxa stdcompat.a \
		stdcompat_tests.cmt stdcompat_tests.cmti \
		stdcompat_tests.cmi stdcompat_tests.cmo

.PHONY : install
install : META stdcompat.cma stdcompat.cmi \
	$(patsubst %,stdcompat.cmx stdcompat.cmxs stdcompat.cmxa stdcompat.a, \
		$(filter true,$(OCAMLOPT_AVAILABLE)))
ifeq ($(HAVE_OCAMLFIND),no)
	$(error ocamlfind is needed for 'make install')
endif
	$(OCAMLFIND) install stdcompat $^

.PHONY : uninstall
uninstall :
ifeq ($(HAVE_OCAMLFIND),no)
	$(error ocamlfind is needed for 'make uninstall')
endif
	$(OCAMLFIND) remove stdcompat

stdcompat.mli : stdcompat.mlip
	<$^ $(PP_INTF) >$@ || rm $@

stdcompat.cmo stdcompat.cmx : stdcompat.cmi

doc : stdcompat.mli
	mkdir -p doc
	$(OCAMLDOC) $(OCAMLFLAGS) -html -d $@ $^
	touch doc

%.cmi : %.mli
	$(OCAMLC) $(OCAMLFLAGS) -c $<

stdcompat.ml_byte : stdcompat.mlp
	<$< $(PP_BYTECODE) >$@ || rm $@

stdcompat.cmo : stdcompat.ml_byte
	$(OCAMLC) $(OCAMLFLAGS) -c -impl $<

stdcompat.ml_native : stdcompat.mlp
	<$< $(PP_NATIVE) >$@ || rm $@

stdcompat.cmx : stdcompat.ml_native
	$(OCAMLOPT) $(OCAMLFLAGS) -c -impl $<

stdcompat.cma : stdcompat.cmo
	$(OCAMLC) -a stdcompat.cmo -o $@

stdcompat.cmxa : stdcompat.cmx
	$(OCAMLOPT) -a stdcompat.cmx -o $@

stdcompat.cmxs : stdcompat.cmx
	$(OCAMLOPT) -shared stdcompat.cmx -o $@

.PHONY : tests_bytecode
tests_bytecode : tests.bytecode
	./tests.bytecode

tests.bytecode : stdcompat.cmo stdcompat_tests.cmo
	$(OCAMLC) $(OCAMLFLAGS) $(OCAMLFLAGS_TESTS) -o $@ $^

.PHONY : tests_native
tests_native : tests.native
	./tests.native

tests.native : stdcompat.cmx stdcompat_tests.cmx
	$(OCAMLOPT) $(OCAMLFLAGS) $(OCAMLFLAGS_TESTS) -o $@ $^

stdcompat_tests.cmo : stdcompat.cmi

stdcompat_tests.cmx : stdcompat.cmi

%.cmo : %.ml
	$(OCAMLC) $(OCAMLFLAGS) -c $<

%.cmx : %.ml
	$(OCAMLOPT) $(OCAMLFLAGS) -c $<

META : META.pp
	<$< $(PP_META) >$@ || rm $@
