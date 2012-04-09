export OCAMLMAKEFILE = ../OCamlMakefile

export INCDIRS = ../lib
export LIBDIRS = ../lib
export LIBS = sexplib
export PACKS = unix bigarray num

define PROJ_sub1
  SOURCES = conv_test.ml
  RESULT = conv_test
endef
export PROJ_sub1

define PROJ_sub2
  SOURCES = sexp_test.ml
  RESULT = sexp_test
endef
export PROJ_sub2

ifndef SUBPROJS
  export SUBPROJS = sub1 sub2
endif

all:	bc

%:
	@$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBTARGET=$@
