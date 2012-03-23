RESULT = type-conv

OCAMLMAKEFILE = ../OCamlMakefile
OCAMLFLAGS = -w Ae

INCDIRS = +camlp4

TRASH = pa_type_conv.cmi pa_type_conv.cmo pa_type_conv.annot

all: pa_type_conv.cmi pa_type_conv.cmo

LIBINSTALL_FILES = pa_type_conv.cmi pa_type_conv.cmo

install:	libinstall
uninstall:	libuninstall

-include $(OCAMLMAKEFILE)
