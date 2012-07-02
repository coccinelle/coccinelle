# Add --table on the next line to use Menhir's table-based back-end.
PGFLAGS         := --infer
GENERATED       := parser.ml parser.mli lexer.ml
MODULES         := parser lexer calc
EXECUTABLE      := calc
OCAMLDEPWRAPPER := ../ocamldep.wrapper
include ../Makefile.shared
include ../Makefile.auto
