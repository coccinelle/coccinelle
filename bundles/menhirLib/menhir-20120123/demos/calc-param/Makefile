# Add --table on the next line to use Menhir's table-based back-end.
PGFLAGS         := --infer
GENERATED       := parser.ml parser.mli lexer.ml tokens.ml tokens.mli
MODULES         := parser lexer calc
EXECUTABLE      := calc
OCAMLDEPWRAPPER := ../ocamldep.wrapper
include ../Makefile.shared
$(eval $(call menhir_monomodule,tokens,--only-tokens))
$(eval $(call menhir_multimodule,parser,tokens.mly parser.mly,--external-tokens Tokens))
