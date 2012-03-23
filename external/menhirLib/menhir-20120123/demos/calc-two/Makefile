# Add --table on the next line to use Menhir's table-based back-end.
PGFLAGS         := --infer
GENERATED       := tokens.ml tokens.mli algebraic.ml algebraic.mli reverse.ml reverse.mli lexer.ml
MODULES         := algebraic reverse lexer calc
EXECUTABLE      := calc
OCAMLDEPWRAPPER := ../ocamldep.wrapper
include ../Makefile.shared
$(eval $(call menhir_monomodule,tokens,--only-tokens))
$(eval $(call menhir_multimodule,algebraic,tokens.mly algebraic.mly common.mly,--external-tokens Tokens))
$(eval $(call menhir_multimodule,reverse,tokens.mly reverse.mly common.mly,--external-tokens Tokens))
