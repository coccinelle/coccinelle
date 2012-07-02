.PHONY: clean

COLD         += clean

# ----------------------------------------------------------------------------

# A few settings differ on Windows versus Unix.

include ../Makefile.arch

# ----------------------------------------------------------------------------
# Locating the ocaml compilers.
# If ocamlfind is available, then it is used for that purpose.

CAMLTOP         := ocaml

CAMLC           := $(shell if ocamlfind ocamlc -v >/dev/null 2>&1 ; \
                       then echo ocamlfind ocamlc ; \
		       elif ocamlc.opt -v >/dev/null 2>&1 ; \
                       then echo ocamlc.opt ; \
		       else echo ocamlc ; fi)

CAMLOPT         := $(shell if ocamlfind ocamlopt -v >/dev/null 2>&1 ; \
                       then echo ocamlfind ocamlopt ; \
		       elif ocamlopt.opt -v >/dev/null 2>&1 ; \
                       then echo ocamlopt.opt ; \
		       else echo ocamlopt ; fi)

CAMLDEP         := $(shell if ocamlfind ocamldep -version >/dev/null 2>&1 ; \
                       then echo ocamlfind ocamldep ; \
		       elif ocamldep.opt -version >/dev/null 2>&1 ; \
                       then echo ocamldep.opt ; \
		       else echo ocamldep ; fi)

CAMLDEPWRAPPER  := ../demos/ocamldep.wrapper

CAMLLEX         := ocamllex

CAMLYACC        := ocamlyacc -v

# -------------------------------------------------------------------------

# Compilation flags.

BFLAGS       := -g
OFLAGS       := -inline 1000
LNKBFLAGS    := -g
LNKOFLAGS    :=
BLIBS        := unix.cma
OLIBS        := unix.cmxa
PGFLAGS      := -v -lg 1 -la 1 -lc 1 --comment --infer --error-recovery --stdlib . --strict

# -------------------------------------------------------------------------

# A list of the source files that must be generated prior to dependency
# analysis.

GENERATED := installation.ml lexmli.ml lexer.ml parser.mli parser.ml		\
lineCount.ml lexdep.ml sentenceParser.mli sentenceParser.ml sentenceLexer.ml

# -------------------------------------------------------------------------

# A list of the modules that must be linked into the MenhirLib library.

# This library is used both at compile time (i.e., within Menhir itself)
# and at run time (i.e., it is made available to Menhir users, who need
# to link it with their own executables if they have used the --table
# option).

# If you change this list, please also update the files LICENSE and
# GNUmakefile in the toplevel directory.

LIBMODULES := infiniteArray packedIntArray rowDisplacement engineTypes	\
engine tableFormat tableInterpreter convert

# -------------------------------------------------------------------------

# A list of the modules that must be linked into the Menhir executable.

MODULES := menhirLib Fix stringSet stringMap mark compressedBitSet	\
           unionFind tarjan nonpositiveCycles patricia misc option	\
           breadth listMonad dot installation version settings time	\
           positions error parameters keyword lineCount printer		\
           rawPrinter action parserAux parser lexer partialGrammar	\
           parameterizedGrammar reachability unparameterizedPrinter	\
           preFront codeBits tokenType interface IO lexmli lexdep	\
           infer nonTerminalDefinitionInlining front grammar item lr0	\
           slr lr1 lr1partial derivation conflict invariant codePieces	\
           sentenceParser sentenceLexer pprint cst			\
           referenceInterpreter interpret tableBackend codeBackend	\
           coqBackend traverse inliner back

# -------------------------------------------------------------------------

# How to bootstrap.

# Set TARGET to byte or opt depending on the desired architecture.

ifndef TARGET
  TARGET     := opt
endif

# The variable GOAL is the name of the executable file.

GOAL         := menhir.$(TARGET)

# We create a symbolic link of GOAL to MENHIREXE.

$(MENHIREXE): .versioncheck
# Build a stage one executable using ocamlyacc.
	$(MAKE) -s PGEN="$(CAMLYACC)" PARSER=parser $(GOAL)
# Remove the ocamlyacc-built parser.
	@/bin/rm -f parser.ml parser.mli
# Build a stage two executable using the stage one executable (which is overwritten).
	$(MAKE) -s PGEN="./$(GOAL) $(PGFLAGS)" PARSER=fancy-parser $(GOAL)
# Create a stage three parser and make sure that it is identical.
	@./$(GOAL) $(PGFLAGS) -b reference fancy-parser.mly 2>/dev/null
	@if diff parser.mli reference.mli 2>&1 >/dev/null ; then \
	  if diff parser.ml reference.ml 2>&1 >/dev/null ; then \
	    echo "Bootstrap successful." ; \
	  else \
	    echo "Bootstrap FAILED: the implementation files differ." && false ; \
          fi ; \
	else \
	  echo "Bootstrap FAILED: the interface files differ." && false ; \
	fi
	@rm -f reference.ml reference.mli
# Rename the stage two executable to the desired name.
# Use a symbolic link, so that further development builds implicitly update
# menhir.
	@ln -sf $(GOAL) $@

# -------------------------------------------------------------------------

# Linking.

menhirLib.cmo menhirLib.cmi: $(LIBMODULES:=.cmo)
	$(CAMLC) $(BFLAGS) -pack -o menhirLib.cmo $^

menhirLib.cmx menhirLib.o: $(LIBMODULES:=.cmx)
	$(CAMLOPT) -pack -o menhirLib.cmx $^

menhir.byte: $(MODULES:=.cmo)
	$(CAMLC) -o $@ $(LNKBFLAGS) $(BLIBS) $^

menhir.opt: $(MODULES:=.cmx)
	$(CAMLOPT) -o $@ $(LNKOFLAGS) $(OLIBS) $^

# -------------------------------------------------------------------------

# Computing dependencies. This can be done in a simple way, even though
# we exploit --infer, because we compile in two stages. Not a good example
# of how to do it yourself -- have a look at demos/Makefile.shared.

# For completeness, we must force ocamldep to understand that MenhirLib
# is a module name. We do this by creating phantom source files for it.

.depend: $(wildcard *.ml *.mli) $(GENERATED)
	@/bin/rm -f .depend
	for i in *.ml *.mli; do \
           $(CAMLDEPWRAPPER) menhirLib.ml menhirLib.mli - $(CAMLDEP) $$i \
        >> $@; \
	done

ifeq ($(findstring $(MAKECMDGOALS),$(COLD)),)
-include .depend
endif

# -------------------------------------------------------------------------

# Cleaning up.

clean::
	/bin/rm -f menhir.byte menhir.opt $(MENHIREXE)
	/bin/rm -f *.cmi *.cmx *.cmo *.$(OBJ) *~ .*~
	/bin/rm -f reference.ml reference.mli $(GENERATED)
	/bin/rm -f .depend *.conflicts *.automaton *.annot *.output

# -------------------------------------------------------------------------

# Compiling. The parser source is found in $(PARSER).mly and is
# processed using $(PGEN).

# These two default definitions really shouldn't be necessary, but
# there are corner cases where they are needed (e.g. when make is
# invoked without a target and the .depend file is regenerated).

ifndef PGEN
  PGEN       := $(CAMLYACC)
endif
ifndef PARSER
  PARSER     := parser
endif

%.cmi: %.mli
	$(CAMLC) $(BFLAGS) -c $<

%.cmo: %.ml
	$(CAMLC) $(BFLAGS) -c $<

# If the module that is being compiled is part of MenhirLib, add the
# -for-pack option to the command line. This is required only when
# compiling to native code (the bytecode compiler accepts but ignores
# this option).

PACK = $(shell if echo $(LIBMODULES) | grep $* >/dev/null ; then echo -for-pack MenhirLib ; else echo ; fi)

%.cmx %.o: %.ml
	$(CAMLOPT) $(OFLAGS) $(PACK) -c $<

# The source file for this parser varies. It is either parser.mly or
# fancy-parser.mly.
#
parser.ml parser.mli: $(PARSER).mly
	@/bin/rm -f parser.ml parser.mli
	$(PGEN) -b parser $<

# This parser must be built with ocamlyacc, because its client
# watches for Parsing.Parse_error!
#
# Using ocamlyacc or Menhir interchangeably would be possible,
# via an ocamlyacc wrapper that adds the definition "exception
# Error = Parsing.Parse_error" at the end of the generated .ml
# and .mli files.
#
sentenceParser.ml sentenceParser.mli : sentenceParser.mly
	@/bin/rm -f sentenceParser.ml sentenceParser.mli
	$(CAMLYACC) -b sentenceParser $<

%.ml: %.mll
	@/bin/rm -f $@
	$(CAMLLEX) $<

# ----------------------------------------------------------------------------
# Checking the version of the ocaml compiler.
#
# We check the bytecode compiler only, because some architectures don't have
# the native code compiler. We assume that both compilers, if present, are in
# sync.

# We build a bytecode executable (rather than use the toplevel loop) because
# we need to load str.cma and some ocaml ports do not support dynamic loading
# (e.g. ocaml 3.09, MacOS/Intel).

.versioncheck:
	@ echo Checking that Objective Caml is recent enough...
	@ $(CAMLC) -o check-ocaml-version str.cma checkOCamlVersion.ml
	@ ./check-ocaml-version --verbose --gt "3.09"
	@ rm check-ocaml-version
	@ touch .versioncheck

clean::
	rm -f .versioncheck

