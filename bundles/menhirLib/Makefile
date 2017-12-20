# Bundles the original menhirLib package without modifications.
# This Makefile integrates its build process with coccinelle.

MENHIR_VERSION=20171013

PACKAGE_DIR=menhir-$(MENHIR_VERSION)
TARBALL=menhir-$(MENHIR_VERSION).tar.gz
MENHIR_MODULES= \
	Fix IO installation keyword lineCount positions stringSet version \
	settings printer action mark stringMap menhirLib misc codeBits gSet \
	compressedBitSet option dot error parameters InputFile parserAux parser lexer \
	tarjan unionFind parameterizedGrammar partialGrammar time \
	unparameterizedSyntax tokenType nonterminalType interface lexdep lexmli infer listMonad \
	nonTerminalDefinitionInlining reachability unparameterizedPrinter \
	FixSolver Boolean Maps patricia grammarFunctor keywordExpansion anonymous Driver front gMap grammar codePieces item derivation \
	lr0 slr lr1 lr1partial DependencyGraph conflict invariant default codeBackend coqBackend \
	pprint cst traverse inliner referenceInterpreter sentenceParser \
	sentenceLexer segment sentenceParserAux interpret tableBackend MySet resizableArray LowIntegerPriorityQueue astar LRijkstra cmly_write back menhir
PRODUCTS=lexer lexdep lexmli parser sentenceLexer sentenceParser
SIDEPRODUCTS=menhir menhirLib.cmi
SIDEPRODUCTS_BYTECODE=menhirLib.cmo
SIDEPRODUCTS_NATIVE=menhirLib.cmx menhirLib.o
OTHERS=standard.mly
CLEANOTHERS=menhirLib.ml menhirLib.mli
SRC_DIR=$(PACKAGE_DIR)/src

OCAMLLDFLAGS=unix.cmxa
OCAMLFLAGS+=-I ../stdcompat

MENHIRLIB_MODULES= \
	General InfiniteArray PackedIntArray LinearizedArray RowDisplacement \
	EngineTypes Engine TableFormat TableInterpreter Convert
MODULES=$(MENHIR_MODULES) $(MENHIRLIB_MODULES)

OCAML_BUNDLES_FLAGS=$(shell \
  if echo $(MENHIRLIB_MODULES) | grep -w $(notdir $(basename $<)) >/dev/null; then \
    echo -for-pack MenhirLib; \
  fi)

include ../Makefile.bundles

depend: .depend
	@$(MAKE) all-build

distclean: clean
	rm -rf $(PACKAGE_DIR)
	rm -f .prepare

.prepare: $(MARKER)
	echo "let libdir = \"$(shell pwd\)\"" > $(SRC_DIR)/installation.ml
	echo "let ocamlfind = false" >> $(SRC_DIR)/installation.ml
	patch -p0 <menhir-for-ocaml-3.12.patch
	touch $@

$(SRC_DIR)/lexdep.mll \
$(SRC_DIR)/lexmli.mll \
$(SRC_DIR)/lexer.mll \
$(SRC_DIR)/sentenceLexer.mll \
$(SRC_DIR)/sentenceParser.mly \
$(SRC_DIR)/yacc-parser.mly: $(MARKER)

$(SRC_DIR)/Driver.ml: $(SRC_DIR)/yaccDriver.ml
	cp $< $@

ifeq ($(OCAMLOPT_CMD),)
$(SRC_DIR)/menhir: $(patsubst %,$(SRC_DIR)/%.cmo,$(MENHIR_MODULES))
	$(OCAMLC_CMD) $(OCAMLLDFLAGS) stdcompat.cmo $^ -o $@
else
$(SRC_DIR)/menhir: $(patsubst %,$(SRC_DIR)/%.cmx,$(MENHIR_MODULES))
	$(OCAMLOPT_CMD) $(OCAMLLDFLAGS) stdcompat.cmx $^ -o $@
endif

standard.mly: $(SRC_DIR)/standard.mly
	cp $< $@

$(SRC_DIR)/misc.cmo: $(SRC_DIR)/menhirLib.cmi

$(SRC_DIR)/misc.cmx: $(SRC_DIR)/menhirLib.cmx

$(SRC_DIR)/menhirLib.cmo: \
	$(patsubst %,$(SRC_DIR)/%.cmo,$(MENHIRLIB_MODULES))
	$(OCAMLC_CMD) -pack \
		$(patsubst %,$(SRC_DIR)/%.cmo,$(MENHIRLIB_MODULES)) \
		-o $(SRC_DIR)/menhirLib.cmo
$(SRC_DIR)/menhirLib.cmi: $(SRC_DIR)/menhirLib.cmo

$(SRC_DIR)/menhirLib.cmx: \
	$(patsubst %,$(SRC_DIR)/%.cmx,$(MENHIRLIB_MODULES))
	$(OCAMLOPT_CMD) -pack \
		$(patsubst %,$(SRC_DIR)/%.cmx,$(MENHIRLIB_MODULES)) \
		-o $(SRC_DIR)/menhirLib.cmx
$(SRC_DIR)/menhirLib.o: $(SRC_DIR)/menhirLib.cmx

$(SRC_DIR)/parser.mly: $(SRC_DIR)/yacc-parser.mly
	cp $< $@

$(SRC_DIR)/Driver.cmo $(SRC_DIR)/Driver.cmx: $(SRC_DIR)/Driver.cmi

$(SRC_DIR)/astar.cmo $(SRC_DIR)/astar.cmx: $(SRC_DIR)/menhirLib.cmi

menhir: $(SRC_DIR)/menhir
	cp $< $@
