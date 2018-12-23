# Bundles the original menhirLib package without modifications.
# This Makefile integrates its build process with coccinelle.

MENHIR_VERSION=20181113

PACKAGE_DIR=menhir-$(MENHIR_VERSION)
MENHIR_MODULES= \
	Fix IO installation keyword lineCount positions stringSet version \
	settings printer codeBits action mark stringMap menhirLib error misc codeBits gSet \
	compressedBitSet option dot InputFile parameters InputFile lexpointfree parserAux \
	parser lexer \
	tarjan unionFind expandTokenAliases partialGrammar time \
	unparameterizedSyntax tokenType nonterminalType interface lexdep \
	lexmli infer listMonad chopInlined sequence CompletedNatWitness \
	Unifier Memoize SortUnification SortInference GroundSort SelectiveExpansion syntax Drop CheckSafeParameterizedGrammar \
	nonTerminalDefinitionInlining reachability unparameterizedPrinter \
	FixSolver Boolean Maps patricia grammarFunctor keywordExpansion \
	anonymous Driver front gMap grammar codePieces item derivation \
	lr0 slr lr1 DependencyGraph lr1partial conflict invariant default Trie MySet resizableArray LowIntegerPriorityQueue LRijkstraCore \
	codeBackend coqBackend \
	pprint cst traverse inliner referenceInterpreter sentenceParser \
	sentenceLexer segment sentenceParserAux interpret tableBackend \
	astar LRijkstra cmly_api \
	cmly_format cmly_read cmly_write back menhir
MERE_INTERFACES=stretch IL lookahead
PRODUCTS=chopInlined lexer lexdep lexmli parser sentenceLexer sentenceParser lexpointfree lineCount segment
SIDEPRODUCTS=menhir menhirLib.cmi
SIDEPRODUCTS_BYTECODE=menhirLib.cmo
SIDEPRODUCTS_NATIVE=menhirLib.cmx menhirLib.o
OTHERS=standard.mly
SRC_DIR := $(PACKAGE_DIR)/src
CLEANOTHERS := menhirLib.ml menhirLib.mli syntax.cmi $(SRC_DIR)/parser.mly $(SRC_DIR)/menhir $(SRC_DIR)/installation.ml

OCAMLLDFLAGS=unix.cmxa
OCAMLFLAGS+=-I ../stdcompat

MENHIRLIB_MODULES= \
	General IncrementalEngine InfiniteArray PackedIntArray LinearizedArray RowDisplacement \
	EngineTypes Engine TableFormat TableInterpreter Convert
MODULES=$(MENHIR_MODULES) $(MENHIRLIB_MODULES)

OCAML_BUNDLES_FLAGS=$(shell \
	if echo $(MENHIRLIB_MODULES) \
		| grep -w $(notdir $(basename $<)) >/dev/null; then \
		echo -for-pack MenhirLib; \
	fi)

include ../Makefile.bundles

include ../../Makefile.package

depend: .depend

.PHONY: distclean
distclean: clean

$(SRC_DIR)/installation.ml:
	echo "let libdir = \"$(shell pwd\)\"" > $(SRC_DIR)/installation.ml
	echo "let ocamlfind = false" >> $(SRC_DIR)/installation.ml

.depend: $(SRC_DIR)/parser.mly

$(SRC_DIR)/Driver.ml: $(SRC_DIR)/yaccDriver.ml
	cp $< $@

ifeq ($(OCAMLOPT_CMD),)
$(SRC_DIR)/menhir: $(patsubst %,$(SRC_DIR)/%.cmo,$(MENHIR_MODULES))
	$(OCAMLC_CMD) $(OCAMLLDFLAGS) $(LINK_BYTECODE) stdcompat.cma $^ -o $@
else
$(SRC_DIR)/menhir: $(patsubst %,$(SRC_DIR)/%.cmx,$(MENHIR_MODULES))
	$(OCAMLOPT_CMD) $(OCAMLLDFLAGS) $(LINK_NATIVE) stdcompat.cmxa $^ -o $@
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
