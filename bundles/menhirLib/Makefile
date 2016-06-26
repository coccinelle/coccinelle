# Bundles the original menhirLib package without modifications.
# This Makefile integrates its build process with coccinelle.

MENHIR_VERSION=20140422

PACKAGE_DIR=menhir-$(MENHIR_VERSION)
TARBALL=menhir-$(MENHIR_VERSION).tar.gz
MODULES= \
	Fix IO installation keyword lineCount positions stringSet version \
	settings printer action mark stringMap misc breadth codeBits gSet \
	compressedBitSet option dot error parameters parserAux parser lexer \
	tarjan unionFind parameterizedGrammar partialGrammar time preFront \
	tokenType interface lexdep lexmli infer listMonad \
	nonTerminalDefinitionInlining reachability unparameterizedPrinter \
	front gMap patricia grammar codePieces item derivation menhirLib \
	lr0 slr lr1 lr1partial conflict invariant codeBackend coqBackend \
	pprint cst traverse inliner referenceInterpreter sentenceParser \
	sentenceLexer interpret tableBackend back menhir
PRODUCTS=lexer lexdep lexmli parser sentenceLexer sentenceParser
SIDEPRODUCTS=menhir menhirLib.cmi menhirLib.cmx menhirLib.cmo menhirLib.o
OTHERS=standard.mly
CLEANOTHERS=menhirLib.ml menhirLib.mli
SRC_DIR=$(PACKAGE_DIR)/src

OCAMLLDFLAGS=unix.cmxa

MENHIRLIB_MODULES= \
	infiniteArray packedIntArray rowDisplacement engineTypes engine \
	tableFormat tableInterpreter convert

include ../Makefile.bundles

depend: .depend
	@$(MAKE) all-build

distclean: clean
	rm -rf $(PACKAGE_DIR)
	rm -f .prepare

.prepare: $(MARKER)
	echo "let libdir = \"$(shell pwd\)\"" > $(SRC_DIR)/installation.ml
	echo "let ocamlfind = false" >> $(SRC_DIR)/installation.ml
	touch $@

$(SRC_DIR)/lexdep.mll \
$(SRC_DIR)/lexmli.mll \
$(SRC_DIR)/lexer.mll \
$(SRC_DIR)/sentenceLexer.mll \
$(SRC_DIR)/sentenceParser.mly \
$(SRC_DIR)/yacc-parser.mly: $(MARKER)

%.cmo: %.ml
	if echo $(MENHIRLIB_MODULES) | grep -w $(notdir $(basename $<)); then \
	  $(OCAMLC_CMD) -for-pack MenhirLib -c $< -o $@; \
	else \
	  $(OCAMLC_CMD) -c $< -o $@; \
	fi

%.cmx: %.ml %.cmo
	if echo $(MENHIRLIB_MODULES) | grep -w $(notdir $(basename $<)); then \
	  $(OCAMLOPT_CMD) -for-pack MenhirLib -c $< -o $@; \
	else \
	  $(OCAMLOPT_CMD) -c $< -o $@; \
	fi

$(SRC_DIR)/menhir: $(patsubst %,$(SRC_DIR)/%.cmx,$(MODULES))
	$(OCAMLOPT_CMD) $(OCAMLLDFLAGS) $^ -o $@

standard.mly: $(SRC_DIR)/standard.mly
	cp $< $@

$(SRC_DIR)/menhirLib.cmi \
$(SRC_DIR)/menhirLib.cmo: \
	$(patsubst %,$(SRC_DIR)/%.cmo,$(MENHIRLIB_MODULES))
	$(OCAMLC_CMD) -pack $^ -o $(SRC_DIR)/menhirLib.cmo

$(SRC_DIR)/menhirLib.cmx \
$(SRC_DIR)/menhirLib.o: \
	$(patsubst %,$(SRC_DIR)/%.cmx,$(MENHIRLIB_MODULES)) \
	$(SRC_DIR)/menhirLib.cmo
	$(OCAMLOPT_CMD) -pack \
		$(patsubst %,$(SRC_DIR)/%.cmx,$(MENHIRLIB_MODULES)) \
		-o $(SRC_DIR)/menhirLib.cmx

$(SRC_DIR)/gSet.cmi $(SRC_DIR)/gSet.cmx: $(SRC_DIR)/gSet.ml
	$(OCAMLOPT_CMD) -c $<

$(SRC_DIR)/parser.mly: $(SRC_DIR)/yacc-parser.mly
	cp $< $@

menhir: $(SRC_DIR)/menhir
	cp $< $@
