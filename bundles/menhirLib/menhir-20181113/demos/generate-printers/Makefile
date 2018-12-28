.PHONY: all test clean

# The Menhir executable in the PATH and the library MenhirSdk
# should agree on their version number, or this test will fail
# complaining that magic strings do not match.
MENHIR     := menhir
OCAMLBUILD := ocamlbuild -use-ocamlfind
MAIN       := generate
EXECUTABLE := menhir-generate-printers

all:
	$(OCAMLBUILD) $(MAIN).native
	rm -f $(MAIN).native
	cp -f _build/$(MAIN).native $(EXECUTABLE)

TEST       := ../../test/good/parser_raw

test: all
	rm -f $(TEST).cmly
	$(MENHIR) --cmly $(TEST).mly
	./$(EXECUTABLE) $(TEST).cmly

clean:
	rm -f *~ .*~
	$(OCAMLBUILD) -clean
	rm -f $(EXECUTABLE)
