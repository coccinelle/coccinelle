.PHONY: all test clean

# The Menhir executable in the PATH and the library MenhirSdk
# should agree on their version number, or this test will fail
# complaining that magic strings do not match.
MENHIR     := menhir
DUNE       := dune
MAIN       := generate
EXECUTABLE := menhir-generate-printers

all:
	@ if command -v $(DUNE) > /dev/null ; then \
	  $(DUNE) build $(MAIN).exe && \
	  cp -f _build/default/$(MAIN).exe $(EXECUTABLE) ; \
	else \
	  echo "Error: $(DUNE) is required." ; \
	fi

TEST       := ../../test/good/parser_raw

test: all
	rm -f $(TEST).cmly
	$(MENHIR) --cmly $(TEST).mly
	./$(EXECUTABLE) $(TEST).cmly

clean:
	@ rm -f $(EXECUTABLE)
	@ rm -rf `cat .gitignore`
	@ rm -f *~
