.PHONY: all clean test

DUNE       := dune
EXECUTABLE := calc.exe

all:
	@ if command -v $(DUNE) > /dev/null ; then \
	  $(DUNE) build $(EXECUTABLE) ; \
	else \
	  echo "Error: $(DUNE) is required." ; \
	fi

clean:
	rm -rf `cat .gitignore`
	rm -f *~

test: all
	@echo "The following command should print 42:"
	echo "(1 + 2 * 5) * 2 + 10 - 2 * -2 * 1 + 4 + 2" | ./_build/default/$(EXECUTABLE)
