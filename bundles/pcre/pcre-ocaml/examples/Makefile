TARGETS = $(addsuffix .bc, cloc count_hash pcregrep subst)

.PHONY: all clean

all:
	@dune build $(TARGETS)

clean:
	@dune clean
