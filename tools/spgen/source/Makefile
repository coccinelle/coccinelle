.PHONY: all
all: spgen

.PHONY: all.opt
all.opt: spgen.opt

.PHONY: clean
clean:
	make -C../../.. clean-spgen

spgen:
	make -C../../.. tools/spgen/source/spgen

spgen.opt:
	make -C../../.. tools/spgen/source/spgen.opt
