.PHONY : all
all : spgen

.PHONY : all.opt
all.opt : spgen.opt

.PHONY : clean
clean :
	$(MAKE) -C../../.. clean-spgen

spgen :
	$(MAKE) -C../../.. tools/spgen/source/spgen

spgen.opt :
	$(MAKE) -C../../.. tools/spgen/source/spgen.opt
