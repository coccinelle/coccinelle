.PHONY : all
all :
	cd stdcompat-7; make && cp *.mli *.cmi *.cmx *.cma *.cmxa *.a ..

.PHONY : clean
clean :
	rm -f *.mli *.cmi *.cmx *.cma *.cmxa *.a
	- make -C stdcompat-6 clean

.PHONY : distclean
distclean : clean
