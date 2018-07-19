.PHONY : all
all :
	cd stdcompat-5; ./configure && make && \
		cp *.mli *.cmi *.cmx *.cmxa *.a ..

.PHONY : clean
clean :
	rm -f *.mli *.cmx *.cmxa *.a
	- make -C stdcompat-5 clean

.PHONY : distclean
distclean : clean
