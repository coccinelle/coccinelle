.PHONY : all
all :
# Hack to make these files fresh
#	touch stdcompat-6/Makefile.in
#	touch stdcompat-6/aclocal.m4
	cd stdcompat-6; OCAMLFIND= ./configure && make && \
		cp *.mli *.cmi *.cmx *.cma *.cmxa *.a ..

.PHONY : clean
clean :
	rm -f *.mli *.cmi *.cmx *.cma *.cmxa *.a
	- make -C stdcompat-6 clean

.PHONY : distclean
distclean : clean
