.PHONY : all stdcompat-current stdcompat-6
all : stdcompat-current
stdcompat-6:
	cd stdcompat-6; OCAMLFIND= aclocal && autoconf && automake && \
		./configure && make && \
		cp *.mli *.cmi *.cmx *.cma *.cmxa *.a ..

stdcompat-current:
	cd stdcompat-current; make && cp *.mli *.cmi *.cmx *.cma *.cmxa *.a ..

.PHONY : clean
clean :
	rm -f *.mli *.cmi *.cmo *.cmt *.cmti *.cmx *.cma *.cmxa *.a
	cd stdcompat-current && \
		rm -f *.cmi *.cmo *.cmx *.cmt *.cmti *.cma *.cmxa *.a *.cmxs

.PHONY : distclean
distclean : clean
