spatch_linux: spatch_linux.c
	gcc -D HOME="\"${COCCINELLE_HOME}/tools/distributed/\"" -o spatch_linux spatch_linux.c

install: spatch_linux cleanup
	cp spatch_linux ~/bin

cleanup: cleanup.ml
	ocamlc -o cleanup str.cma cleanup.ml

clean:
	rm -f cleanup cleanup.cmi cleanup.cmo spatch_linux
