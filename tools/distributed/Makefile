spatch_linux: spatch_linux.c
	gcc -o spatch_linux spatch_linux.c

install: spatch_linux cleanup
	cp spatch_linux ~/bin

cleanup: cleanup.ml
	ocamlc -o cleanup str.cma cleanup.ml
