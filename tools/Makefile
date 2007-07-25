PROGS=gitgrep split_patch extract_c_and_res

all: $(PROGS)

gitgrep: gitgrep.ml
	ocamlopt -o gitgrep str.cmxa gitgrep.ml


split_patch: classic_patch.mli classic_patch.ml split_patch.ml
	ocamlc -g -o $@ str.cma unix.cma \
	../commons/commons.cma -I ../commons \
	$+

extract_c_and_res: extract_c_and_res.ml
	ocamlc -g -o $@ str.cma unix.cma \
	 ../commons/commons.cma -I ../commons \
	$+


clean::
	rm -f $(PROGS)

clean::
	rm -f *.cm[ioxa] *.o *.a *.cmxa *.annot

