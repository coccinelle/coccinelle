PROGS=gitgrep split_patch extract_c_and_res

all: $(PROGS)

gitgrep: gitgrep.ml
	ocamlopt -o gitgrep str.cmxa gitgrep.ml


split_patch: classic_patch.mli classic_patch.ml split_patch.ml
	ocamlopt -o $@ str.cmxa unix.cmxa \
	../commons/commons.cmxa -I ../commons \
	$+

extract_c_and_res: extract_c_and_res.ml
	ocamlopt -o $@ str.cmxa unix.cmxa \
	 ../commons/commons.cmxa -I ../commons \
	$+


clean::
	rm -f $(PROGS)

clean::
	rm -f *.cm[ioxa] *.o *.a *.cmxa *.annot

