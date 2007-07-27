PROGS=gitgrep split_patch extract_c_and_res meta_files

all: $(PROGS)

gitgrep: gitgrep.ml
	ocamlopt -o gitgrep str.cmxa gitgrep.ml


split_patch: classic_patch.mli classic_patch.ml maintainers.mli maintainers.ml split_patch.ml
	ocamlc -g -dtypes -o $@ str.cma unix.cma \
	../commons/commons.cma -I ../commons \
	$+

extract_c_and_res: extract_c_and_res.ml
	ocamlc -g -dtypes -o $@ str.cma unix.cma \
	 ../commons/commons.cma -I ../commons \
	$+

meta_files: maintainers.mli maintainers.ml meta_files.ml
	ocamlc -g -dtypes -o $@ str.cma unix.cma \
	../commons/commons.cma -I ../commons -I ../ \
	../kbuild.cmo   \
	$+

clean::
	rm -f $(PROGS)

clean::
	rm -f *.cm[ioxa] *.o *.a *.cmxa *.annot

