-include ../Makefile.config

PROGS=spp gitgrep splitpatch extract_c_and_res #generate_dependencies

all: $(SUBDIRS) $(PROGS)

# for recursive make
all.opt: $(PROGS)

gitgrep: gitgrep.ml
	ocamlopt -o gitgrep str.cmxa gitgrep.ml

gitsort: gitsort.ml
	ocamlopt -o gitsort str.cmxa gitsort.ml

isoprof: process_isoprofile.ml
	ocamlc -g -o isoprof str.cma process_isoprofile.ml

alloc_free: alloc_free.ml
	ocamlopt -o alloc_free str.cmxa alloc_free.ml

# more flexible version of alloc_free
bridge: bridge.ml
	ocamlopt -o bridge str.cmxa dumper.mli dumper.ml bridge.ml

install_bridge: bridge
	cp -f bridge /usr/local/bin

.PHONY: $(SUBDIRS)


SYSLIBS = str.cma unix.cma bigarray.cma

INCLUDE=-I ../commons -I ../extra -I  ../parsing_c
LIBS=../commons/commons.cma ../globals/globals.cma \
     ../parsing_c/parsing_c.cma ../extra/extra.cma

OCAMLC=ocamlc$(OPTBIN) -g -dtypes   $(INCLUDE)
OCAMLOPT=ocamlopt$(OPTBIN)   $(INCLUDE) $(OPTFLAGS)
#OCAMLLEX=ocamllex$(OPTBIN) -ml
OCAMLLEX=ocamllex$(OPTBIN)
OCAMLYACC=ocamlyacc -v
OCAMLDEP=ocamldep$(OPTBIN) $(INCLUDE)
OCAMLMKTOP=ocamlmktop -g -custom $(INCLUDE)

spp: spp.ml
	$(OCAMLC) -o $@ $(SYSLIBS) $(INCLUDE) $(LIBS)  $+
#	ocamlc -I ../commons/ -o spp str.cma unix.cma bigarray.cma  ../commons/common.cmo spp.ml

splitpatch: splitpatch.cmo
	$(OCAMLC) -o $@ $(SYSLIBS) $(INCLUDE) $+

extract_c_and_res: extract_c_and_res.cmo
	$(OCAMLC) -o $@ $(SYSLIBS) $(INCLUDE) $(LIBS)  $+

generate_dependencies: generate_dependencies.cmo
	$(OCAMLC) -o $@ $(SYSLIBS) $(INCLUDE) $(LIBS)  $+

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) -c $<
.mli.cmi:
	$(OCAMLC) -c $<
.ml.cmx:
	$(OCAMLOPT) -c $<

clean:
	rm -f $(PROGS)
	rm -f *.cm[ioxa] *.o *.a *.cmxa *.annot

beforedepend::

depend:: beforedepend
	ocamldep $(INCLUDE) *.mli *.ml    > .depend

distclean:: clean
	rm -f .depend

-include .depend
