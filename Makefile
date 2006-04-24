#############################################################################
# Configuration section
#############################################################################

TARGET=coccinelle

#############################################################################

SOURCEMAIN = \
	flag.ml  \
	classic_patch.ml  \
	isomorphisms.ml \
	cocci.ml   main.ml

# aux.ml
#	ast_c.ml \
#	ast_cocci.ml \
#	visitor_c.ml control_flow_c.ml \
#	isomorphisms.ml \
#	\
#	parsing_c/semantic_c.ml parsing_c/lexer_parser.ml \
#	parsing_c/parser_c.ml parsing_c/lexer_c.ml parsing_c/parse_c.ml \
#	parsing_c/unparse_c.ml \

EXEC=$(TARGET)
OPTEXEC=$(EXEC).opt

OBJS = $(SOURCEMAIN:.ml=.cmo)
OPTOBJS = $(SOURCEMAIN:.ml=.cmx)

SYSLIBS = str.cma unix.cma
LIBS=commons/commons.cma parsing_c/c_parser.cma parsing_cocci/cocci_parser.cma engine/engine.cma
SUBDIRS=commons parsing_c parsing_cocci engine
MAKESUBDIRS=commons parsing_c parsing_cocci engine

ADDONSPATH = -I commons -I parsing_c -I parsing_cocci  -I engine

OCAMLRUNPARAM = 'b'
export OCAMLRUNPARAM

# -w A ?
OCAMLC=ocamlc -g   $(ADDONSPATH)
OCAMLOPT=ocamlopt   $(ADDONSPATH) -p -inline 0
OCAMLLEX=ocamllex -ml
OCAMLYACC=ocamlyacc -v
OCAMLDEP=ocamldep  $(ADDONSPATH)
OCAMLMKTOP=ocamlmktop -g -custom $(ADDONSPATH)

all: rec $(EXEC)
opt: rec.opt $(OPTEXEC)

rec:
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i all; done 

rec.opt:
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i all.opt; done 

$(EXEC): $(OBJS) $(LIBS)
	$(OCAMLC) -o $(EXEC) $(SYSLIBS) $(LIBS) $(OBJS)


$(OPTEXEC): $(OPTOBJS) $(OPTLIBS)
	$(OCAMLOPT) -o $(OPTEXEC) $(SYSLIBS:.cma=.cmxa) $(LIBS:.cma=.cmxa) $(OPTOBJS)



clean::
	rm -f $(TARGET) $(TARGET).opt


clean::
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i clean; done 



.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(ADDONSPATH) -c $<
.mli.cmi:
	$(OCAMLC) $(ADDONSPATH) -c $<
.ml.cmx:
	$(OCAMLOPT) $(ADDONSPATH) -c $<

.ml.mldepend: 
	$(OCAMLC) $(ADDONSPATH) -i $<

clean::
	rm -f *.cm[iox] *.o

clean::
	rm -f *~ .*~ gmon.out #*#

beforedepend:

depend:: beforedepend
	$(OCAMLDEP) *.mli *.ml > .depend
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i depend; done

#parsing_c/*.mli parsing_c/*.ml engine/*.ml engine/*.mli

include .depend


