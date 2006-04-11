#############################################################################
# Configuration section
#############################################################################

TARGET=coccinelle

#############################################################################

SOURCEMAIN = aux.ml \
	flag.ml  \
	classic_patch.ml  \
	\
	ast_c.ml \
	ast_cocci.ml \
	visitor_c.ml control_flow_c.ml \
	isomorphisms.ml \
	pattern.ml \
	\
        parsing_cocci/ast0_cocci.ml parsing_cocci/ast0toast.ml parsing_cocci/top_level.ml parsing_cocci/check_meta.ml parsing_cocci/arity.ml parsing_cocci/plus.ml parsing_cocci/merge.ml  parsing_cocci/data.ml \
	\
	parsing_c/semantic_c.ml          parsing_c/lexer_parser.ml     parsing_c/parser_c.ml         parsing_c/lexer_c.ml          parsing_c/parse_c.ml  	   parsing_c/unparse_c.ml \
	parsing_cocci/semantic_cocci.ml                                parsing_cocci/parser_cocci.ml parsing_cocci/lexer_cocci.ml  parsing_cocci/unparse_cocci.ml parsing_cocci/parse_cocci.ml      \
	\
	cocci.ml   main.ml

SYSLIBS = str.cma unix.cma
LIBS=commons/commons.cma 
COMMONDIR=commons
SUBDIRS=commons parsing_c parsing_cocci


ADDONSPATH = -I $(COMMONDIR) -I parsing_c -I parsing_cocci 

OCAMLRUNPARAM = 'b'
export OCAMLRUNPARAM

# -w A ?
OCAMLC=ocamlc -g   $(ADDONSPATH)
OCAMLOPT=ocamlopt   $(ADDONSPATH) -p -inline 0
OCAMLLEX=ocamllex -ml
OCAMLYACC=ocamlyacc -v
OCAMLDEP=ocamldep  $(ADDONSPATH)
OCAMLMKTOP=ocamlmktop -g -custom $(ADDONSPATH)

LIB=$(TARGET).cma
OPTLIB=$(LIB:.cma=.cmxa)



OBJS = $(SOURCEMAIN:.ml=.cmo)
OPTOBJS = $(SOURCEMAIN:.ml=.cmx)




all: $(TARGET)   
#$(TARGET).top
#$(TARGET).opt

$(TARGET): $(OBJS) $(LIBS)
	$(OCAMLC) -o $(TARGET) $(SYSLIBS) $(LIBS) $(OBJS)

$(TARGET).opt: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -o $(TARGET).opt   $(SYSLIBS:.cma=.cmxa) $(LIBS:.cma=.cmxa)  $(OPTOBJS)

$(TARGET).top: $(OBJS) $(LIBS)
	$(OCAMLMKTOP) -o $(TARGET).top $(SYSLIBS) $(LIBS) $(OBJS)


clean::
	rm -f $(TARGET) $(TARGET).opt $(TARGET).top




commons/commons.cma:
	cd commons; $(MAKE) commons.cma

commons/%: 
	cd commons; $(MAKE)



clean::
	set -e; for i in $(SUBDIRS); do $(MAKE) -C $$i clean; done 



parsing_c/lexer_c.ml: parsing_c/lexer_c.mll
	$(OCAMLLEX) $<
clean::
	rm -f parsing_c/lexer_c.ml
beforedepend:: parsing_c/lexer_c.ml


parsing_c/parser_c.ml parsing_c/parser_c.mli: parsing_c/parser_c.mly
	$(OCAMLYACC) $<
clean::
	rm -f parsing_c/parser_c.ml parsing_c/parser_c.mli parsing_c/parser_c.output
beforedepend:: parsing_c/parser_c.ml parsing_c/parser_c.mli





parsing_cocci/lexer_cocci.ml: parsing_cocci/lexer_cocci.mll
	$(OCAMLLEX) $<
clean::
	rm -f parsing_cocci/lexer_cocci.ml
beforedepend:: parsing_cocci/lexer_cocci.ml


parsing_cocci/parser_cocci.ml parsing_cocci/parser_cocci.mli: parsing_cocci/parser_cocci.mly
	$(OCAMLYACC) $<
clean::
	rm -f parsing_cocci/parser_cocci.ml parsing_cocci/parser_cocci.mli parsing_cocci/parser_cocci.output
beforedepend:: parsing_cocci/parser_cocci.ml parsing_cocci/parser_cocci.mli




.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) -c $<
.mli.cmi:
	$(OCAMLC) -c $<
.ml.cmx:
	$(OCAMLOPT) -c $<

.ml.mldepend: 
	$(OCAMLC) -i $<

clean::
	rm -f *.cm[iox] *.o

clean::
	rm -f *~ .*~ gmon.out #*#

beforedepend::

depend:: beforedepend
	$(OCAMLDEP) *.mli *.ml parsing_c/*.mli parsing_c/*.ml parsing_cocci/*.mli parsing_cocci/*.ml  > .depend

include .depend


