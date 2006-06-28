# Don't forget to first do a 'make links' to be able to compile the standalone c parser

TARGET=c_parser

SOURCES = flag_parsing_c.ml ast_c.ml type_annoter_c.ml semantic_c.ml visitor_c.ml \
          control_flow_c.ml lexer_parser.ml parser_c.ml lexer_c.ml parse_c.ml \
          unparse_c.ml

SYSLIBS = str.cma unix.cma 

# parsing_c now depends on cocci_parser because in addition to decorate the
# token in Ast_c with some parse info, we now also make some place to
# welcome some mcodekind of Ast_cocci.
LIBS=../commons/commons.cma  ../parsing_cocci/cocci_parser.cma
ADDONSPATH= -I ../commons -I  ../parsing_cocci 

OCAMLRUNPARAM = 'b'
export OCAMLRUNPARAM

#for warning:  -w A 
#for profiling:  -p -inline 0   with OCAMLOPT
OCAMLC=ocamlc -g   $(ADDONSPATH) 
OCAMLOPT=ocamlopt   $(ADDONSPATH) $(OPTFLAGS)
OCAMLLEX=ocamllex -ml
OCAMLYACC=ocamlyacc -v
OCAMLDEP=ocamldep  $(ADDONSPATH)
OCAMLMKTOP=ocamlmktop -g -custom $(ADDONSPATH)


OBJS = $(SOURCES:.ml=.cmo)
OPTOBJS = $(SOURCES:.ml=.cmx)



all: $(TARGET).byte $(TARGET).cma
#$(TARGET).top
#$(TARGET).opt

all.opt: $(TARGET).cmxa

$(TARGET).byte: $(OBJS) $(LIBS)
	$(OCAMLC) -o $(TARGET).byte $(SYSLIBS) $(LIBS) $(OBJS)

$(TARGET).opt: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -o $(TARGET).opt   $(SYSLIBS:.cma=.cmxa) $(LIBS:.cma=.cmxa)  $(OPTOBJS)

$(TARGET).cma: $(OBJS) $(LIBS)
	$(OCAMLC) -a -o $(TARGET).cma $(OBJS)

$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -a -o $(TARGET).cmxa $(OPTOBJS)

$(TARGET).top: $(OBJS) $(LIBS)
	$(OCAMLMKTOP) -o $(TARGET).top $(SYSLIBS) $(LIBS) $(OBJS)

clean::
	rm -f $(TARGET).byte $(TARGET).opt rm -f $(TARGET).top



lexer_c.ml: lexer_c.mll
	$(OCAMLLEX) $<
clean::
	rm -f lexer_c.ml
beforedepend:: lexer_c.ml


parser_c.ml parser_c.mli: parser_c.mly
	$(OCAMLYACC) $<
clean::
	rm -f parser_c.ml parser_c.mli parser_c.output
beforedepend:: parser_c.ml parser_c.mli








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
	rm -f *.cm[ioxa] *.o

clean::
	rm -f *~ .*~ gmon.out #*#

beforedepend::

depend:: beforedepend
	$(OCAMLDEP) *.mli *.ml    > .depend

-include .depend


