# Don't forget to first do a 'make links' to be able to compile the standalone c parser

TARGET=c_parser

SOURCEMAIN = flag.ml ast_c.ml semantic_c.ml visitor_c.ml control_flow_c.ml \
	    lexer_parser.ml parser_c.ml lexer_c.ml parse_c.ml   unparse_c.ml transformation.ml main.ml

SYSLIBS = str.cma unix.cma threads.cma
LIBS=../commons/commons.cma
COMMONDIR=../commons
SUBDIRS=../commons


ADDONSPATH = -I $(COMMONDIR) 

OCAMLRUNPARAM = 'b'
export OCAMLRUNPARAM

# -w A ?
OCAMLC=ocamlc -g  $(PP) $(ADDONSPATH) -thread
OCAMLOPT=ocamlopt  $(PP) $(ADDONSPATH) -thread -p -inline 0
OCAMLLEX=ocamllex -ml
OCAMLYACC=ocamlyacc -v
OCAMLDEP=ocamldep $(PP) $(ADDONSPATH)
OCAMLMKTOP=ocamlmktop -g -custom $(PP) -thread $(ADDONSPATH)


OBJS = $(SOURCEMAIN:.ml=.cmo)
OPTOBJS = $(SOURCEMAIN:.ml=.cmx)



all: $(TARGET) $(TARGET).cma
#$(TARGET).top
#$(TARGET).opt

$(TARGET): $(OBJS) $(LIBS)
	$(OCAMLC) -o $(TARGET) $(SYSLIBS) $(LIBS) $(OBJS)

$(TARGET).opt: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -o $(TARGET).opt   $(SYSLIBS:.cma=.cmxa) $(LIBS:.cma=.cmxa)  $(OPTOBJS)

$(TARGET).cma: $(OBJS) $(LIBS)
	$(OCAMLC) -a -o $(TARGET).cma $(OBJS)

clean::
	rm -f $(TARGET) $(TARGET).opt


$(TARGET).top: $(OBJS) $(LIBS)
	$(OCAMLMKTOP) -o $(TARGET).top $(SYSLIBS) $(LIBS) $(OBJS)
clean:: 
	rm -f $(TARGET).top

links:
	ln -s ../flag.ml
#	ln -s ../ast_c.ml
#	ln -s ../visitor_c.ml
#	ln -s ../commons

dellinks:
	rm -f flag.ml 
#ast_c.ml visitor_c.ml commons

../commons/commons.cmxa:
	cd commons; $(MAKE) commons.cmxa

commons/%: 
	cd commons; $(MAKE)



clean::
	set -e; for i in $(SUBDIRS); do $(MAKE) -C $$i clean; done 

clean::
	rm -f *.cm[iox] *.o



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
	rm -f *.cm[iox] *.o

clean::
	rm -f *~ .*~ gmon.out #*#

beforedepend::

depend:: beforedepend
	$(OCAMLDEP) *.mli *.ml    > .depend

include .depend


