
TARGET=globals

SOURCES = flag.ml config.ml

LIBS=../commons/commons.cma
ADDONSPATH= -I ../commons

#for warning:  -w A 
#for profiling:  -p -inline 0   with OCAMLOPT
OCAMLC=ocamlc$(OPTBIN) -g -dtypes   $(ADDONSPATH) 
OCAMLOPT=ocamlopt$(OPTBIN)   $(ADDONSPATH) $(OPTFLAGS)
#OCAMLLEX=ocamllex$(OPTBIN) -ml
OCAMLLEX=ocamllex$(OPTBIN)
OCAMLYACC=ocamlyacc -v
OCAMLDEP=ocamldep$(OPTBIN)  $(ADDONSPATH)
OCAMLMKTOP=ocamlmktop -g -custom $(ADDONSPATH)


OBJS = $(SOURCES:.ml=.cmo)
OPTOBJS = $(SOURCES:.ml=.cmx)


all: $(TARGET).cma

all.opt: $(TARGET).cmxa


$(TARGET).cma: $(OBJS) $(LIBS)
	$(OCAMLC) -a -o $(TARGET).cma $(OBJS)

$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -a -o $(TARGET).cmxa $(OPTOBJS)




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
	rm -f *.cm[ioxa] *.o *.a *.cmxa *.annot

clean::
	rm -f *~ .*~ gmon.out #*#

beforedepend::

depend:: beforedepend
	$(OCAMLDEP) *.mli *.ml    > .depend

-include .depend




