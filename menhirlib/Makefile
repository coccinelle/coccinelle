##############################################################################
# Variables
##############################################################################
TARGET=menhirLib

SRC= infiniteArray.ml packedIntArray.ml rowDisplacement.ml engineTypes.ml \
  engine.ml tableFormat.ml tableInterpreter.ml convert.ml

LIBS=
INCLUDES= 

# copy what the menhir authors do
EXTRAOPT=-for-pack MenhirLib

##############################################################################
# Generic variables
##############################################################################
OCAMLCFLAGS=-g -dtypes
OPTFLAGS=

OCAMLC=ocamlc$(OPTBIN)     $(OCAMLCFLAGS) $(INCLUDES) 
OCAMLOPT= ocamlopt$(OPTBIN) $(OPTFLAGS) $(EXTRAOPT)   $(INCLUDES) 
OCAMLOPT2=ocamlopt$(OPTBIN) $(OPTFLAGS)              $(INCLUDES) 
OCAMLLEX=ocamllex$(OPTBIN)
OCAMLYACC=ocamlyacc -v
OCAMLDEP=ocamldep$(OPTBIN)  $(INCLUDES)
OCAMLMKTOP=ocamlmktop -g -custom $(INCLUDES)

OBJS= $(SRC:.ml=.cmo)
OPTOBJS= $(SRC:.ml=.cmx)


##############################################################################
# Top rules
##############################################################################
all: $(TARGET).cma menhirLib.cmo
all.opt: $(TARGET).cmxa menhirLib.cmx
opt: all.opt

$(TARGET).cma: $(OBJS) $(LIBS)
	$(OCAMLC) -a -o $@ $(OBJS)

$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -a -o $@ $(OPTOBJS)

# copy what the menhir authors do in their own makefile
menhirLib.cmo: $(OBJS)
	$(OCAMLC) -pack -o menhirLib.cmo $^

menhirLib.cmx: $(OPTOBJS)
	$(OCAMLOPT2) -pack -o menhirLib.cmx $^

##############################################################################
# Generic rules
##############################################################################
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
