include ../../Makefile.config

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
OCAMLCFLAGS ?=-g -dtypes
OPTFLAGS=

OCAMLC_CMD=$(OCAMLC) $(OCAMLCFLAGS) $(INCLUDES)
OCAMLOPT_CMD=$(OCAMLOPT) $(OPTFLAGS) $(INCLUDES)
OCAMLOPT_CMD2=$(OCAMLOPT) $(OPTFLAGS) $(EXTRAOPT) $(INCLUDES)
OCAMLDEP_CMD=$(OCAMLDEP) $(INCLUDESDEP)
OCAMLMKTOP_CMD=$(OCAMLMKTOP) -g -custom $(INCLUDES)

OBJS= $(SRC:.ml=.cmo)
OPTOBJS= $(SRC:.ml=.cmx)


##############################################################################
# Top rules
##############################################################################
all: $(TARGET).cma menhirLib.cmo
all.opt: $(TARGET).cmxa menhirLib.cmx
opt: all.opt

$(TARGET).cma: $(OBJS) $(LIBS)
	$(OCAMLC_CMD) -a -o $@ $(OBJS)

$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT_CMD2) -a -o $@ $(OPTOBJS)

# I thought at first that only one file menhirLib.ml
# was needed but in fact it's a wierd cos menhirLib.cmo results from multi
# files. They used the -pack ocamlc option, and for strange reason
# decided to produce a .cma instead of a classical .cma.
# So I put all the necesseray files in this directory.

# copy what the menhir authors do in their own makefile
menhirLib.cmo: $(OBJS)
	$(OCAMLC_CMD) -pack -o menhirLib.cmo $^

menhirLib.cmx: $(OPTOBJS)
	$(OCAMLOPT_CMD) -pack -o menhirLib.cmx $^

##############################################################################
# Generic rules
##############################################################################
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC_CMD) -c $<
.mli.cmi:
	$(OCAMLC_CMD) -c $<
.ml.cmx:
	$(OCAMLOPT_CMD2) -c $<

.ml.mldepend:
	$(OCAMLC_CMD) -i $<

clean::
	rm -f *.cm[ioxa] *.o *.a *.cmxa *.annot

clean::
	rm -f *~ .*~ gmon.out #*#
	rm -f .depend

distclean: clean

.PHONEY: depend
.depend depend:
	$(OCAMLDEP_CMD) *.mli *.ml > .depend

-include .depend
