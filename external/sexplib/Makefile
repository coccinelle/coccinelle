include ../../Makefile.config

##############################################################################
# Variables
##############################################################################

SRC= \
  type.ml \
  parser.ml \
  lexer.ml \
  pre_sexp.ml \
  sexp_intf.ml \
  sexp.ml \
  path.ml \
  conv.ml \
  conv_error.ml

TARGET=sexplib
TARGETPACK=sexplib
LIB_PACK_NAME = Sexplib

##############################################################################
# Generic variables
##############################################################################

INCLUDES=
SYSLIBS= str.cma unix.cma bigarray.cma num.cma

##############################################################################
# Generic variables
##############################################################################

#dont use -custom, it makes the bytecode unportable.
OCAMLCFLAGS ?= -g -dtypes # -w A

# This flag is also used in subdirectories so don't change its name here.
OPTFLAGS=

OCAMLC_CMD=$(OCAMLC) $(OCAMLCFLAGS) $(INCLUDES) $(SYSINCLUDES) -thread
OCAMLOPT_CMD=$(OCAMLOPT) $(OPTFLAGS) $(INCLUDES) $(SYSINCLUDES) -thread
OCAMLDEP_CMD=$(OCAMLDEP) $(INCLUDES)
OCAMLMKTOP_CMD=$(OCAMLMKTOP) -g -custom $(INCLUDES) -thread

##############################################################################
# Top rules
##############################################################################

OBJS = $(SRC:.ml=.cmo)
OPTOBJS = $(SRC:.ml=.cmx)

all: $(TARGET).cma $(TARGETPACK).cmo
all.opt: $(TARGET).cmxa $(TARGETPACK).cmx

$(TARGET).cma: $(OBJS)
	$(OCAMLC_CMD) -a -o $@ $(OBJS)

$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT_CMD) -a -o $@ $(OPTOBJS)

$(TARGET).top: $(OBJS) $(LIBS)
	$(OCAMLMKTOP_CMD) -o $(TARGET).top $(SYSLIBS) $(LIBS) $(OBJS)

clean::
	rm -f $(TARGET).top

$(TARGETPACK).cmo: $(OBJS)
	$(OCAMLC_CMD) -pack -o $@ $(OBJS)

$(TARGETPACK).cmx: $(OPTOBJS)
	$(OCAMLOPT_CMD) -pack -o $@ $(OPTOBJS)

# special handling, fun that they use cpp whereas they claim camplp4 can do everything
pre_sexp.cmo: pre_sexp.ml
	$(OCAMLC_CMD) -pp "${CPP} -x c -w" -c $<

pre_sexp.cmx: pre_sexp.ml
	$(OCAMLOPT_CMD) -for-pack $(LIB_PACK_NAME) -pp "${CPP} -x c -w" -c $<

lexer.ml: lexer.mll
	$(OCAMLLEX) $<
clean::
	rm -f lexer.ml


parser.ml parser.mli: parser.mly
	$(OCAMLYACC) -v $<
clean::
	rm -f parser.ml parser.mli parser.output


##############################################################################
# Generic rules
##############################################################################

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC_CMD) -c $<
.mli.cmi:
	$(OCAMLC_CMD) -c $<

.ml.cmx:
	$(OCAMLOPT_CMD) -for-pack $(LIB_PACK_NAME) -c $<

.ml.mldepend:
	$(OCAMLC_CMD) -i $<

clean::
	rm -f *.cm[ioxa] *.o *.a *.cmxa *.annot
clean::
	rm -f *~ .*~ gmon.out #*#

clean::
	rm -f .depend

.PHONEY: depend
.depend depend: parser.ml parser.mli lexer.ml
	$(OCAMLDEP_CMD) -pp "${CPP} -x c -w" *.mli *.ml > .depend

distclean: clean

-include .depend
