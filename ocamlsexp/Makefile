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
#SOURCES = \
#  parser.mly \
#  lexer.mll \
# sexp.mli path.mli conv.mli 


TARGET=sexplib1
TARGETPACK=sexplib
#LIB_PACK_NAME = sexplib

##############################################################################
# Generic variables
##############################################################################

INCLUDES=
#-I +camlp4
SYSLIBS= str.cma unix.cma bigarray.cma num.cma
#INCDIRS = +camlp4
#pad: take care for bigarray otherwise get some caml_ba_get_1 error msg

##############################################################################
#OCAMLFLAGS = -for-pack Sexplib

#PACKS = type-conv
#RESULT = sexplib

#TRASH = pa_sexp_conv.cmi pa_sexp_conv.cmo pa_sexp_conv.annot

#all: \
#	pack-byte-code pack-native-code \
#	sexplib.cma sexplib.cmxa \
#	pa_sexp_conv.cmi pa_sexp_conv.cmo
#
#LIBINSTALL_FILES = \
#  sexp.mli path.mli conv.mli \
#  sexplib.cmi sexplib.cma sexplib.cmxa sexplib.a \
#  pa_sexp_conv.cmi pa_sexp_conv.cmo
#
#install:	libinstall
#uninstall:	libuninstall
#
#clean::	clean-doc
#
#-include $(OCAMLMAKEFILE)

##############################################################################

##############################################################################
# Generic variables
##############################################################################

#dont use -custom, it makes the bytecode unportable.
OCAMLCFLAGS= -g -dtypes # -w A
#-for-pack Sexplib

# This flag is also used in subdirectories so don't change its name here.
OPTFLAGS=


OCAMLC=ocamlc$(OPTBIN) $(OCAMLCFLAGS)  $(INCLUDES)  $(SYSINCLUDES) -thread
OCAMLOPT=ocamlopt$(OPTBIN) $(OPTFLAGS) $(INCLUDES) $(SYSINCLUDES) -thread
OCAMLLEX=ocamllex #-ml # -ml for debugging lexer, but slightly slower
OCAMLYACC=ocamlyacc -v
OCAMLDEP=ocamldep $(INCLUDES)
OCAMLMKTOP=ocamlmktop -g -custom $(INCLUDES) -thread

#-ccopt -static
STATIC=


##############################################################################
# Top rules
##############################################################################

OBJS = $(SRC:.ml=.cmo)
OPTOBJS = $(SRC:.ml=.cmx)

all: $(TARGET).cma $(TARGETPACK).cmo
all.opt: $(TARGET).cmxa

$(TARGET).cma: $(OBJS)
	$(OCAMLC) -a -o $(TARGET).cma $(OBJS)

$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -a -o $(TARGET).cmxa $(OPTOBJS)

$(TARGET).top: $(OBJS) $(LIBS)
	$(OCAMLMKTOP) -o $(TARGET).top $(SYSLIBS) $(LIBS) $(OBJS)

clean::
	rm -f $(TARGET).top



$(TARGETPACK).cmo: $(OBJS)
	$(OCAMLC) -pack -o $(TARGETPACK).cmo $(OBJS)

# special handling, fun that they use cpp whereas they claim camplp4 can do everything
pre_sexp.cmo: pre_sexp.ml
	$(OCAMLC) -pp cpp -c $<

pre_sexp.cmx: pre_sexp.ml
	$(OCAMLOPT) -pp cpp -c $<



lexer.ml: lexer.mll
	$(OCAMLLEX) $<
clean::
	rm -f lexer.ml
beforedepend:: lexer.ml


parser.ml parser.mli: parser.mly
	$(OCAMLYACC) $<
clean::
	rm -f parser.ml parser.mli parser.output
beforedepend:: parser.ml parser.mli



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

# need also -pp cpp here
depend:: beforedepend
	$(OCAMLDEP) -pp cpp *.mli *.ml    > .depend

distclean: clean
	rm -f .depend

-include .depend
