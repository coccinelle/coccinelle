TARGET=cocciengine
CTLTARGET=engine

SOURCES = flag_engine.ml lib_engine.ml pretty_print_engine.ml \
	  check_exhaustive_pattern.ml \
	  check_reachability.ml \
	  c_vs_c.ml isomorphisms_c_c.ml \
          cocci_vs_c_3.ml pattern3.ml sgrep.ml transformation3.ml  \
	  unparse_cocci.ml unparse_c.ml unparse_cocci2.ml unparse_c2.ml \
	  asttomember.ml asttoctl2.ml ctltotex.ml \
	  postprocess_transinfo.ml ctlcocci_integration.ml

# just to test asttoctl
# CTLSOURCES = lib_engine.ml pretty_print_engine.ml asttoctl.ml ctltotex.ml \
#	main.ml

INCLUDE_PATH = -I ../commons -I ../globals \
	       -I ../ctl -I ../parsing_c -I ../parsing_cocci

SYSLIBS = str.cma unix.cma
LIBS=../commons/commons.cma ../globals/globals.cma \
     ../ctl/ctl.cma ../parsing_c/c_parser.cma ../parsing_cocci/cocci_parser.cma


# The Caml compilers.
CAMLC =ocamlc$(OPTBIN) -g -dtypes 
CAMLOPT = ocamlopt$(OPTBIN) $(OPTFLAGS)
CAMLLEX = ocamllex$(OPTBIN)
CAMLYACC= ocamlyacc -v
CAMLDEP = ocamldep$(OPTBIN)

EXEC=$(TARGET).byte
LIB=$(TARGET).cma
OPTLIB=$(LIB:.cma=.cmxa)

CTLEXEC=$(CTLTARGET)

OBJS = $(SOURCES:.ml=.cmo)
OPTOBJS = $(OBJS:.cmo=.cmx)

CTLOBJS = $(CTLSOURCES:.ml=.cmo)
CTLOPTOBJS = $(CTLOBJS:.cmo=.cmx)


#all: $(EXEC) $(LIB)
all: $(LIB)

all.opt: $(OPTLIB)

ctl: $(CTLEXEC)


$(LIB): $(OBJS)
	$(CAMLC) -a -o $(LIB) $(OBJS)

# clean rule for LIB
clean::
	rm -f $(LIB)


$(OPTLIB): $(OPTOBJS) 
	$(CAMLOPT) -a -o $(OPTLIB) $(OPTOBJS)


$(EXEC): $(OBJS) main.cmo $(LIBS)
	$(CAMLC) -o $(EXEC) $(SYSLIBS) $(LIBS) $(OBJS) main.cmo

$(CTLEXEC): $(CTLOBJS) $(LIBS)
	$(CAMLC) -o $(CTLEXEC) $(SYSLIBS) $(LIBS) $(CTLOBJS)


# clean rule for LIB.opt
clean::
	rm -f $(OPTLIB) $(LIB:.cma=.a) 	
	rm -f $(TARGET) rm -f $(TARGET).byte
	rm -f $(CTLTARGET)


.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) $(INCLUDE_PATH) -c $<

.mli.cmi:
	$(CAMLC) $(INCLUDE_PATH) -c $<

.ml.cmx:
	$(CAMLOPT) $(INCLUDE_PATH) -c $<


# clean rule for others files
clean::
	rm -f *.cm[iox] *.o  *.annot
	rm -f *~ .*~ #*# 

beforedepend:

depend: beforedepend
	$(CAMLDEP) $(INCLUDE_PATH) *.mli *.ml > .depend

.depend: 
	$(CAMLDEP) $(INCLUDE_PATH) *.mli *.ml > .depend

-include .depend
