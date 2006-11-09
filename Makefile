TARGET=spatch

SRC = flag.ml cocci.ml test.ml main.ml


SYSLIBS = str.cma unix.cma
LIBS=commons/commons.cma ctl/ctl.cma \
     parsing_cocci/cocci_parser.cma parsing_c/c_parser.cma \
     engine/cocciengine.cma
CLIBS=prim.o

MAKESUBDIRS=commons ctl parsing_cocci parsing_c  engine
ADDONSPATH = -I commons -I ctl -I parsing_c -I parsing_cocci  -I engine


OCAMLRUNPARAM = 'b'
export OCAMLRUNPARAM

#for warning:  -w A 
#for profiling:  -p -inline 0   with OCAMLOPT
#pad: 'make forprofiling' below does that for you.

OCAMLC=ocamlc$(OPTBIN) -g   $(ADDONSPATH) -custom
OCAMLOPT=ocamlopt$(OPTBIN)   $(ADDONSPATH) $(OPTFLAGS)
OCAMLLEX=ocamllex$(OPTBIN) -ml
OCAMLYACC=ocamlyacc -v
OCAMLDEP=ocamldep$(OPTBIN)  $(ADDONSPATH)
OCAMLMKTOP=ocamlmktop -g -custom $(ADDONSPATH)


EXEC=$(TARGET)
OPTEXEC=$(TARGET).opt

OBJS = $(SRC:.ml=.cmo)
OPTOBJS = $(SRC:.ml=.cmx)


all: rec $(EXEC)
opt: rec.opt $(OPTEXEC)

test: $(TARGET)
	./$(TARGET) -testall

#can add -inline 0  to see all the functions in the profile.
forprofiling:
	$(MAKE) OPTFLAGS="-p " opt


rec:
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i all; done 

rec.opt:
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i all.opt; done 

$(EXEC): $(OBJS) $(LIBS)
	$(OCAMLC) -o $(EXEC) $(SYSLIBS) $(LIBS) $(OBJS)

$(TARGET).top: $(OBJS) $(LIBS) $(CLIBS)
	$(OCAMLMKTOP) -o $(TARGET).top $(SYSLIBS) $(LIBS) $(CLIBS) $(OBJS)

$(OPTEXEC): $(OPTOBJS) $(LIBS:.cma=.cmxa) $(CLIBS)
	$(OCAMLOPT) -o $(OPTEXEC) $(SYSLIBS:.cma=.cmxa) \
	  $(LIBS:.cma=.cmxa) $(CLIBS) $(OPTOBJS)


INC=$(dir $(shell which ocaml))
INCX=$(INC:/=)
INCY=$(dir $(INCX))
INCZ=$(INCY:/=)/lib/ocaml

prim.o: prim.c
	gcc -c -o prim.o -I $(INCZ) prim.c


clean::
	rm -f $(TARGET) $(TARGET).opt $(TARGET).top

clean::
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i clean; done 


test.ml: 
	echo "let foo_ctl () = failwith \"there is no foo_ctl formula\"" \
	  > test.ml

beforedepend:: test.ml



.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC)  -c $<
.mli.cmi:
	$(OCAMLC)  -c $<
.ml.cmx:
	$(OCAMLOPT)  -c $<

.ml.mldepend: 
	$(OCAMLC) -i $<


clean::
	rm -f *.cm[iox] *.o

clean::
	rm -f misc/*.cm[iox] *.o

clean::
	rm -f *~ .*~ gmon.out *.exe #*#

beforedepend::

depend:: beforedepend
	$(OCAMLDEP) *.mli *.ml misc/*.mli misc/*.ml > .depend
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i depend; done

-include .depend




