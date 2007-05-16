##############################################################################
# Variables
##############################################################################
TARGET=spatch

SRC=flag.ml cocci.ml testing.ml test.ml main.ml

SYSLIBS=str.cma unix.cma
LIBS=commons/commons.cma \
     ctl/ctl.cma \
     parsing_cocci/cocci_parser.cma parsing_c/c_parser.cma \
     engine/cocciengine.cma popl/popl.cma

MAKESUBDIRS=commons ctl parsing_cocci parsing_c engine popl
INCLUDEDIRS=commons ctl parsing_cocci parsing_c engine popl

##############################################################################
# Generic variables
##############################################################################

INCLUDES=$(INCLUDEDIRS:%=-I %)

OBJS=    $(SRC:.ml=.cmo)
OPTOBJS= $(SRC:.ml=.cmx)

EXEC=$(TARGET)

##############################################################################
# Generic ocaml variables
##############################################################################

OCAMLCFLAGS=-g -dtypes -custom # -w A

# for profiling add  -p -inline 0
# but 'make forprofiling' below does that for you.
# This flag is also used in subdirectories so don't change its name here.
OPTFLAGS=

# the OPTBIN variable is here to allow to use ocamlc.opt instead of 
# ocaml, when it is available, which speeds up compilation. So
# if you want the fast version of the ocaml chain tools, set this var 
# or setenv it to ".opt" in your startup script.
OPTBIN= #.opt

OCAMLC=ocamlc$(OPTBIN) $(OCAMLCFLAGS)  $(INCLUDES)
OCAMLOPT=ocamlopt$(OPTBIN) $(OPTFLAGS) $(INCLUDES) 
OCAMLLEX=ocamllex #-ml # -ml for debugging lexer, but slightly slower
OCAMLYACC=ocamlyacc -v
OCAMLDEP=ocamldep $(INCLUDES)
OCAMLMKTOP=ocamlmktop -g -custom $(INCLUDES)


##############################################################################
# Top rules
##############################################################################

all: rec $(EXEC)
opt: rec.opt $(EXEC).opt

rec:
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i all; done 
rec.opt:
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i all.opt; done 

$(EXEC): $(LIBS) $(OBJS)
	$(OCAMLC) -o $@ $(SYSLIBS) $^

$(EXEC).opt: $(LIBS:.cma=.cmxa) $(OPTOBJS) 
	$(OCAMLOPT) -o $@ $(SYSLIBS:.cma=.cmxa) $^

$(EXEC).top: $(LIBS) $(OBJS) 
	$(OCAMLMKTOP) -o $@ $(SYSLIBS) $^

clean::
	rm -f $(TARGET) $(TARGET).opt $(TARGET).top

clean::
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i clean; done 


##############################################################################
# Developer rules
##############################################################################

test: $(TARGET)
	./$(TARGET) -testall

# -inline 0  to see all the functions in the profile.
forprofiling:
	$(MAKE) OPTFLAGS="-p -inline 0 " opt

clean::
	rm -f gmon.out 

tags:
	otags -no-mli-tags -r  .

##############################################################################
# Misc rules
##############################################################################

# each member of the project can have its own test.ml. this file is 
# not under CVS.
test.ml: 
	echo "let foo_ctl () = failwith \"there is no foo_ctl formula\"" \
	  > test.ml

beforedepend:: test.ml


#INC=$(dir $(shell which ocaml))
#INCX=$(INC:/=)
#INCY=$(dir $(INCX))
#INCZ=$(INCY:/=)/lib/ocaml
#
#prim.o: prim.c
#	gcc -c -o prim.o -I $(INCZ) prim.c


##############################################################################
# Generic ocaml rules
##############################################################################

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
	rm -f *.cm[iox] *.o *.annot

clean::
	rm -f *~ .*~ *.exe #*#

beforedepend::

depend:: beforedepend
	$(OCAMLDEP) *.mli *.ml > .depend
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i depend; done

-include .depend
