TARGET=spatch

SRC = \
	flag.ml  \
	misc/classic_patch.ml  \
	cocci.ml main.ml

SYSLIBS = str.cma unix.cma
LIBS=commons/commons.cma ctl/ctl.cma parsing_cocci/cocci_parser.cma parsing_c/c_parser.cma engine/cocciengine.cma
MAKESUBDIRS=commons ctl parsing_cocci parsing_c  engine
ADDONSPATH = -I commons -I ctl -I parsing_c -I parsing_cocci  -I engine -I misc


OCAMLRUNPARAM = 'b'
export OCAMLRUNPARAM

#for warning:  -w A 
#for profiling:  -p -inline 0   with OCAMLOPT
OPTFLAGS=

OCAMLC=ocamlc -g   $(ADDONSPATH)
OCAMLOPT=ocamlopt   $(ADDONSPATH) $(OPTFLAGS)
OCAMLLEX=ocamllex -ml
OCAMLYACC=ocamlyacc -v
OCAMLDEP=ocamldep  $(ADDONSPATH)
OCAMLMKTOP=ocamlmktop -g -custom $(ADDONSPATH)


EXEC=$(TARGET)
OPTEXEC=$(EXEC).opt

OBJS = $(SRC:.ml=.cmo)
OPTOBJS = $(SRC:.ml=.cmx)


ifeq ($(PAD),pad)
all: mystuff
else
all: all1
endif

all1: rec $(EXEC)
opt: rec.opt $(OPTEXEC)


rec:
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i all; done 

rec.opt:
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i all.opt; done 

$(EXEC): $(OBJS) $(LIBS)
	$(OCAMLC) -o $(EXEC) $(SYSLIBS) $(LIBS) $(OBJS)

$(TARGET).top: $(OBJS) $(LIBS)
	$(OCAMLMKTOP) -o $(TARGET).top $(SYSLIBS) $(LIBS) $(OBJS)

$(OPTEXEC): $(OPTOBJS) $(OPTLIBS)
	$(OCAMLOPT) -o $(OPTEXEC) $(SYSLIBS:.cma=.cmxa) $(LIBS:.cma=.cmxa) $(OPTOBJS)



clean::
	rm -f $(TARGET) $(TARGET).opt $(TARGET).top


clean::
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i clean; done 




MYSRC = flag.ml  \
	misc/classic_patch.ml  \
	cocci.ml   mytest.ml mymain.ml
MYOBJS = $(MYSRC:.ml=.cmo)
MYOPTOBJS = $(MYSRC:.ml=.cmx)
MYEXEC = myspatch

mystuff: rec $(MYEXEC) $(MYEXEC).top

$(MYEXEC): $(MYOBJS) $(LIBS)
	$(OCAMLC) -o $(MYEXEC) $(SYSLIBS) $(LIBS) $(MYOBJS)

$(MYEXEC).top: $(MYOBJS) $(LIBS)
	$(OCAMLMKTOP) -o $(MYEXEC).top $(SYSLIBS) $(LIBS) $(MYOBJS)

$(MYEXEC).opt: $(MYOPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -o $(MYEXEC).opt  $(SYSLIBS:.cma=.cmxa) $(LIBS:.cma=.cmxa) $(MYOPTOBJS)

clean::
	rm -f $(MYEXEC) $(MYEXEC).top $(MYEXEC).opt


forprofiling:
	$(MAKE) OPTFLAGS="-p" opt
	$(MAKE) OPTFLAGS="-p" myspatch.opt


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
	rm -f *~ .*~ gmon.out #*#

beforedepend:

depend:: beforedepend
	$(OCAMLDEP) *.mli *.ml misc/*.mli misc/*.ml > .depend
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i depend; done


-include .depend
