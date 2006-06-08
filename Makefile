TARGET=maxicoccinelle

SOURCEMAIN = \
	flag.ml  \
	misc/classic_patch.ml  \
	cocci.ml   test.ml main.ml

EXEC=$(TARGET).byte
OPTEXEC=$(EXEC).opt

OBJS = $(SOURCEMAIN:.ml=.cmo)
OPTOBJS = $(SOURCEMAIN:.ml=.cmx)

SYSLIBS = str.cma unix.cma
LIBS=commons/commons.cma ctl/ctl.cma parsing_cocci/cocci_parser.cma parsing_c/c_parser.cma engine/cocciengine.cma
MAKESUBDIRS=commons ctl parsing_cocci parsing_c  engine
ADDONSPATH = -I commons -I ctl -I parsing_c -I parsing_cocci  -I engine -I misc

#ocamlc -I ../commons -I ../ctl -I ../parsing_c -I ../parsing_cocci  -I ../engine -I ../misc

OCAMLRUNPARAM = 'b'
export OCAMLRUNPARAM

#for warning:  -w A 
#for profiling:  -p -inline 0   with OCAMLOPT
OCAMLC=ocamlc -g   $(ADDONSPATH)
OCAMLOPT=ocamlopt   $(ADDONSPATH) 
OCAMLLEX=ocamllex -ml
OCAMLYACC=ocamlyacc -v
OCAMLDEP=ocamldep  $(ADDONSPATH)
OCAMLMKTOP=ocamlmktop -g -custom $(ADDONSPATH)

all: $(EXEC) $(TARGET).top
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
	rm -f $(TARGET) $(TARGET).opt


clean::
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i clean; done 



.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(ADDONSPATH) -c $<
.mli.cmi:
	$(OCAMLC) $(ADDONSPATH) -c $<
.ml.cmx:
	$(OCAMLOPT) $(ADDONSPATH) -c $<

.ml.mldepend: 
	$(OCAMLC) $(ADDONSPATH) -i $<

clean::
	rm -f *.cm[iox] *.o

clean::
	rm -f misc/*.cm[iox] *.o

clean::
	rm -f *~ .*~ gmon.out #*#

beforedepend:

depend:: beforedepend
	$(OCAMLDEP) *.mli *.ml > .depend
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i depend; done

include .depend
