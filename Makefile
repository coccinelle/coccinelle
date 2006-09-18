TARGET=spatch

SRC = flag.ml cocci.ml test.ml main.ml

SYSLIBS = str.cma unix.cma
LIBS=commons/commons.cma ctl/ctl.cma parsing_cocci/cocci_parser.cma \
	parsing_c/c_parser.cma engine/cocciengine.cma
MAKESUBDIRS=commons ctl parsing_cocci parsing_c  engine
TESTSUBDIRS=tests
ADDONSPATH = -I commons -I ctl -I parsing_c -I parsing_cocci  -I engine -I misc


OCAMLRUNPARAM = 'b'
export OCAMLRUNPARAM

#for warning:  -w A 
#for profiling:  -p -inline 0   with OCAMLOPT
#OPTFLAGS=-p -inline 0
#pad: but no need to uncomment the line, 'make forprofiling' does that for you.

OCAMLC=ocamlc$(OPTBIN) -g   $(ADDONSPATH)
OCAMLOPT=ocamlopt$(OPTBIN)   $(ADDONSPATH) $(OPTFLAGS)
OCAMLLEX=ocamllex$(OPTBIN) -ml
OCAMLYACC=ocamlyacc -v
OCAMLDEP=ocamldep$(OPTBIN)  $(ADDONSPATH)
OCAMLMKTOP=ocamlmktop -g -custom $(ADDONSPATH)


EXEC=$(TARGET)
OPTEXEC=$(TARGET).opt

OBJS = $(SRC:.ml=.cmo)
OPTOBJS = $(SRC:.ml=.cmx)

ifeq ($(PAD),pad)
all: mystuff all1
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
	$(OCAMLOPT) -o $(OPTEXEC) $(SYSLIBS:.cma=.cmxa) \
	  $(LIBS:.cma=.cmxa) $(OPTOBJS)



clean::
	rm -f $(TARGET) $(TARGET).opt $(TARGET).top


clean::
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i clean; done 


test.ml: 
	echo "let foo_ctl () = failwith \"there is no foo_ctl formula\"" \
	  > test.ml

beforedepend:: test.ml


MYSRC = flag.ml  \
	misc/classic_patch.ml  \
	cocci.ml   mytest.ml mymain.ml
MYOBJS = $(MYSRC:.ml=.cmo)
MYOPTOBJS = $(MYSRC:.ml=.cmx)
MYEXEC = myspatch

mystuff: rec $(MYEXEC) 
#$(MYEXEC).top

$(MYEXEC): $(MYOBJS) $(LIBS)
	$(OCAMLC) -o $(MYEXEC) $(SYSLIBS) $(LIBS) $(MYOBJS)

$(MYEXEC).top: $(MYOBJS) $(LIBS)
	$(OCAMLMKTOP) -o $(MYEXEC).top $(SYSLIBS) $(LIBS) $(MYOBJS)

$(MYEXEC).opt: $(MYOPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -o $(MYEXEC).opt  $(SYSLIBS:.cma=.cmxa) \
	   $(LIBS:.cma=.cmxa) $(MYOPTOBJS)

clean::
	rm -f $(MYEXEC) $(MYEXEC).top $(MYEXEC).opt



#can add -inline 0  to see all the functions in the profile.
forprofiling:
	$(MAKE) OPTFLAGS="-p " opt


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

# Regression testing
tests:  spatch
	set -e; for i in $(TESTSUBDIRS); do $(MAKE) -C $$i all; done 


# Benchs
%.ruledir: spatch
	cd tests/$(@:.ruledir=); $(MAKE) SPATCH=../../spatch.opt

HARD=devfs video_usercopy     
# check_region 

MIDDLE=
#rule9 rule6 rule18 rule18a rule19(big?)  rule74 rule83 (pad-bugs?)


#easy: 10, 17, 30(lots of pad-bugs), 31, 33, 36?, 37(ugly), 38, 42, 48?, 53, 57, 82!, 85

#rule10 rule17 rule30 rule31 rule33 rule38  rule42    
#no cocci yet: rule53 rule57 rule82 rule85 

#EUROSYS= rule9 rule18 rule18a rule19 rule83 video_usercopy
EUROSYS=$(HARD) $(MIDDLE) $(EASY)

eurosys: $(EUROSYS:=.ruledir)
