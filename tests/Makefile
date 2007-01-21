MAKESUBDIRS=rule10 rule101 rule103 rule105 rule17 rule18 rule18a rule19 \
rule20 rule21 rule30 rule31 rule33 rule35 rule36 rule38 rule41 rule42 \
rule50 rule51 rule52 rule53 rule56 rule57 rule6 rule62 rule64 \
rule65 rule66 rule71 rule74 rule75 rule82 rule83 rule84 rule85 rule86 \
rule88 rule89 rule9 rule90 rule91 rule92 rule93 rule94 rule95 rule96 \
rule97 rule98 rule99 rule1 rule7 rule8

MAKESUBDIRS= rule86 \
rule88 rule89 rule9 rule90 rule91 rule92 rule93 rule94 rule95 rule96 \
rule97 rule98 rule99

#todo, get .c and .res for:
# rule64 rule65 

#no .c and .res because difficult to get them because ugly micro-patches in git
# rule37  rule48   
#no .cocci or makefile for this: 
# check_region  rule53 rule57 rule85   



all:
	time set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i; done 

clean:
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i clean; done 

clean2::
	find -name "*.ok" | xargs rm -f
	find -name "*.spatch_ok" | xargs rm -f
	find -name "*.failed" | xargs rm -f

score:
	find -name "*.ok" | wc -l
	find -name "*.spatch_ok" | xargs rm -f
	find -name "*ok" | wc -l
	find -name "*.failed" | wc -l

opt:	
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) SPATCH=../../spatch.opt -C $$i; done 
