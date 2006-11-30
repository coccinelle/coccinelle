MAKESUBDIRS=devfs video_usercopy usb_submit_urb \
 rule10 rule17 rule18 rule18a rule19 rule30 rule31 rule33 rule36 rule38 \
 rule42   rule6 rule74 rule82 rule83  rule9 \
 rule21 rule62 rule35 rule66 rule75   

#todo, get .c and .res for:
# rule64 rule65 

#no .c and .res because difficult to get them because ugly micro-patches in git
# rule37  rule48   
#no .cocci or makefile for this: 
# check_region  rule53 rule57 rule85   



all:
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i; done 

clean:
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i clean; done 

clean2::
	find -name "*.ok" | xargs rm -f
	find -name "*.failed" | xargs rm -f

score:
	find -name "*.ok" | wc -l
	find -name "*.failed" | wc -l

opt:	
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) SPATCH=../../spatch.opt -C $$i; done 
