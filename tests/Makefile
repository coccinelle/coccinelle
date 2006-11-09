MAKESUBDIRS=devfs video_usercopy usb_submit_urb \
 rule10 rule17 rule18 rule18a rule19 rule30 rule31 rule33 rule36 rule38 \
 rule42   rule6 rule74 rule82 rule83  rule9 

# no .cocci nor makefile for this: rule53 rule57 rule85   check_region
# 

#MAKESUBDIRS=usb_submit_urb

all:
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i; done 

clean:
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i clean; done 


score:
	find -name "*.ok" | wc -l
	find -name "*.failed" | wc -l

