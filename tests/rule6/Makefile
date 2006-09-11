SP=mem_map.cocci

# normally cocci should detect a bug with cs46xx.c
# the final good result for this file is in 1_cs46xx.res

# if you also want to handle the removal of the #include, then 
# extract the .c and /res from the second micro-patch. In that case
# there will be duplicate c files, and some c files will be identical to some res files.

# a7fe32f7a98f83218f85b2196fed0485f1d5dc22
#  main
# 0d3db480b649edffd4c0b43e956d1e6be5816a2f
#  remove #include, remove wrapper.h (E)
#  and also do a CE in a a .h (drm_memory.h)
# fb346e7bf88ec59e6da20fe6a7813e650b19d1d7
#  bugfix by morton


SOURCES = audio.c \
	  cmpci.c \
	  cpia.c \
	  cs4281_wrapper-24.c \
	  cs4281m.c \
	  cs46xx.c \
	  dmabuf.c \
	  es1370.c \
	  es1371.c \
	  esssolo1.c \
	  ftape-buffer.c \
	  i810_audio.c \
	  ite8172.c \
	  maestro.c \
	  maestro3.c \
	  meye.c \
	  ov511.c \
	  planb.c \
	  pwc-if.c \
	  se401.c \
	  sonicvibes.c \
	  sscape.c \
	  stv680.c \
	  trident.c \
	  usbvideo.c \
	  vicam.c \
	  zr36067.c \
	  zr36120_mem.c 


include ../../generic_makefile
