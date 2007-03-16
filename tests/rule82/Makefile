CEDESCRIPTION="remove argument pci_save/restore_state"
#
# Make file for rule 82
#

# Semantic Patch file
SP=rule82.cocci

# The C files
SOURCES = 3c59x.c           \
	  8139cp.c	    \
	  8139too.c	    \
	  airo.c	    \
	  ali5455.c	    \
	  amd8111e.c	    \
	  b44.c		    \
	  bttv-driver.c	    \
	  cx88-video.c	    \
	  e1000_main.c	    \
	  e100.c	    \
	  eepro100.c	    \
	  hcd-pci.c	    \
	  i810_audio.c	    \
	  i810_main.c	    \
	  init.c	    \
	  intel-agp.c	    \
	  intel-mch-agp.c   \
	  ipr.c		    \
	  islpci_hotplug.c  \
	  ixgb_main.c	    \
	  meye.c	    \
	  mptbase.c	    \
	  nsp32.c	    \
	  pci-driver.c	    \
	  pci-skeleton.c    \
	  s2io.c	    \
	  sis900.c	    \
	  tg3.c		    \
	  typhoon.c	    \
	  via-rhine.c	    \
	  via-velocity.c    \
	  vlsi_ir.c	    \
	  xircom_tulip_cb.c \
	  yenta_socket.c    \
          bugfix_files/ne2k-pci.c \
	  bugfix_files/via-agp.c



#
include ../generic_makefile

clean::
	rm -f bugfix_files/*.ok bugfix_files/.*failed
