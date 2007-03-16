CEDESCRIPTION="remove pci_pool_create sixth argument"
SP=rule51.cocci

SOURCES = amdtp.c \
buffer.c \
ehci-mem.c \
ohci-mem.c \
uhci-hcd.c

include ../generic_makefile

# pool.c, which was in the git file, is not included because this is the
# library file