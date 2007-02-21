struct ep_attribute {
	struct attribute attr;
 	ssize_t (*show)(struct usb_device *,
			struct usb_endpoint_descriptor *, char *);
};


irqreturn_t (*interrupt)(int, void *, struct pt_regs *);


//HANDLING: /tmp/linux-2.6.13/drivers/base/map.c
//EXECUTING: cpp -nostdinc -isystem /usr/lib/gcc-lib/i486-slackware-linux/3.3.4/include -D__GNUC__=3 -D__KERNEL__ -I/tmp/linux-2.6.13/include -I/tmp/linux-2.6.13/include/asm-i386/mach-default -I/tmp/linux-2.6.13/include/asm-ppc/    -I/tmp/linux-2.6.13/drivers/base -DMODULE -DKBUILD_BASENAME=gscd -DKBUILD_MODNAME=gscd  /tmp/linux-2.6.13/drivers/base/map.c> /tmp/main_ml-1.c
//parse error=File "/tmp/main_ml-2.c", line 7718, characters 37-42:
//    around = dev_t, whole content =              kobj_probe_t *, int (*)(dev_t, void *), void *);
// charpos = 206325

int f(kobj_probe_t *, int (*)(dev_t, void *), void *);
