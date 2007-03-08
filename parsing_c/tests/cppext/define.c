#define  AVM_FRITZ_PCI		1
#define CFG_RESET_DELAY		5
#define CFG_NUM			6	/* number of configuration items */


#define chip_t vortex_t
#define VORTEX_PCM_TYPE(x) (x->name[40])

#define IRQ_T(info) ((info->flags & ASYNC_SHARE_IRQ) ? SA_SHIRQ : SA_INTERRUPT)

#define IRQ_T(info) ((info->flags & ASYNC_SHARE_IRQ) ? IRQF_SHARED : IRQF_DISABLED)

#define SONIC_IRQ_FLAG SA_INTERRUPT
#define SONIC_IRQ_FLAG IRQF_DISABLED

#define cs4x_mem_map_reserve(page) mem_map_reserve(page)
#define cs4x_mem_map_unreserve(page) mem_map_unreserve(page)

# define PCI_PRESENT pci_present ()
# define PCI_PRESENT (1)

#define pci_present pcibios_present


#define pci_present()				pcibios_present()


#define zr_remap_page_range(a,b,c,d,e) remap_page_range(a,b,c,d,e)

#define DEVICE_NR(device) (minor(device))
#define QUEUE (&sjcd_queue)


#define ARRAY_IS_ACTIVE KERN_INFO \
"multipath: array md%d active with %d out of %d IO paths (%d spare IO paths)\n"

#define RELEVANT_IFLAG(iflag)	(iflag & (IGNBRK|BRKINT|IGNPAR|PARMRK|INPCK|\
 					  IXON|IXOFF))

#define CS_EXIT_TEST(ret, svc, label) \
if (ret != CS_SUCCESS) { cs_error(link->handle, svc, ret); goto label; }

#define CS_EXIT_TEST(ret, svc, label) \
if (ret != CS_SUCCESS) { cs_error(link, svc, ret); goto label; }


#define CS_CHECK(fn, ret) \
 do { last_fn = (fn); if ((last_ret = (ret)) != 0) goto cs_failed; } while (0)

#define SET_TIMER							\
 	do {								\
 		mod_timer(&device_timer, jiffies + TIMEOUT_VALUE);	\
 	} while (0)

#define PciDeviceFn(d)		((d)&0xff)

#define __PciDev(busn, devfn)	(((busn)<<8)+(devfn))

#define ROCKET_PARANOIA_CHECK
#define ROCKET_SOFT_FLOW
#define ROCKET_DISABLE_SIMUSAGE

#define _INLINE_ inline

#define NUM_UPCI_IDS	(sizeof(upci_ids) / sizeof(upci_ids[0]))


#define pci_set_dma_mask(dev, mask)		dev->dma_mask = mask;

#define pci_find_subsys(id, dev, sid, sdev, pdev) pci_find_device(id,dev,pdev)
#define scsi_set_pci_device(host, pdev)

#define PCI_DEVICE_ID_TEKRAM_TRMS1040           0x0391	/* Device ID    */

#define DC395x_LOCK_IO(dev)   spin_lock_irqsave(((struct Scsi_Host *)dev)->host_lock, flags)


#define UCODE2(x) #x
 
static u32 __initdata ucode_start = 
#include UCODE(start)


#define SPRINTF(args...) \
       do { if(pos < buffer + length) pos += sprintf(pos, ## args); } while(0)


#define CS_CHECK(fn, args...) \
 while ((last_ret=CardServices(last_fn=(fn),args))!=0) goto cs_failed
#define CFG_CHECK(fn, args...) \
 if (CardServices(fn, args) != 0) goto next_entry



#define FOO1(s,a,b) \
	    while(nlen > 1) {		\
		    int ilen = p[1];	\
		    if(nlen < ilen+2) {	\
			    l3_debug(st, "FOO1  nlen < ilen+2"); \
			    return;		\
		    }			\
		    nlen -= ilen+2;		\
		    if((*p & 0xFF) == (a)) {	\
			    int nlen = ilen;	\
			    p += 2;		\
			    b;		\
		    } else {		\
			    p += ilen+2;	\
		    }			\
	    }

void main (int i)
{


				switch (ident) {
					case 0x22:	/* during */
						FOO1("1A", 0x30, FOO1("1C", 0xA1, FOO1("1D", 0x30, FOO1("1E", 0x02, ( {
							       ident = 0;
							nlen = (nlen)?nlen:0; /* Make gcc happy */
							while (ilen > 0) {
														     ident = (ident << 8) | *p++;
								  ilen--;
									}
														     if (ident > pc->para.chargeinfo) {
														     pc->para.chargeinfo = ident;
														     st->l3.l3l4(st, CC_CHARGE | INDICATION, pc);
									}
														     if (st->l3.debug & L3_DEB_CHARGE) {
														     if (*(p + 2) == 0) {
														     l3_debug(st, "charging info during %d", pc->para.chargeinfo);
									}
								   else {
														     l3_debug(st, "charging info final %d", pc->para.chargeinfo);
									}
									}
									}
								    )))))
!!!! 							break;
					case 0x24:	/* final */
						FOO1("2A", 0x30, FOO1("2B", 0x30, FOO1("2C", 0xA1, FOO1("2D", 0x30, FOO1("2E", 0x02, ( {
							       ident = 0;
							nlen = (nlen)?nlen:0; /* Make gcc happy */
							while (ilen > 0) {
																      ident = (ident << 8) | *p++;
								  ilen--;
									}
																      if (ident > pc->para.chargeinfo) {
																      pc->para.chargeinfo = ident;
																      st->l3.l3l4(st, CC_CHARGE | INDICATION, pc);
									}
																      if (st->l3.debug & L3_DEB_CHARGE) {
																      l3_debug(st, "charging info final %d", pc->para.chargeinfo);
									}
									}
								   ))))))
							break;
					default:
                                                       l3_debug(st, "invoke break invalid ident %02x",ident);
						break;
				}
#undef FOO1




/*
+static void (*do_hd)(void) = NULL;
 #define SET_HANDLER(x) \
-if ((DEVICE_INTR = (x)) != NULL) \
+if ((do_hd = (x)) != NULL) \
 	SET_TIMER; \
 else \
 	del_timer(&device_timer);
@@ -494,7 +497,7 @@
 {
 	unsigned int dev;
 
-	DEVICE_INTR = NULL;
+	do_hd = NULL;
 
 	if (blk_queue_empty(QUEUE))
 		return;
@@ -544,15 +547,16 @@
 {
 	unsigned int dev, block, nsect, sec, track, head, cyl;
 
-	if (DEVICE_INTR)
+	if (do_hd)
 		return;
 repeat:
 	del_timer(&device_timer);
 	sti();
 
-	if (blk_queue_empty(QUEUE)) 
+	if (blk_queue_empty(QUEUE)) {
+		do_hd = NULL;
 		return;
-	
+	}
 
 	if (reset) {
 		cli();
@@ -699,9 +703,9 @@
 	
 static void hd_interrupt(int irq, void *dev_id, struct pt_regs *regs)
 {
-	void (*handler)(void) = DEVICE_INTR;
+	void (*handler)(void) = do_hd;
 
-	DEVICE_INTR = NULL;
+	do_hd = NULL;
 	del_timer(&device_timer);
 	if (!handler)
 		handler = unexpected_hd_interrupt;


*/
