/* sun3x_esp.c:  EnhancedScsiProcessor Sun3x SCSI driver code.
 *
 * (C) 1999 Thomas Bogendoerfer (tsbogend@alpha.franken.de)
 *
 * Based on David S. Miller's esp driver
 */

#include <linux/kernel.h>
#include <linux/types.h>
#include <linux/string.h>
#include <linux/malloc.h>
#include <linux/blk.h>
#include <linux/proc_fs.h>
#include <linux/stat.h>
#include <linux/delay.h>

#include "scsi.h"
#include "hosts.h"
#include "NCR53C9x.h"

#include "sun3x_esp.h"
#include <asm/sun3x.h>
#include <asm/dvma.h>

extern struct NCR_ESP *espchain;

static void dma_barrier(struct NCR_ESP *esp);
static int  dma_bytes_sent(struct NCR_ESP *esp, int fifo_count);
static int  dma_can_transfer(struct NCR_ESP *esp, Scsi_Cmnd *sp);
static void dma_drain(struct NCR_ESP *esp);
static void dma_dump_state(struct NCR_ESP *esp);
static void dma_init_read(struct NCR_ESP *esp, __u32 vaddress, int length);
static void dma_init_write(struct NCR_ESP *esp, __u32 vaddress, int length);
static void dma_ints_off(struct NCR_ESP *esp);
static void dma_ints_on(struct NCR_ESP *esp);
static int  dma_irq_p(struct NCR_ESP *esp);
static void dma_poll(struct NCR_ESP *esp, unsigned char *vaddr);
static int  dma_ports_p(struct NCR_ESP *esp);
static void dma_reset(struct NCR_ESP *esp);
static void dma_setup(struct NCR_ESP *esp, __u32 addr, int count, int write);
static void dma_mmu_get_scsi_one (struct NCR_ESP *esp, Scsi_Cmnd *sp);
static void dma_mmu_get_scsi_sgl (struct NCR_ESP *esp, Scsi_Cmnd *sp);
static void dma_mmu_release_scsi_one (struct NCR_ESP *esp, Scsi_Cmnd *sp);
static void dma_mmu_release_scsi_sgl (struct NCR_ESP *esp, Scsi_Cmnd *sp);
static void dma_advance_sg (Scsi_Cmnd *sp);

volatile unsigned char cmd_buffer[16];
                                /* This is where all commands are put
                                 * before they are trasfered to the ESP chip
                                 * via PIO.
                                 */

/* Detecting ESP chips on the machine.  This is the simple and easy
 * version.
 */
int sun3x_esp_detect(Scsi_Host_Template *tpnt)
{
	struct NCR_ESP *esp;
	struct ConfigDev *esp_dev;

	esp_dev = 0;
	esp = esp_allocate(tpnt, (void *) esp_dev);

	/* Do command transfer with DMA */
	esp->do_pio_cmds = 0;

	/* Required functions */
	esp->dma_bytes_sent = &dma_bytes_sent;
	esp->dma_can_transfer = &dma_can_transfer;
	esp->dma_dump_state = &dma_dump_state;
	esp->dma_init_read = &dma_init_read;
	esp->dma_init_write = &dma_init_write;
	esp->dma_ints_off = &dma_ints_off;
	esp->dma_ints_on = &dma_ints_on;
	esp->dma_irq_p = &dma_irq_p;
	esp->dma_ports_p = &dma_ports_p;
	esp->dma_setup = &dma_setup;

	/* Optional functions */
	esp->dma_barrier = &dma_barrier;
	esp->dma_drain = &dma_drain;
	esp->dma_irq_entry = &dma_ints_off;
	esp->dma_irq_exit = &dma_ints_on;
	esp->dma_led_on = 0;
	esp->dma_led_off = 0;
	esp->dma_poll = &dma_poll;
	esp->dma_reset = &dma_reset;

        /* virtual DMA functions */
        esp->dma_mmu_get_scsi_one = &dma_mmu_get_scsi_one;
        esp->dma_mmu_get_scsi_sgl = &dma_mmu_get_scsi_sgl;
        esp->dma_mmu_release_scsi_one = &dma_mmu_release_scsi_one;
        esp->dma_mmu_release_scsi_sgl = &dma_mmu_release_scsi_sgl;
        esp->dma_advance_sg = &dma_advance_sg;
	    
	/* SCSI chip speed */
	esp->cfreq = 20000000;
	esp->eregs = (struct ESP_regs *)(SUN3X_ESP_BASE);
	esp->dregs = (void *)SUN3X_ESP_DMA;

	esp->esp_command = (volatile unsigned char *)cmd_buffer;
	esp->esp_command_dvma = dvma_alloc(virt_to_phys(cmd_buffer), 
					   sizeof (cmd_buffer));

	esp->irq = 2;
	request_irq(esp->irq, esp_intr, SA_INTERRUPT, "SUN3X SCSI", NULL);

	esp->scsi_id = 7;
	esp->diff = 0;

	esp_initialize(esp);

	printk("ESP: Total of %d ESP hosts found, %d actually in use.\n", nesps,
	       esps_in_use);
	esps_running = esps_in_use;
	return esps_in_use;
}

static void dma_barrier(struct NCR_ESP *esp)
{
	struct sparc_dma_registers *dregs =
		(struct sparc_dma_registers *) esp->dregs;

	while(dregs->cond_reg & DMA_PEND_READ)
		udelay(1);
	dregs->cond_reg &= ~(DMA_ENABLE);
}

/* This uses various DMA csr fields and the fifo flags count value to
 * determine how many bytes were successfully sent/received by the ESP.
 */
static int dma_bytes_sent(struct NCR_ESP *esp, int fifo_count)
{
	struct sparc_dma_registers *dregs = 
		(struct sparc_dma_registers *) esp->dregs;

	int rval = dregs->st_addr - esp->esp_command_dvma;

	return rval - fifo_count;
}

static int dma_can_transfer(struct NCR_ESP *esp, Scsi_Cmnd *sp)
{
	__u32 base, end, sz;

	base = ((__u32)sp->SCp.ptr);
	base &= (0x1000000 - 1);
	end = (base + sp->SCp.this_residual);
	if(end > 0x1000000)
		end = 0x1000000;
	sz = (end - base);
	return sz;
}

static void dma_drain(struct NCR_ESP *esp)
{
	struct sparc_dma_registers *dregs =
		(struct sparc_dma_registers *) esp->dregs;

	if(dregs->cond_reg & DMA_FIFO_ISDRAIN) {
		dregs->cond_reg |= DMA_FIFO_STDRAIN;
		while(dregs->cond_reg & DMA_FIFO_ISDRAIN)
			udelay(1);
	}
}

static void dma_dump_state(struct NCR_ESP *esp)
{
	struct sparc_dma_registers *dregs =
		(struct sparc_dma_registers *) esp->dregs;

	ESPLOG(("esp%d: dma -- cond_reg<%08lx> addr<%p>\n",
		esp->esp_id, dregs->cond_reg, dregs->st_addr));
}

static void dma_init_read(struct NCR_ESP *esp, __u32 vaddress, int length)
{
	struct sparc_dma_registers *dregs = 
		(struct sparc_dma_registers *) esp->dregs;

	dregs->cond_reg |= (DMA_ST_WRITE | DMA_ENABLE);
	dregs->st_addr = vaddress;
}

static void dma_init_write(struct NCR_ESP *esp, __u32 vaddress, int length)
{
	struct sparc_dma_registers *dregs = 
		(struct sparc_dma_registers *) esp->dregs;

	/* Set up the DMA counters */
	dregs->cond_reg = ((dregs->cond_reg & ~(DMA_ST_WRITE)) | DMA_ENABLE);
	dregs->st_addr = vaddress;
}

static void dma_ints_off(struct NCR_ESP *esp)
{
	DMA_INTSOFF((struct sparc_dma_registers *) esp->dregs);
}

static void dma_ints_on(struct NCR_ESP *esp)
{
	DMA_INTSON((struct sparc_dma_registers *) esp->dregs);
}

static int dma_irq_p(struct NCR_ESP *esp)
{
	return DMA_IRQ_P((struct sparc_dma_registers *) esp->dregs);
}

static void dma_poll(struct NCR_ESP *esp, unsigned char *vaddr)
{
	dma_drain(esp);

	/* Wait till the first bits settle. */
	while(vaddr[0] == 0xff)
		udelay(1);
}	

static int dma_ports_p(struct NCR_ESP *esp)
{
	return (((struct sparc_dma_registers *) esp->dregs)->cond_reg 
			& DMA_INT_ENAB);
}

/* Resetting various pieces of the ESP scsi driver chipset/buses. */
static void dma_reset(struct NCR_ESP *esp)
{
	struct sparc_dma_registers *dregs =
		(struct sparc_dma_registers *)esp->dregs;

	/* Punt the DVMA into a known state. */
	dregs->cond_reg |= DMA_RST_SCSI;
	dregs->cond_reg &= ~(DMA_RST_SCSI);
	DMA_INTSON(dregs);
}

static void dma_setup(struct NCR_ESP *esp, __u32 addr, int count, int write)
{
	struct sparc_dma_registers *dregs = 
		(struct sparc_dma_registers *) esp->dregs;
	unsigned long nreg = dregs->cond_reg;

	if(write)
		nreg |= DMA_ST_WRITE;
	else
		nreg &= ~(DMA_ST_WRITE);
	nreg |= DMA_ENABLE;
	dregs->cond_reg = nreg;
	dregs->st_addr = addr;
}

static void dma_mmu_get_scsi_one (struct NCR_ESP *esp, Scsi_Cmnd *sp)
{
    sp->SCp.have_data_in = dvma_alloc(virt_to_phys(sp->SCp.buffer),
				       sp->SCp.this_residual);
    sp->SCp.ptr = (char *)((unsigned long)sp->SCp.have_data_in);
}

static void dma_mmu_get_scsi_sgl (struct NCR_ESP *esp, Scsi_Cmnd *sp)
{
    int sz = sp->SCp.buffers_residual;
    struct mmu_sglist *sg = (struct mmu_sglist *) sp->SCp.buffer;

    while (sz >= 0) {
        sg[sz].dvma_addr = dvma_alloc(virt_to_phys(sg[sz].addr), sg[sz].len);
        sz--;
    }
    sp->SCp.ptr=(char *)((unsigned long)sp->SCp.buffer->dvma_address);
}

static void dma_mmu_release_scsi_one (struct NCR_ESP *esp, Scsi_Cmnd *sp)
{
    dvma_free(sp->SCp.have_data_in, sp->request_bufflen);
}

static void dma_mmu_release_scsi_sgl (struct NCR_ESP *esp, Scsi_Cmnd *sp)
{
    int sz = sp->use_sg - 1;
    struct mmu_sglist *sg = (struct mmu_sglist *)sp->buffer;
                        
    while(sz >= 0) {
        dvma_free(sg[sz].dvma_addr,sg[sz].len);
        sz--;
    }
}

static void dma_advance_sg (Scsi_Cmnd *sp)
{
    sp->SCp.ptr = (char *)((unsigned long)sp->SCp.buffer->dvma_address);
}

static Scsi_Host_Template driver_template = SCSI_SUN3X_ESP;

#include "scsi_module.c"
