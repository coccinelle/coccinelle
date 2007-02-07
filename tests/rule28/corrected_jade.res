/* $Id: corrected_jade.res,v 1.1 2007-02-07 18:52:00 julia Exp $
 *
 * JADE stuff (derived from original hscx.c)
 *
 * Author       Roland Klabunde
 * Copyright    by Roland Klabunde   <R.Klabunde@Berkom.de>
 * 
 * This software may be used and distributed according to the terms
 * of the GNU General Public License, incorporated herein by reference.
 *
 */


#include <linux/init.h>
#include "hisax.h"
#include "hscx.h"
#include "jade.h"
#include "isdnl1.h"
#include <linux/interrupt.h>

static spinlock_t jade_lock = SPIN_LOCK_UNLOCKED;

static inline u8
jade_read_reg(struct IsdnCardState *cs, int jade, u8 addr)
{
	return cs->bc_hw_ops->read_reg(cs, jade, addr);
}

static inline void
jade_write_reg(struct IsdnCardState *cs, int jade, u8 addr, u8 val)
{
	cs->bc_hw_ops->write_reg(cs, jade, addr, val);
}

static inline void
jade_write_fifo(struct BCState *bcs, u8 *p, int len)
{
	struct IsdnCardState *cs = bcs->cs;

	cs->bc_hw_ops->write_fifo(cs, bcs->unit, p, len);
}

int __init
JadeVersion(struct IsdnCardState *cs, char *s)
{
    int ver,i;
    int to = 50;
    jade_write_reg(cs, -1, 0x50, 0x19);
    i=0;
    while (to) {
    	udelay(1);
	ver = jade_read_reg(cs, -1, 0x60);
	to--;
	if (ver)
    	    break;
	if (!to) {
	    printk(KERN_INFO "%s JADE version not obtainable\n", s);
    	    return (0);
        }
    }
    /* Wait for the JADE */
    udelay(10);
    /* Read version */
    ver = jade_read_reg(cs, -1, 0x60);
    printk(KERN_INFO "%s JADE version: %d\n", s, ver);
    return (1);
}

/* Write to indirect accessible jade register set */
static void
jade_write_indirect(struct IsdnCardState *cs, u8 reg, u8 value)
{
    int to = 50;
    unsigned long flags;
    u8 ret;
    spin_lock_irqsave(&jade_lock, flags);
    /* Write the data */
    jade_write_reg(cs, -1, COMM_JADE+1, value);
    /* Say JADE we wanna write indirect reg 'reg' */
    jade_write_reg(cs, -1, COMM_JADE, reg);
    to = 50;
    /* Wait for RDY goes high */
    while (to) {
    	udelay(1);
	ret = jade_read_reg(cs, -1, COMM_JADE);
	to--;
	if (ret & 1)
	    /* Got acknowledge */
	    break;
	if (!to) {
	    spin_unlock_irqrestore(&jade_lock, flags);
    	    printk(KERN_INFO "Can not see ready bit from JADE DSP (reg=0x%X, value=0x%X)\n", reg, value);
	    return;
	}
    }
    spin_unlock_irqrestore(&jade_lock, flags);
}



void
modejade(struct BCState *bcs, int mode, int bc)
{
    struct IsdnCardState *cs = bcs->cs;
    int jade = bcs->unit;

    if (cs->debug & L1_DEB_HSCX) {
	char tmp[40];
	sprintf(tmp, "jade %c mode %d ichan %d",
		'A' + jade, mode, bc);
	debugl1(cs, tmp);
    }
    bcs->mode = mode;
    bcs->channel = bc;
	
    jade_write_reg(cs, jade, jade_HDLC_MODE, (mode == L1_MODE_TRANS ? jadeMODE_TMO:0x00));
    jade_write_reg(cs, jade, jade_HDLC_CCR0, (jadeCCR0_PU|jadeCCR0_ITF));
    jade_write_reg(cs, jade, jade_HDLC_CCR1, 0x00);

    jade_write_indirect(cs, jade_HDLC1SERRXPATH, 0x08);
    jade_write_indirect(cs, jade_HDLC2SERRXPATH, 0x08);
    jade_write_indirect(cs, jade_HDLC1SERTXPATH, 0x00);
    jade_write_indirect(cs, jade_HDLC2SERTXPATH, 0x00);

    jade_write_reg(cs, jade, jade_HDLC_XCCR, 0x07);
    jade_write_reg(cs, jade, jade_HDLC_RCCR, 0x07);

    if (bc == 0) {
	jade_write_reg(cs, jade, jade_HDLC_TSAX, 0x00);
	jade_write_reg(cs, jade, jade_HDLC_TSAR, 0x00);
    } else {
	jade_write_reg(cs, jade, jade_HDLC_TSAX, 0x04);
	jade_write_reg(cs, jade, jade_HDLC_TSAR, 0x04);
    }
    switch (mode) {
	case (L1_MODE_NULL):
		jade_write_reg(cs, jade, jade_HDLC_MODE, jadeMODE_TMO);
		break;
	case (L1_MODE_TRANS):
		jade_write_reg(cs, jade, jade_HDLC_MODE, (jadeMODE_TMO|jadeMODE_RAC|jadeMODE_XAC));
		break;
	case (L1_MODE_HDLC):
		jade_write_reg(cs, jade, jade_HDLC_MODE, (jadeMODE_RAC|jadeMODE_XAC));
		break;
    }
    if (mode) {
	jade_write_reg(cs, jade, jade_HDLC_RCMD, (jadeRCMD_RRES|jadeRCMD_RMC));
	jade_write_reg(cs, jade, jade_HDLC_XCMD, jadeXCMD_XRES);
	/* Unmask ints */
	jade_write_reg(cs, jade, jade_HDLC_IMR, 0xF8);
    }
    else
	/* Mask ints */
	jade_write_reg(cs, jade, jade_HDLC_IMR, 0x00);
}

static void
jade_l2l1(struct PStack *st, int pr, void *arg)
{
    struct sk_buff *skb = arg;

    switch (pr) {
	case (PH_DATA | REQUEST):
		xmit_data_req_b(st->l1.bcs, skb);
		break;
	case (PH_PULL | INDICATION):
		xmit_pull_ind_b(st->l1.bcs, skb);
		break;
	case (PH_PULL | REQUEST):
		xmit_pull_req_b(st);
		break;
	case (PH_ACTIVATE | REQUEST):
		test_and_set_bit(BC_FLG_ACTIV, &st->l1.bcs->Flag);
		modejade(st->l1.bcs, st->l1.mode, st->l1.bc);
		l1_msg_b(st, pr, arg);
		break;
	case (PH_DEACTIVATE | REQUEST):
		l1_msg_b(st, pr, arg);
		break;
	case (PH_DEACTIVATE | CONFIRM):
		test_and_clear_bit(BC_FLG_ACTIV, &st->l1.bcs->Flag);
		test_and_clear_bit(BC_FLG_BUSY, &st->l1.bcs->Flag);
		modejade(st->l1.bcs, 0, st->l1.bc);
		L1L2(st, PH_DEACTIVATE | CONFIRM, NULL);
		break;
    }
}

void
close_jadestate(struct BCState *bcs)
{
	modejade(bcs, 0, bcs->channel);
	bc_close(bcs);
}

static int
open_jadestate(struct IsdnCardState *cs, struct BCState *bcs)
{
	return bc_open(bcs);
}


int
setstack_jade(struct PStack *st, struct BCState *bcs)
{
	bcs->channel = st->l1.bc;
	if (open_jadestate(st->l1.hardware, bcs))
		return (-1);
	st->l1.bcs = bcs;
	st->l1.l2l1 = jade_l2l1;
	setstack_manager(st);
	bcs->st = st;
	setstack_l1_B(st);
	return (0);
}

static void jade_fill_fifo(struct BCState *bcs);

static struct bc_l1_ops jade_l1_ops = {
	.fill_fifo = jade_fill_fifo,
};

void __init
initjade(struct IsdnCardState *cs)
{
	int val;

	cs->bc_l1_ops = &jade_l1_ops;
	cs->bcs[0].BC_SetStack = setstack_jade;
	cs->bcs[1].BC_SetStack = setstack_jade;
	cs->bcs[0].BC_Close = close_jadestate;
	cs->bcs[1].BC_Close = close_jadestate;
	cs->bcs[0].unit = 0;
	cs->bcs[1].unit = 1;

	jade_write_reg(cs, 0, jade_HDLC_IMR, 0x00);
	jade_write_reg(cs, 1, jade_HDLC_IMR, 0x00);

	val = jade_read_reg(cs, 1, jade_HDLC_ISR);
	debugl1(cs, "jade B ISTA %x", val);
	val = jade_read_reg(cs, 0, jade_HDLC_ISR);
	debugl1(cs, "jade A ISTA %x", val);
	val = jade_read_reg(cs, 1, jade_HDLC_STAR);
	debugl1(cs, "jade B STAR %x", val);
	val = jade_read_reg(cs, 0, jade_HDLC_STAR);
	debugl1(cs, "jade A STAR %x", val);

	/* Unmask ints */
	jade_write_reg(cs, 0, jade_HDLC_IMR, 0xF8);
	jade_write_reg(cs, 1, jade_HDLC_IMR, 0xF8);

	/* Stop DSP audio tx/rx */
	jade_write_indirect(cs, 0x11, 0x0f);
	jade_write_indirect(cs, 0x17, 0x2f);

	/* Transparent Mode, RxTx inactive, No Test, No RFS/TFS */
	jade_write_reg(cs, 0, jade_HDLC_MODE, jadeMODE_TMO);
	jade_write_reg(cs, 1, jade_HDLC_MODE, jadeMODE_TMO);
	/* Power down, 1-Idle, RxTx least significant bit first */
	jade_write_reg(cs, 0, jade_HDLC_CCR0, 0x00);
	jade_write_reg(cs, 1, jade_HDLC_CCR0, 0x00);
	/* Mask all interrupts */
	jade_write_reg(cs, 0, jade_HDLC_IMR,  0x00);
	jade_write_reg(cs, 1, jade_HDLC_IMR,  0x00);
	/* Setup host access to hdlc controller */
	jade_write_indirect(cs, jade_HDLCCNTRACCESS, (jadeINDIRECT_HAH1|jadeINDIRECT_HAH2));
	/* Unmask HDLC int (don´t forget DSP int later on)*/
	jade_write_reg(cs, -1,jade_INT, (jadeINT_HDLC1|jadeINT_HDLC2));

	/* once again TRANSPARENT */	
	modejade(cs->bcs, 0, 0);
	modejade(cs->bcs + 1, 0, 0);
}

#include "jade_irq.c"
