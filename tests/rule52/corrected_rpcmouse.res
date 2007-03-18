/*
 *  Acorn RiscPC mouse driver for Linux/ARM
 *
 *  Copyright (c) 2000-2002 Vojtech Pavlik
 *  Copyright (C) 1996-1998 Russell King
 *
 */

/*
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 as published by
 * the Free Software Foundation.
 *
 * This handles the Acorn RiscPCs mouse.  We basically have a couple of
 * hardware registers that track the sensor count for the X-Y movement and
 * another register holding the button state.  On every VSYNC interrupt we read
 * the complete state and then work out if something has changed.
 */

#include <linux/module.h>
#include <linux/sched.h>
#include <linux/ptrace.h>
#include <linux/interrupt.h>
#include <linux/init.h>

#include <asm/hardware.h>
#include <asm/irq.h>
#include <asm/io.h>
#include <asm/hardware/iomd.h>

MODULE_AUTHOR("Vojtech Pavlik, Russell King");
MODULE_DESCRIPTION("Acorn RiscPC mouse driver");
MODULE_LICENSE("GPL");

static short rpcmouse_lastx, rpcmouse_lasty;

static struct input_dev rpcmouse_dev = {
	evbit:		{ BIT(EV_KEY) | BIT(EV_REL) },
	keybit: 	{ [LONG(BTN_LEFT)] = BIT(BTN_LEFT) | BIT(BTN_MIDDLE) | BIT(BTN_RIGHT) },
	relbit:		{ BIT(REL_X) | BIT(REL_Y) },
	name:		"Acorn RiscPC Mouse",
	phys:		"rpcmouse/input0",
};

static void rpcmouse_irq(int irq, void *dev_id, struct pt_regs *regs)
{
	short x, y, dx, dy, b;

	x = (short) iomd_readl(IOMD_MOUSEX);
	y = (short) iomd_readl(IOMD_MOUSEY);
	b = (short) (__raw_readl(0xe0310000) >> 4) & 7;

	dx = x - rpcmouse_lastx;
	dy = y - rpcmouse_lasty; 

	rpcmouse_lastx = x;
	rpcmouse_lasty = y;

	input_report_rel(&rpcmouse_dev, REL_X, dx);
	input_report_rel(&rpcmouse_dev, REL_Y, dy);

	input_report_key(&rpcmouse_dev, BTN_LEFT,   buttons & 0x10);
	input_report_key(&rpcmouse_dev, BTN_MIDDLE, buttons & 0x20);
	input_report_key(&rpcmouse_dev, BTN_RIGHT,  buttons & 0x40);
}

static int __init rpcmouse_init(void)
{
	rpcmouse_lastx = (short) iomd_readl(IOMD_MOUSEX);
	rpcmouse_lasty = (short) iomd_readl(IOMD_MOUSEY);

	if (request_irq(IRQ_VSYNCPULSE, rpcmouse_irq, SA_SHIRQ, "rpcmouse", NULL)) {
		printk(KERN_ERR "rpcmouse: unable to allocate VSYNC interrupt\n");
		return -1;
	}

	input_register_device(&rpcmouse_dev);
	rpcmouse_dev.id.bustype	=BUS_HOST,
	rpcmouse_dev.id.vendor	=0x0005,
	rpcmouse_dev.id.product	=0x0001,
	rpcmouse_dev.id.version	=0x0100,

	printk(KERN_INFO "input: Acorn RiscPC mouse irq %d", IRQ_VSYNCPULSE);

	return 0;
}

static void __exit rpcmouse_exit(void)
{
	input_unregister_device(&rpcmouse_dev);
	free_irq(IRQ_VSYNCPULSE, NULL);
}

module_init(rpcmouse_init);
module_exit(rpcmouse_exit);
