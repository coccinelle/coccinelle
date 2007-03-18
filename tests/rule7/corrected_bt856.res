/* 
    bt856 - BT856A Digital Video Encoder (Rockwell Part)

    Copyright (C) 1999 Mike Bernson <mike@mlb.org>
    Copyright (C) 1998 Dave Perks <dperks@ibm.net>

    Modifications for LML33/DC10plus unified driver
    Copyright (C) 2000 Serguei Miridonov <mirsev@cicese.mx>
    
    This code was modify/ported from the saa7111 driver written
    by Dave Perks.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#include <linux/module.h>
#include <linux/init.h>
#include <linux/delay.h>
#include <linux/errno.h>
#include <linux/fs.h>
#include <linux/kernel.h>
#include <linux/major.h>
#include <linux/slab.h>
#include <linux/mm.h>
#include <linux/pci.h>
#include <linux/signal.h>
#include <asm/io.h>
#include <asm/pgtable.h>
#include <asm/page.h>
#include <linux/sched.h>
#include <linux/types.h>
#include <linux/wrapper.h>

#include <linux/videodev.h>
#include <linux/version.h>
#include <asm/uaccess.h>

#include <linux/i2c.h>
#include <linux/video_encoder.h>

#define DEBUG(x)   x		/* Debug driver */

/* ----------------------------------------------------------------------- */

static unsigned short normal_i2c[] = {34>>1, I2C_CLIENT_END };
static unsigned short normal_i2c_range[] = { I2C_CLIENT_END };
static unsigned short probe[2] = { I2C_CLIENT_END, I2C_CLIENT_END };
static unsigned short probe_range[2] = { I2C_CLIENT_END , I2C_CLIENT_END };
static unsigned short ignore[2] = { I2C_CLIENT_END , I2C_CLIENT_END };
static unsigned short ignore_range[2] = { I2C_CLIENT_END , I2C_CLIENT_END };
static unsigned short force[2] = { I2C_CLIENT_END , I2C_CLIENT_END };

static struct i2c_client_address_data addr_data = {
	normal_i2c , normal_i2c_range,
	probe , probe_range,
	ignore , ignore_range,
	force
};

static struct i2c_client client_template;

struct bt856 {
	struct i2c_client *client;
	int addr;
	unsigned char reg[128];

	int norm;
	int enable;
	int bright;
	int contrast;
	int hue;
	int sat;
	struct semaphore lock;
};

#define   I2C_BT856        0x88

#define I2C_DELAY   10

/* ----------------------------------------------------------------------- */

static int bt856_setbit(struct bt856 *dev, int subaddr, int bit, int data)
{
	return i2c_smbus_write_byte_data(dev->client, subaddr,(dev->reg[subaddr] & ~(1 << bit)) | (data ? (1 << bit) : 0));
}

/* ----------------------------------------------------------------------- */

static int bt856_attach(struct i2c_adapter *adap, int addr , unsigned long flags, int kind)
{
	struct bt856 *encoder;
	struct i2c_client *client;
	
	client = kmalloc(sizeof(*client), GFP_KERNEL);
	if(client == NULL)
		return -ENOMEM;
	client_template.adapter = adap;
	client_template.addr = addr;
	memcpy(client, &client_template, sizeof(*client));	
	
	/* This chip is not on the buz card but at the same address saa7185 */
	//if (memcmp(device->bus->name, "buz", 3) == 0 || memcmp(device->bus->name, "zr36057", 6) == 0)
	//   return 1;

	encoder = kmalloc(sizeof(struct bt856), GFP_KERNEL);

	if (encoder == NULL) {
		MOD_DEC_USE_COUNT;
		return -ENOMEM;
	}


	memset(encoder, 0, sizeof(struct bt856));
	strncpy(client->dev.name, "bt856", DEVICE_NAME_SIZE);
	encoder->client = client;
	i2c_set_clientdata(client, encoder);
	encoder->addr = client->addr;
	encoder->norm = VIDEO_MODE_NTSC;
	encoder->enable = 1;

	DEBUG(printk(KERN_INFO "%s-bt856: attach\n", encoder->client->dev.name));

	i2c_smbus_write_byte_data(client, 0xdc, 0x18);
	encoder->reg[0xdc] = 0x18;
	i2c_smbus_write_byte_data(client, 0xda, 0);
	encoder->reg[0xda] = 0;
	i2c_smbus_write_byte_data(client, 0xde, 0);
	encoder->reg[0xde] = 0;

	bt856_setbit(encoder, 0xdc, 3, 1);
	//bt856_setbit(encoder, 0xdc, 6, 0);
	bt856_setbit(encoder, 0xdc, 4, 1);

	switch (encoder->norm) {

	case VIDEO_MODE_NTSC:
		bt856_setbit(encoder, 0xdc, 2, 0);
		break;

	case VIDEO_MODE_PAL:
		bt856_setbit(encoder, 0xdc, 2, 1);
		break;
	}

	bt856_setbit(encoder, 0xdc, 1, 1);
	bt856_setbit(encoder, 0xde, 4, 0);
	bt856_setbit(encoder, 0xde, 3, 1);
	init_MUTEX(&encoder->lock);
	i2c_attach_client(client);
	MOD_INC_USE_COUNT;
	return 0;
}

static int bt856_probe(struct i2c_adapter *adap)
{
	return i2c_probe(adap, &addr_data , bt856_attach);
}

static int bt856_detach(struct i2c_client *client)
{
	i2c_detach_client(client);
	kfree(i2c_get_clientdata(client));
	kfree(client);
	MOD_DEC_USE_COUNT;
	return 0;
}

static int bt856_command(struct i2c_client *client, unsigned int cmd,
			 void *arg)
{
	struct bt856 *encoder = i2c_get_clientdata(client);

	switch (cmd) {

	case ENCODER_GET_CAPABILITIES:
		{
			struct video_encoder_capability *cap = arg;

			DEBUG(printk
			      (KERN_INFO "%s-bt856: get capabilities\n",
			       encoder->client->dev.name));

			cap->flags
			    = VIDEO_ENCODER_PAL
			    | VIDEO_ENCODER_NTSC | VIDEO_ENCODER_CCIR;
			cap->inputs = 2;
			cap->outputs = 1;
		}
		break;

	case ENCODER_SET_NORM:
		{
			int *iarg = arg;

			DEBUG(printk(KERN_INFO "%s-bt856: set norm %d\n",
				     encoder->client->dev.name, *iarg));

			switch (*iarg) {

			case VIDEO_MODE_NTSC:
				bt856_setbit(encoder, 0xdc, 2, 0);
				break;

			case VIDEO_MODE_PAL:
				bt856_setbit(encoder, 0xdc, 2, 1);
				bt856_setbit(encoder, 0xda, 0, 0);
				//bt856_setbit(encoder, 0xda, 0, 1);
				break;

			default:
				return -EINVAL;

			}
			encoder->norm = *iarg;
		}
		break;

	case ENCODER_SET_INPUT:
		{
			int *iarg = arg;

			DEBUG(printk(KERN_INFO "%s-bt856: set input %d\n",
				     encoder->client->dev.name, *iarg));

			/*     We only have video bus.
			   *iarg = 0: input is from bt819
			   *iarg = 1: input is from ZR36060 */

			switch (*iarg) {

			case 0:
				bt856_setbit(encoder, 0xde, 4, 0);
				bt856_setbit(encoder, 0xde, 3, 1);
				bt856_setbit(encoder, 0xdc, 3, 1);
				bt856_setbit(encoder, 0xdc, 6, 0);
				break;
			case 1:
				bt856_setbit(encoder, 0xde, 4, 0);
				bt856_setbit(encoder, 0xde, 3, 1);
				bt856_setbit(encoder, 0xdc, 3, 1);
				bt856_setbit(encoder, 0xdc, 6, 1);
				break;
			case 2:	// Color bar
				bt856_setbit(encoder, 0xdc, 3, 0);
				bt856_setbit(encoder, 0xde, 4, 1);
				break;
			default:
				return -EINVAL;

			}
		}
		break;

	case ENCODER_SET_OUTPUT:
		{
			int *iarg = arg;

			DEBUG(printk(KERN_INFO "%s-bt856: set output %d\n",
				     encoder->client->dev.name, *iarg));

			/* not much choice of outputs */
			if (*iarg != 0) {
				return -EINVAL;
			}
		}
		break;

	case ENCODER_ENABLE_OUTPUT:
		{
			int *iarg = arg;

			encoder->enable = !!*iarg;

			DEBUG(printk
			      (KERN_INFO "%s-bt856: enable output %d\n",
			       encoder->client->dev.name, encoder->enable));
		}
		break;

	default:
		return -EINVAL;
	}

	return 0;
}

/* ----------------------------------------------------------------------- */

static struct i2c_driver i2c_driver_bt856 = {
	.owner = THIS_MODULE,
	.name = "bt856",		/* name */
	.id = I2C_DRIVERID_BT856,	/* ID */
	.flags = I2C_DF_NOTIFY,
	.attach_adapter = bt856_probe,
	.detach_client = bt856_detach,
	.command = bt856_command
};

static struct i2c_client client_template = {
	.id = -1,
	.driver = &i2c_driver_bt856,
	.dev = {
		.name = "bt856_client",
	},
};

static int bt856_init(void)
{
	return i2c_add_driver(&i2c_driver_bt856);
}

static void bt856_exit(void)
{
	i2c_del_driver(&i2c_driver_bt856);
}

module_init(bt856_init);
module_exit(bt856_exit);
MODULE_LICENSE("GPL");
