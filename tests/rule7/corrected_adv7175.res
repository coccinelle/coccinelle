#define DEBUGLEVEL 0
/* 
    adv7175 - adv7175a video encoder driver version 0.0.3

    Copyright (C) 1998 Dave Perks <dperks@ibm.net>

    Copyright (C) 1999 Wolfgang Scherr <scherr@net4you.net>
    Copyright (C) 2000 Serguei Miridonov <mirsev@cicese.mx>
       - some corrections for Pinnacle Systems Inc. DC10plus card.

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

#if (DEBUGLEVEL > 0)
#define DEBUG(x...)  x		/* Debug driver */
#else
#define DEBUG(x...)
#endif

#define I2C_ADV7175	0xd4
#define I2C_ADV7176	0x54

#define IF_NAME		"adv7175"

static char adv7175_name[] = "adv7175";
static char adv7176_name[] = "adv7176";
static char unknown_name[] = "UNKNOWN";
char *dname;

#if (DEBUGLEVEL > 0)
static char *inputs[] = { "pass_through", "play_back", "color_bar" };
static char *norms[] = { "PAL", "NTSC", "SECAM->PAL (may not work!)" };
#endif

#define I2C_DELAY   10

static unsigned short normal_i2c[] = {I2C_ADV7175, I2C_CLIENT_END};
static unsigned short normal_i2c_range[] = {I2C_CLIENT_END};
static unsigned short probe[2] = {I2C_CLIENT_END, I2C_CLIENT_END};
static unsigned short probe_range[2] = {I2C_CLIENT_END, I2C_CLIENT_END};
static unsigned short ignore[2] = {I2C_CLIENT_END, I2C_CLIENT_END};
static unsigned short ignore_range[2] = {I2C_CLIENT_END, I2C_CLIENT_END};
static unsigned short force[2] = {I2C_CLIENT_END, I2C_CLIENT_END};

static struct i2c_client_address_data addr_data = {
						normal_i2c, normal_i2c_range,
						probe, probe_range,
						ignore, ignore_range,
						force};
static struct i2c_client client_template;

struct adv7175 {
	struct i2c_client 	*client;
	int 			addr;
	unsigned char 		reg[128];
	struct semaphore	lock;
	int 			norm;
	int 			input;
	int 			enable;
	int 			bright;
	int 			contrast;
	int 			hue;
	int 			sat;
};

/* ----------------------------------------------------------------------- */
// Output filter:  S-Video  Composite

#define MR050       0x11	//0x09
#define MR060       0x14	//0x0c

//---------------------------------------------------------------------------

#define TR0MODE     0x46
#define TR0RST	    0x80

#define TR1CAPT	    0x80
#define TR1PLAY	    0x00

static const unsigned char init_common[] = {

	0x00, MR050,		/* MR0, PAL enabled */
	0x01, 0x00,		/* MR1 */
	0x02, 0x0c,		/* subc. freq. */
	0x03, 0x8c,		/* subc. freq. */
	0x04, 0x79,		/* subc. freq. */
	0x05, 0x26,		/* subc. freq. */
	0x06, 0x40,		/* subc. phase */

	0x07, TR0MODE,		/* TR0, 16bit */
	0x08, 0x21,		/*  */
	0x09, 0x00,		/*  */
	0x0a, 0x00,		/*  */
	0x0b, 0x00,		/*  */
	0x0c, TR1CAPT,		/* TR1 */
	0x0d, 0x4f,		/* MR2 */
	0x0e, 0x00,		/*  */
	0x0f, 0x00,		/*  */
	0x10, 0x00,		/*  */
	0x11, 0x00,		/*  */
	0x12, 0x00,		/* MR3 */
	0x24, 0x00,		/*  */
};

static const unsigned char init_pal[] = {
	0x00, MR050,		/* MR0, PAL enabled */
	0x01, 0x00,		/* MR1 */
	0x02, 0x0c,		/* subc. freq. */
	0x03, 0x8c,		/* subc. freq. */
	0x04, 0x79,		/* subc. freq. */
	0x05, 0x26,		/* subc. freq. */
	0x06, 0x40,		/* subc. phase */
};

static const unsigned char init_ntsc[] = {
	0x00, MR060,		/* MR0, NTSC enabled */
	0x01, 0x00,		/* MR1 */
	0x02, 0x55,		/* subc. freq. */
	0x03, 0x55,		/* subc. freq. */
	0x04, 0x55,		/* subc. freq. */
	0x05, 0x25,		/* subc. freq. */
	0x06, 0x1a,		/* subc. phase */
};

static int adv717x_attach(struct i2c_adapter *adap, int addr, unsigned short flags, int kind)
{
	struct adv7175 *encoder;
	struct	i2c_client	*client;
	int rv, i, x_common=39; /* x is number entries init_common - 1 */

	printk(KERN_INFO "adv717x: video chip found.\n");
	client=kmalloc(sizeof(*client), GFP_KERNEL);
	if(client == NULL)
		return -ENOMEM;

	client_template.adapter = adap;
	client_template.addr = addr;
	memcpy(client, &client_template, sizeof(*client));

	encoder = kmalloc(sizeof(*encoder), GFP_KERNEL);
	if (encoder == NULL) {
		kfree(client);
		return -ENOMEM;
	}

	memset(encoder, 0, sizeof(*encoder));
	if ((encoder->addr == I2C_ADV7175) || (encoder->addr == (I2C_ADV7175 + 2))) {
		dname = adv7175_name;
	} else if ((encoder->addr == I2C_ADV7176) || (encoder->addr == (I2C_ADV7176 + 2))) {
		dname = adv7176_name;
	} else {
		// We should never get here!!!
		dname = unknown_name;
	}
	strncpy(client->dev.name, dname, DEVICE_NAME_SIZE);
	init_MUTEX(&encoder->lock);
	encoder->client = client;
	encoder->addr = addr;
	encoder->norm = VIDEO_MODE_PAL;
	encoder->input = 0;
	encoder->enable = 1;

	for (i=1; i<x_common; i++) {
		rv = i2c_smbus_write_byte(client,init_common[i]);
		if (rv < 0) {
			printk(KERN_ERR "%s_attach: init error %d\n", client->dev.name, rv);
			break;
		}
	}

	if (rv >= 0) {
		i2c_smbus_write_byte_data(client,0x07, TR0MODE | TR0RST);
		i2c_smbus_write_byte_data(client,0x07, TR0MODE);
		i2c_smbus_read_byte_data(client,0x12);
		printk(KERN_INFO "%s_attach: %s rev. %d at 0x%02x\n",
		       client->dev.name, dname, rv & 1, client->addr);
	}

	i2c_attach_client(client);

	return 0;
}

static
int adv717x_probe(struct i2c_adapter *adap)
{
	return i2c_probe(adap, &addr_data, adv717x_attach);
}


static int adv717x_detach(struct i2c_client *client)
{
	i2c_detach_client(client);
	kfree(i2c_get_clientdata(client));
	kfree(client);
	return 0;
}

static int adv717x_command(struct i2c_client *client, unsigned int cmd,
			   void *arg)
{
	struct adv7175 *encoder = i2c_get_clientdata(client);
	int i, x_ntsc=13, x_pal=13; 
		/* x_ntsc is number of entries in init_ntsc -1 */
		/* x_pal is number of entries in init_pal -1 */

	switch (cmd) {

	case ENCODER_GET_CAPABILITIES:
		{
			struct video_encoder_capability *cap = arg;

			cap->flags = VIDEO_ENCODER_PAL | VIDEO_ENCODER_NTSC
			    // | VIDEO_ENCODER_SECAM
			    // | VIDEO_ENCODER_CCIR
			    ;
			cap->inputs = 2;
			cap->outputs = 1;
		}
		break;

	case ENCODER_SET_NORM:
		{
			int iarg = *(int *) arg;

			if (encoder->norm != iarg) {
				switch (iarg) {

				case VIDEO_MODE_NTSC:
					for (i=1; i< x_ntsc; i++)
						i2c_smbus_write_byte(client, init_ntsc[i]);
					if (encoder->input == 0)
						i2c_smbus_write_byte_data(client,0x0d, 0x4f); // Enable genlock
					i2c_smbus_write_byte_data(client,0x07, TR0MODE | TR0RST);
					i2c_smbus_write_byte_data(client,0x07, TR0MODE);
					break;

				case VIDEO_MODE_PAL:
					for (i=1; i< x_pal; i++)
						i2c_smbus_write_byte(client, init_pal[i]);
					if (encoder->input == 0)
						i2c_smbus_write_byte_data(client,0x0d, 0x4f); // Enable genlock
					i2c_smbus_write_byte_data(client,0x07, TR0MODE | TR0RST);
					i2c_smbus_write_byte_data(client,0x07, TR0MODE);
					break;

				case VIDEO_MODE_SECAM:	// WARNING! ADV7176 does not support SECAM.
					// This is an attempt to convert SECAM->PAL (typically
					// it does not work due to genlock: when decoder
					// is in SECAM and encoder in in PAL the subcarrier
					// can not be syncronized with horizontal frequency)
					for (i=1; i< x_pal; i++)
						i2c_smbus_write_byte(client, init_pal[i]);
					if (encoder->input == 0)
						i2c_smbus_write_byte_data(client,0x0d, 0x49); // Disable genlock
					i2c_smbus_write_byte_data(client,0x07, TR0MODE | TR0RST);
					i2c_smbus_write_byte_data(client,0x07, TR0MODE);
					break;
				default:
					printk(KERN_ERR
					       "%s: illegal norm: %d\n",
					       client->dev.name, iarg);
					return -EINVAL;

				}
				DEBUG(printk
				      (KERN_INFO "%s: switched to %s\n",
				       client->name, norms[iarg]));
				encoder->norm = iarg;
			}
		}
		break;

	case ENCODER_SET_INPUT:
		{
			int iarg = *(int *) arg;

			/* RJ: *iarg = 0: input is from SAA7110
			   *iarg = 1: input is from ZR36060
			   *iarg = 2: color bar */

			if (encoder->input != iarg) {
				switch (iarg) {

				case 0:
					i2c_smbus_write_byte_data(client, 0x01, 0x00);
					i2c_smbus_write_byte_data(client, 0x0c, TR1CAPT);	/* TR1 */
					if (encoder->norm ==
					    VIDEO_MODE_SECAM)
						i2c_smbus_write_byte_data(client, 0x0d, 0x49);	// Disable genlock
					else
						i2c_smbus_write_byte_data(client, 0x0d, 0x4f);	// Enable genlock
					i2c_smbus_write_byte_data(client, 0x07, TR0MODE | TR0RST);
					i2c_smbus_write_byte_data(client, 0x07, TR0MODE);
					//udelay(10);
					break;

				case 1:
					i2c_smbus_write_byte_data(client, 0x01, 0x00);
					i2c_smbus_write_byte_data(client, 0x0c, TR1PLAY);	/* TR1 */
					i2c_smbus_write_byte_data(client, 0x0d, 0x49);
					i2c_smbus_write_byte_data(client, 0x07, TR0MODE | TR0RST);
					i2c_smbus_write_byte_data(client, 0x07, TR0MODE);
					//udelay(10);
					break;

				case 2:
					i2c_smbus_write_byte_data(client, 0x01, 0x80);
					i2c_smbus_write_byte_data(client, 0x0d, 0x49);
					i2c_smbus_write_byte_data(client, 0x07, TR0MODE | TR0RST);
					i2c_smbus_write_byte_data(client, 0x07, TR0MODE);
					//udelay(10);
					break;

				default:
					printk(KERN_ERR
					       "%s: illegal input: %d\n",
					       client->dev.name, iarg);
					return -EINVAL;

				}
				DEBUG(printk
				      (KERN_INFO "%s: switched to %s\n",
				       client->name, inputs[iarg]));
				encoder->input = iarg;
			}
		}
		break;

	case ENCODER_SET_OUTPUT:
		{
			int *iarg = arg;

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
			i2c_smbus_write_byte_data(client, 0x61,
				      (encoder->
				       reg[0x61] & 0xbf) | (encoder->
							    enable ? 0x00 :
							    0x40));
		}
		break;

	default:
		return -EINVAL;
	}

	return 0;
}

/* ----------------------------------------------------------------------- */

static struct i2c_driver i2c_driver_adv7175 = {
	.owner		= THIS_MODULE,
	.name		= "adv7175",		/* name */
	.id		= I2C_DRIVERID_ADV717x,	/* ID */
	.flags		= I2C_DF_NOTIFY, //I2C_ADV7175, I2C_ADV7175 + 3,
	.attach_adapter	= adv717x_probe,
	.detach_client	= adv717x_detach,
	.command	= adv717x_command,
};

static struct i2c_driver i2c_driver_adv7176 = {
	.owner		= THIS_MODULE,
	.name		= "adv7176",		/* name */
	.id		= I2C_DRIVERID_ADV717x,	/* ID */
	.flags		= I2C_DF_NOTIFY, //I2C_ADV7176, I2C_ADV7176 + 3,
	.attach_adapter	= adv717x_probe,
	.detach_client	= adv717x_detach,
	.command	= adv717x_command,
};

static struct i2c_client client_template = {
	.driver		= &i2c_driver_adv7175,
	.dev		= {
		.name	= "adv7175_client",
	},
};

static int adv717x_init(void)
{
	int res_adv7175 = 0, res_adv7176 = 0;
	res_adv7175 = i2c_add_driver(&i2c_driver_adv7175);
	res_adv7176 = i2c_add_driver(&i2c_driver_adv7176);
	return (res_adv7175 | res_adv7176);	// Any idea how to make it better?
}

static void adv717x_exit(void)
{
	i2c_del_driver(&i2c_driver_adv7176);
	i2c_del_driver(&i2c_driver_adv7175);
}

module_init(adv717x_init);
module_exit(adv717x_exit);
MODULE_LICENSE("GPL");
