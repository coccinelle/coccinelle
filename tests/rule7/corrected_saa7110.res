/*
    saa7110 - Philips SAA7110(A) video decoder driver

    Copyright (C) 1998 Pauline Middelink <middelin@polyware.nl>

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
#include <linux/types.h>
#include <linux/delay.h>
#include <linux/slab.h>
#include <asm/io.h>
#include <asm/uaccess.h>

#include <linux/i2c.h>
#include <linux/videodev.h>
#include "linux/video_decoder.h"

#define DEBUG(x...)			/* remove when no long debugging */

#define SAA7110_MAX_INPUT	9	/* 6 CVBS, 3 SVHS */
#define SAA7110_MAX_OUTPUT	0	/* it's a decoder only */

#define	I2C_SAA7110		0x9C	/* or 0x9E */

#define IF_NAME	"saa7110"
#define	I2C_DELAY		10	/* 10 us or 100khz */

static unsigned short normal_i2c[] = {34>>1, I2C_CLIENT_END };
static unsigned short normal_i2c_range[] = { I2C_CLIENT_END };
static unsigned short probe[2] = { I2C_CLIENT_END, I2C_CLIENT_END };
static unsigned short probe_range[2] = { I2C_CLIENT_END, I2C_CLIENT_END };
static unsigned short ignore[2] = { I2C_CLIENT_END, I2C_CLIENT_END };
static unsigned short ignore_range[2] = { I2C_CLIENT_END, I2C_CLIENT_END };
static unsigned short force[2] = { I2C_CLIENT_END, I2C_CLIENT_END };

static struct i2c_client_address_data addr_data = {
	normal_i2c, normal_i2c_range,
	probe, probe_range,
	ignore, ignore_range,
	force
};

static struct i2c_client client_template;

struct saa7110 {
	struct i2c_client *client;
	int		addr;
	unsigned char	reg[36];
	struct semaphore lock;
	int		norm;
	int		input;
	int		enable;
	int		bright;
	int		contrast;
	int		hue;
	int		sat;
};

/* ----------------------------------------------------------------------- */
/* SAA7110 functions							   */
/* ----------------------------------------------------------------------- */
static
int saa7110_selmux(struct i2c_client *client, int chan)
{
static	const unsigned char modes[9][8] = {
/* mode 0 */	{ 0x00, 0xD9, 0x17, 0x40, 0x03, 0x44, 0x75, 0x16 },
/* mode 1 */	{ 0x00, 0xD8, 0x17, 0x40, 0x03, 0x44, 0x75, 0x16 },
/* mode 2 */	{ 0x00, 0xBA, 0x07, 0x91, 0x03, 0x60, 0xB5, 0x05 },
/* mode 3 */	{ 0x00, 0xB8, 0x07, 0x91, 0x03, 0x60, 0xB5, 0x05 },
/* mode 4 */	{ 0x00, 0x7C, 0x07, 0xD2, 0x83, 0x60, 0xB5, 0x03 },
/* mode 5 */	{ 0x00, 0x78, 0x07, 0xD2, 0x83, 0x60, 0xB5, 0x03 },
/* mode 6 */	{ 0x80, 0x59, 0x17, 0x42, 0xA3, 0x44, 0x75, 0x12 },
/* mode 7 */	{ 0x80, 0x9A, 0x17, 0xB1, 0x13, 0x60, 0xB5, 0x14 },
/* mode 8 */	{ 0x80, 0x3C, 0x27, 0xC1, 0x23, 0x44, 0x75, 0x21 } };
	const unsigned char* ptr = modes[chan];

	i2c_smbus_write_byte_data(client,0x06,ptr[0]);	/* Luminance control	*/
	i2c_smbus_write_byte_data(client,0x20,ptr[1]);	/* Analog Control #1	*/
	i2c_smbus_write_byte_data(client,0x21,ptr[2]);	/* Analog Control #2	*/
	i2c_smbus_write_byte_data(client,0x22,ptr[3]);	/* Mixer Control #1	*/
	i2c_smbus_write_byte_data(client,0x2C,ptr[4]);	/* Mixer Control #2	*/
	i2c_smbus_write_byte_data(client,0x30,ptr[5]);	/* ADCs gain control	*/
	i2c_smbus_write_byte_data(client,0x31,ptr[6]);	/* Mixer Control #3	*/
	i2c_smbus_write_byte_data(client,0x21,ptr[7]);	/* Analog Control #2	*/

	return 0;
}

static
int determine_norm(struct i2c_client* client)
{
	int	status;

	/* mode changed, start automatic detection */
	status = i2c_smbus_read_byte(client);
	if ((status & 3) == 0) {
		i2c_smbus_write_byte_data(client,0x06,0x80);
		if (status & 0x20) {
			DEBUG(printk(KERN_INFO "%s: norm=bw60\n",adp->name));
			i2c_smbus_write_byte_data(client,0x2E,0x81);
			return VIDEO_MODE_NTSC;
		}
		DEBUG(printk(KERN_INFO "%s: norm=bw50\n",adp->name));
		i2c_smbus_write_byte_data(client,0x2E,0x9A);
		return VIDEO_MODE_PAL;
	}

	i2c_smbus_write_byte_data(client,0x06,0x00);
	if (status & 0x20) {	/* 60Hz */
		DEBUG(printk(KERN_INFO "%s: norm=ntsc\n",adp->name));
		i2c_smbus_write_byte_data(client,0x0D,0x06);
		i2c_smbus_write_byte_data(client,0x11,0x2C);
		i2c_smbus_write_byte_data(client,0x2E,0x81);
		return VIDEO_MODE_NTSC;
	}

	/* 50Hz -> PAL/SECAM */
	i2c_smbus_write_byte_data(client,0x0D,0x06);
	i2c_smbus_write_byte_data(client,0x11,0x59);
	i2c_smbus_write_byte_data(client,0x2E,0x9A);

	mdelay(150);	/* pause 150 ms */

	status = i2c_smbus_read_byte(client);
	if ((status & 0x03) == 0x01) {
		DEBUG(printk(KERN_INFO "%s: norm=secam\n",dev->name));
		i2c_smbus_write_byte_data(client,0x0D,0x07);
		return VIDEO_MODE_SECAM;
	}
	DEBUG(printk(KERN_INFO "%s: norm=pal\n",dev->name));
	return VIDEO_MODE_PAL;
}

static
int saa7110_attach(struct i2c_adapter *adap, int  addr, unsigned short flags, int kind)
{
static	const unsigned char initseq[] = {
	     0, 0x4C, 0x3C, 0x0D, 0xEF, 0xBD, 0xF0, 0x00, 0x00,
		0xF8, 0xF8, 0x60, 0x60, 0x00, 0x06, 0x18, 0x90,
		0x00, 0x2C, 0x40, 0x46, 0x42, 0x1A, 0xFF, 0xDA,
		0xF0, 0x8B, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0xD9, 0x17, 0x40, 0x41, 0x80, 0x41, 0x80, 0x4F,
		0xFE, 0x01, 0xCF, 0x0F, 0x03, 0x01, 0x81, 0x03,
		0x40, 0x75, 0x01, 0x8C, 0x03};
	struct	saa7110	*decoder;
	struct i2c_client *client;
	int			rv;
	client=kmalloc(sizeof(*client), GFP_KERNEL);
	if(client == NULL) 
		return -ENOMEM;
	client_template.adapter = adap;
	client_template.addr = addr;
	memcpy(client, &client_template, sizeof(*client));

	decoder = kmalloc(sizeof(*decoder), GFP_KERNEL);
	if (decoder == NULL) {
		kfree(client);
		return -ENOMEM;
		}

	/* clear our private data */
	memset(decoder, 0, sizeof(*decoder));
	strncpy(client->dev.name, IF_NAME, DEVICE_NAME_SIZE);
	decoder->client = client;
	i2c_set_clientdata(client, decoder);
	decoder->addr = addr;
	decoder->norm = VIDEO_MODE_PAL;
	decoder->input = 0;
	decoder->enable = 1;
	decoder->bright = 32768;
	decoder->contrast = 32768;
	decoder->hue = 32768;
	decoder->sat = 32768;

	rv = i2c_master_send(client, initseq, sizeof(initseq));
	if (rv < 0)
		printk(KERN_ERR "%s_attach: init status %d\n", client->dev.name, rv);
	else {
		i2c_smbus_write_byte_data(client,0x21,0x16);
		i2c_smbus_write_byte_data(client,0x0D,0x04);
		DEBUG(printk(KERN_INFO "%s_attach: chip version %x\n", client->dev.name, i2c_smbus_read_byte(client)));
		i2c_smbus_write_byte_data(client,0x0D,0x06);
	}

	init_MUTEX(&decoder->lock);
	i2c_attach_client(client);
	MOD_INC_USE_COUNT;
	/* setup and implicit mode 0 select has been performed */
	return 0;
}

static 
int saa7110_probe(struct i2c_adapter *adap) 
{
	return i2c_probe(adap, &addr_data, saa7110_attach);
}

static
int saa7110_detach(struct i2c_client *client)
{
	struct saa7110* decoder = i2c_get_clientdata(client);

	i2c_detach_client(client);

	DEBUG(printk(KERN_INFO "%s_detach\n",client->dev.name));

	/* stop further output */
	i2c_smbus_write_byte_data(client,0x0E,0x00);

	kfree(decoder);
	kfree(client);

	MOD_DEC_USE_COUNT;
	return 0;
}

static
int saa7110_command(struct i2c_client *client, unsigned int cmd, void *arg)
{
	struct saa7110* decoder = i2c_get_clientdata(client);
	int	v;

	switch (cmd) {
	 case DECODER_GET_CAPABILITIES:
		{
			struct video_decoder_capability *dc = arg;
			dc->flags = VIDEO_DECODER_PAL
				  | VIDEO_DECODER_NTSC
				  | VIDEO_DECODER_SECAM
				  | VIDEO_DECODER_AUTO
				  | VIDEO_DECODER_CCIR;
			dc->inputs = SAA7110_MAX_INPUT;
			dc->outputs = SAA7110_MAX_OUTPUT;
		}
		break;

	 case DECODER_GET_STATUS:
		{
			struct saa7110* decoder = i2c_get_clientdata(client);
			int status;
			int res = 0;

			status = i2c_smbus_read_byte(client);
			if (status & 0x40)
				res |= DECODER_STATUS_GOOD;
			if (status & 0x03)
				res |= DECODER_STATUS_COLOR;

			switch (decoder->norm) {
			 case VIDEO_MODE_NTSC:
				res |= DECODER_STATUS_NTSC;
				break;
			 case VIDEO_MODE_PAL:
				res |= DECODER_STATUS_PAL;
				break;
			 case VIDEO_MODE_SECAM:
				res |= DECODER_STATUS_SECAM;
				break;
			}
			*(int*)arg = res;
		}
		break;

	 case DECODER_SET_NORM:
		v = *(int*)arg;
		if (decoder->norm != v) {
			decoder->norm = v;
			i2c_smbus_write_byte_data(client, 0x06, 0x00);
			switch (v) {
			 case VIDEO_MODE_NTSC:
				i2c_smbus_write_byte_data(client, 0x0D, 0x06);
				i2c_smbus_write_byte_data(client, 0x11, 0x2C);
				i2c_smbus_write_byte_data(client, 0x30, 0x81);
				i2c_smbus_write_byte_data(client, 0x2A, 0xDF);
				break;
			 case VIDEO_MODE_PAL:
				i2c_smbus_write_byte_data(client, 0x0D, 0x06);
				i2c_smbus_write_byte_data(client, 0x11, 0x59);
				i2c_smbus_write_byte_data(client, 0x2E, 0x9A);
				break;
			 case VIDEO_MODE_SECAM:
				i2c_smbus_write_byte_data(client, 0x0D, 0x07);
				i2c_smbus_write_byte_data(client, 0x11, 0x59);
				i2c_smbus_write_byte_data(client, 0x2E, 0x9A);
				break;
			 case VIDEO_MODE_AUTO:
				*(int*)arg = determine_norm(client);
				break;
			 default:
				return -EPERM;
			}
		}
		break;

	 case DECODER_SET_INPUT:
		v = *(int*)arg;
		if (v<0 || v>SAA7110_MAX_INPUT)
			return -EINVAL;
		if (decoder->input != v) {
			decoder->input = v;
			saa7110_selmux(client, v);
		}
		break;

	 case DECODER_SET_OUTPUT:
		v = *(int*)arg;
		/* not much choice of outputs */
		if (v != 0)
			return -EINVAL;
		break;

	 case DECODER_ENABLE_OUTPUT:
		v = *(int*)arg;
		if (decoder->enable != v) {
			decoder->enable = v;
			i2c_smbus_write_byte_data(client,0x0E, v ? 0x18 : 0x00);
		}
		break;

	 case DECODER_SET_PICTURE:
		{
			struct video_picture *pic = arg;

			if (decoder->bright != pic->brightness) {
				/* We want 0 to 255 we get 0-65535 */
				decoder->bright = pic->brightness;
				i2c_smbus_write_byte_data(client, 0x19, decoder->bright >> 8);
			}
			if (decoder->contrast != pic->contrast) {
				/* We want 0 to 127 we get 0-65535 */
				decoder->contrast = pic->contrast;
				i2c_smbus_write_byte_data(client, 0x13, decoder->contrast >> 9);
			}
			if (decoder->sat != pic->colour) {
				/* We want 0 to 127 we get 0-65535 */
				decoder->sat = pic->colour;
				i2c_smbus_write_byte_data(client, 0x12, decoder->sat >> 9);
			}
			if (decoder->hue != pic->hue) {
				/* We want -128 to 127 we get 0-65535 */
				decoder->hue = pic->hue;
				i2c_smbus_write_byte_data(client, 0x07, (decoder->hue>>8)-128);
			}
		}
		break;

	 case DECODER_DUMP:
		for (v=0; v<34; v+=16) {
			int j;
			DEBUG(printk(KERN_INFO "%s: %03x\n",client->dev.name,v));
			for (j=0; j<16; j++) {
				DEBUG(printk(KERN_INFO " %02x",decoder->reg[v+j]));
			}
			DEBUG(printk(KERN_INFO "\n"));
		}
		break;

	 default:
		DEBUG(printk(KERN_INFO "unknown saa7110_command??(%d)\n",cmd));
		return -EINVAL;
	}
	return 0;
}

/* ----------------------------------------------------------------------- */

static struct i2c_driver i2c_driver_saa7110 =
{
	.owner 		= THIS_MODULE,
	.name 		= IF_NAME,			/* name */
	.id  		= I2C_DRIVERID_SAA7110,	/* in i2c.h */
	.flags 		= I2C_DF_NOTIFY,	/* Addr range */
	.attach_adapter = saa7110_probe,
	.detach_client 	= saa7110_detach,
	.command 	= saa7110_command
};
static struct i2c_client client_template = {
	.id 		= -1,
	.driver 	= &i2c_driver_saa7110,
	.dev		= {
		.name	= "saa7110_client",
	},
};

static int saa7110_init(void)
{
	return i2c_add_driver(&i2c_driver_saa7110);
}

static void saa7110_exit(void)
{
	i2c_del_driver(&i2c_driver_saa7110);
}


module_init(saa7110_init);
module_exit(saa7110_exit);
MODULE_LICENSE("GPL");
