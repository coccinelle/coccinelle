extern int abs(int __x) __attribute__ ((__const__)); /* Shut up warning */


static unsigned char bugbuf[16] __attribute__ ((aligned (16)));

struct descriptor frame __attribute__ ((__aligned__(16)));

// HANDLING: test_c/bugs/47/errors/linux-2.5.28/drivers/net/ns83820.c
// parse error 
//  = File "test_c/bugs/47/errors/linux-2.5.28/drivers/net/ns83820.c", line 438, characters 15
//    around = '__attribute__', whole content = 	char		pad[16] __attribute__((aligned(16)));
// charpos = 11973

extern int abs(int __x) __attribute__ ((__const__)); /* Shut up warning */

//__attribute__((unused))



unsigned long long ret __attribute__ ((aligned (64)));



struct ei_device *ei_local __attribute((unused)) = (struct ei_device *) netdev_priv(dev);


u32		information __attribute__ ((packed));


lcd_info __attribute__((aligned(8))) = 1;




static u32  crc32tab[] __attribute__ ((aligned(8))) = {
 	0xD202EF8D,  0xA505DF1B,  0x3C0C8EA1,  0x4B0BBE37,
};

int warn __attribute__((unused)) = 0;

static char *lvm_snap_version __attribute__ ((unused)) = 1;



// HANDLING: test_c/bugs/7/ok/linux-2.5.67/drivers/i2c/chips/via686a.c
//parse error 
// = File "test_c/bugs/7/ok/linux-2.5.67/drivers/i2c/chips/via686a.c", line 728, characters 0
//    around = '__attribute__', whole content = __attribute__((unused))
// charpos = 27520
//FOUND SYNC at line 755


/* FIXME, remove these functions, they are here to verify the sysfs conversion
 * is correct, or not */
__attribute__((unused))
static void via686a_in(struct i2c_client *client, int operation, int ctl_name,
               int *nrels_mag, long *results)
{
	struct via686a_data *data = i2c_get_clientdata(client);
}
