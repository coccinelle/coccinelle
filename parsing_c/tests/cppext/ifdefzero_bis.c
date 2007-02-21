static struct {
 	char drv_id;           /* "jumpered" drive ID or -1 */
 	char drv_sel;          /* drive select lines bits */

#if 000 /* no supported drive has it */
 	char vol_chan2;
 	u_char vol_ctrl2;
 	char vol_chan3;
 	u_char vol_ctrl3;
#endif 000
 	u_char volume_control; /* TEAC on/off bits */
};

void main(int i) {


  ((file->f_mode & FMODE_READ) && as->usbin.dma.mapped);
#if 0
	if (arg)
		get_user(val, (int *)arg);
	printk(KERN_DEBUG "usbaudio: usb_audio_ioctl cmd=%x arg=%lx *arg=%d\n", cmd, arg, val)
#endif
	switch (cmd) {
	case OSS_GETVERSION:
          foo;
        }
}
