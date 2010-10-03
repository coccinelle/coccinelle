static void empeg_close	(struct usb_serial_port *port, struct file *filp);
static void empeg_close (struct usb_serial_port *port, struct file * filp)
{
}


static int empeg_write (struct usb_serial_port *port, const unsigned char *buf, int count)
{
		usb_fill_bulk_urb (empeg_write_bulk_callback);

} 

static void empeg_write_bulk_callback (struct urb *urb, struct pt_regs *regs)
{
}


static void empeg_read_bulk_callback (struct urb *urb, struct pt_regs *regs)
{
	usb_fill_bulk_urb(empeg_read_bulk_callback);

}
