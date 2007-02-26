
void main (int i) 
{
	if (err)
		DBG(1, "Failed to register the I2C adapter")
	else
		DBG(5, "I2C adapter registered")

}



void main (int i) 
{
  if (err) {
    DBG(1, "Failed to register the I2C adapter");
  } else {
    DBG(5, "I2C adapter registered");
  }

}


static void
zero_suspend (struct usb_gadget *gadget)
{
	struct zero_dev		*dev = get_gadget_data (gadget);

	if (gadget->speed == USB_SPEED_UNKNOWN)
		return;

	if (autoresume) {
		mod_timer (&dev->resume, jiffies + (HZ * autoresume));
		DBG (dev, "suspend, wakeup in %d seconds\n", autoresume);
	} else
		DBG (dev, "suspend\n");
}
