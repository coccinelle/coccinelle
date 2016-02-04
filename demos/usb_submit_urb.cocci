@@
expression lock, flags;
expression urb;
@@

  spin_lock_irqsave(lock, flags);
  <...
- usb_submit_urb(urb)
+ usb_submit_urb(urb, GFP_ATOMIC)
  ...>
  spin_unlock_irqrestore(lock, flags);

@@
expression urb;
@@

- usb_submit_urb(urb)
+ usb_submit_urb(urb, GFP_KERNEL)
