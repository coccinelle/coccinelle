@@
identifier I;
identifier f;
@@

static struct usb_serial_driver I = {
	.write_bulk_callback =	f
};

@@
identifier p1, p2;
@@

- void f (struct urb *p1, struct pt_regs *p2)
+ void f (struct urb *p1)
  {
    ... when != p1
  }

@@
identifier I;
identifier f;
@@

static struct usb_serial_driver I = {
	.read_bulk_callback =	f
};

@@
identifier p1, p2;
@@

- void f (struct urb *p1, struct pt_regs *p2)
+ void f (struct urb *p1)
  {
    ... when != p1
  }

@@
identifier I;
identifier f;
@@

static struct usb_serial_driver I = {
	.read_int_callback =	f
};

@@
identifier p1, p2;
@@

- void f (struct urb *p1, struct pt_regs *p2)
+ void f (struct urb *p1)
  {
    ... when != p1
  }

@@
expression E1, E2, E3, E4, E5, E6, E7;
identifier f;
struct urb u;
struct urb *u1;
@@

(
usb_fill_bulk_urb(E1, E2, E3, E4, E5, f, E6)
|
usb_fill_int_urb(E1, E2, E3, E4, E5, f, E6, E7)
|
usb_fill_control_urb(E1, E2, E3, E4, E5, E6, f, E7)
|
u.complete = f
|
u1->complete = f
)

@@
identifier p1, p2;
@@

- void f (struct urb *p1, struct pt_regs *p2)
+ void f (struct urb *p1)
  {
    ... when != p2
  }

//@@
//identifier setup_urb;
//identifier callback, regs;
//expression E1, E2, E3, E4, E5, E6;
//@@
//
//setup_urb(...,void (*callback)(struct urb *
//-                                , struct pt_regs *regs
//          ), ...)
//{
//	<...
//\+	usb_fill_bulk_urb(E1, E2, E3, E4, E5, callback, E6);
//        ...>
//}
//
//@@
//identifier f;
//@@
//
//setup_urb(...,f,...)
//
//@@
//identifier p1, p2;
//@@
//
//- void f (struct urb *p1, struct pt_regs *p2)
//+ void f (struct urb *p1)
//  {
//    ... when != p2
//  }

@@
struct pcmcia_device *link;
identifier interrupt_fn;
@@

link->irq.Handler = interrupt_fn;

@@
identifier irq, dev_inst, regs;
@@
interrupt_fn(int irq, void *dev_inst
-            , struct pt_regs *regs
  ) {
    ... when != regs
  }
