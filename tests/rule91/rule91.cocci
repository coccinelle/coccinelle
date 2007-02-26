@@
identifier I;
identifier f;
@@

struct usb_serial_driver I = {
	.write_bulk_callback =	f,
};

@@
identifier p1, p2;
@@

- void f (struct urb *p1, struct pt_regs *p2)
+ void f (struct urb *p1)
  {
    ... when != p2
  }

@@
identifier I;
identifier f;
@@

struct usb_serial_driver I = {
	.read_bulk_callback =	f,
};

@@
identifier p1, p2;
@@

- void f (struct urb *p1, struct pt_regs *p2)
+ void f (struct urb *p1)
  {
    ... when != p2
  }

@@
identifier I;
identifier f;
@@

struct usb_serial_driver I = {
	.read_int_callback =	f,
};

@@
identifier p1, p2;
@@

- void f (struct urb *p1, struct pt_regs *p2)
+ void f (struct urb *p1)
  {
    ... when != p2
  }

@@
expression E1, E2, E3, E4, E5, E6, E7;
identifier f1, f2, f3, f4, f5;
struct urb u;
struct urb *u1;
@@

(
usb_fill_bulk_urb(E1, E2, E3, E4, E5, f1, E6)
|
usb_fill_int_urb(E1, E2, E3, E4, E5, f2, E6, E7)
|
usb_fill_control_urb(E1, E2, E3, E4, E5, E6, f3, E7)
|
u.complete = f4
|
u1->complete = f5
)

// need five copies of the following rule to try to avoid the incompatible
// values problem in the above.  need to think about why the constraint exists.
@@
identifier p1, p2;
@@

- void f1 (struct urb *p1, struct pt_regs *p2)
+ void f1 (struct urb *p1)
  {
    ... when != p2
  }

@@
identifier p1, p2;
@@

- void f2 (struct urb *p1, struct pt_regs *p2)
+ void f2 (struct urb *p1)
  {
    ... when != p2
  }

@@
identifier p1, p2;
@@

- void f3 (struct urb *p1, struct pt_regs *p2)
+ void f3 (struct urb *p1)
  {
    ... when != p2
  }

@@
identifier p1, p2;
@@

- void f4 (struct urb *p1, struct pt_regs *p2)
+ void f4 (struct urb *p1)
  {
    ... when != p2
  }

@@
identifier p1, p2;
@@

- void f5 (struct urb *p1, struct pt_regs *p2)
+ void f5 (struct urb *p1)
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
