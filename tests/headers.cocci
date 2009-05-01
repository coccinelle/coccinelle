@ rule1 @
identifier f;
@@

usb_fill_bulk_urb(f)

@ rule2 extends rule1 @
identifier p1, p2;
@@

- void f(struct urb *p1, struct pt_regs *p2)
+ void f(struct urb *p1)
  {
    ... when != p2
  }
