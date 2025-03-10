@ rule1 @
@@

#include <linux/interrupt.h>

// --------------------------------------------------------------
// do some cleaup first.  
// we should specify that these rules only apply to files in the directory
// arch/v850/kernel

@@
int E;
struct pt_regs *regs;
@@

(
- handle_irq(E,regs)
+ handle_irq(E)
|
- __do_IRQ(E,regs)
+ __do_IRQ(E)
)

@@
identifier irq, regs;
@@

  unsigned int
- handle_irq (int irq, struct pt_regs *regs)
+ handle_irq (int irq)
  {
    ... when != regs
  }

// --------------------------------------------------------------
// now the real transformation

@ rule2 depends on rule1 @
expression irq;
identifier handler;
expression irqflags;
expression devname;
expression dev_id;
@@

request_irq(irq, handler, irqflags, devname, dev_id)

@@
typedef irqreturn_t;
identifier rule2.handler, irq, dev, regs;
@@

  static irqreturn_t
- handler(int irq, void *dev, struct pt_regs *regs)
+ handler(int irq, void *dev)
  {
    ... when != regs
  }

@@
identifier rule2.handler;
expression E1, E2;
@@

handler(E1,E2
-       ,NULL
       )

@@
identifier rule2.handler;
expression E1, E2, E3;
@@

handler(E1,E2,
-       E3
+       NEED_TO_CHECK_THIS_USE_OF(E3)
       )
