// sgrep

// Case 1: search for irq functions where interrupt.h is not used
// these might be ok as is, because some definitions of request_irq still
// have the pt_regs parameter in the signature

@ rule1 @
@@

#include <linux/interrupt.h>

@ rule2 depends on !rule1 @
expression irq;
identifier handler;
expression irqflags;
expression devname;
expression dev_id;
@@

request_irq(irq, handler, irqflags, devname, dev_id)

@@
identifier rule2.handler, irq, dev, regs;
@@

* handler(int irq, void *dev, struct pt_regs *regs)
  { ... }

// ----------------------------------------------------------------------

// Case 2: the function is not static.  This only works when there is no
// static handler function in the file, but fortunately this is the case
// (we have detected this by actually doing the transformation, which makes
// the second rule no longer match; unfortunately there is no disjunction
// at the function level)

@ rule3 depends on rule1 @
expression irq;
identifier handler;
expression irqflags;
expression devname;
expression dev_id;
@@

request_irq(irq, handler, irqflags, devname, dev_id)

@ rule4 @
typedef irqreturn_t;
identifier rule3.handler, irq, dev, regs;
@@

  static irqreturn_t handler(int irq, void *dev, struct pt_regs *regs)
  { ... }

@ rule5 depends on !rule4 @
identifier rule3.handler, irq, dev, regs;
@@

* handler(int irq, void *dev, struct pt_regs *regs)
  { ... }

// ----------------------------------------------------------------------

// Case 3: the code contains a reference to the regs parameter

@@
identifier rule3.handler, irq, dev, regs;
int E;
@@

  handler(int irq, void *dev, struct pt_regs *regs)
  {
    <...
(
    handle_irq(E,regs)
|
*   regs
)
    ...>
  }
