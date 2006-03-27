@@
expression n;
context C;
fresh identifier irq_mask, delay;
constant n1;
constant n2 = 100 / n1;
@@

  {
    unsigned long irq_mask, delay;
-   autoirq_setup(n);
+   irq_mask = probe_irq_on();
    ...
(
-   C[autoirq_report(1)]
+   delay = jiffies + HZ/50;
|
-   C[autoirq_report(n1)]
+   delay = jiffies + HZ/n2;
)
+   while (time_before(jiffies, delay)) ;
+   C[probe_irq_off(irq_mask)]

  }
