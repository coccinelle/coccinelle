@ Exemple4 @  @@

  init_IRQ(...)
  {
    <...
+   #ifdef CONFIG_NKERNEL
+   if(irq < IRQ_LIMIT)
+   #endif
    *desc = irq_desc;
    ...>
}
