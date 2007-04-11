static struct irqchip mpuio_irq_chip = {
      .ack    = mpuio_ack_irq,
      .mask   = mpuio_mask_irq,
      .unmask = mpuio_unmask_irq
};

static struct irqchip xxx = {
  .a = 12,
  .b = 15,
};

int hello ( String input )
{
  String input = input.lowercase();
  printf(input);   
}
