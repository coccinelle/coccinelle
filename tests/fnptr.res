static irqreturn_t sci_tx_interrupt(int irq, void *ptr, struct pt_regs *regs)
{
}

static int sci_request_irq(struct sci_port *port) {
  int i;
  irqreturn_t (*handlers[4])(int irq, void *ptr, struct pt_regs *regs) = {
    sci_er_interrupt, sci_rx_interrupt, sci_tx_interrupt,
    sci_br_interrupt,
  };
  request_irq(port->irqs[0], sci_mpxed_interrupt, foo());
}
