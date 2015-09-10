#define IRQ_T(info) ((info->flags & ASYNC_SHARE_IRQ) ? \
  IRQF_SHARED : IRQF_DISABLED)


void main(int i)
{
}
