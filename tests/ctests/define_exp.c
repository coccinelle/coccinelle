#define IRQ_T(info) ((info->flags & ASYNC_SHARE_IRQ) ? \
  SA_SHIRQ : SA_INTERRUPT)


void main(int i)
{
}
