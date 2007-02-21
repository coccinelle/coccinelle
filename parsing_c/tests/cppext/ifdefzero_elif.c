

void main(int i)
{

#if 0
  /* XXX the i82593 manual page 6-4 seems to indicate that the stop register
     should be set as below */
  /* outb(CR1_STOP_REG_UPDATE|((RX_SIZE - 0x40)>> RX_SIZE_SHIFT),LCCR(base));*/
#elif 0
  /* but I set it 0 instead */
  lp->stop = 0;
#else
  /* but I set it to 3 bytes per packet less than 8K */
  lp->stop = (0 + RX_SIZE - ((RX_SIZE / 64) * 3)) % RX_SIZE;
#endif

#if 0
  /* XXX the i82593 manual page 6-4 seems to indicate that the stop register
     should be set as below */
  /* outb(CR1_STOP_REG_UPDATE|((RX_SIZE - 0x40)>> RX_SIZE_SHIFT),LCCR(base));*/
#elif 0
  /* but I set it 0 instead */
  lp->stop = 0;
#else
  /* but I set it to 3 bytes per packet less than 8K */
  lp->stop = (0 + RX_SIZE - ((RX_SIZE / 64) * 3)) % RX_SIZE;
#endif

}
