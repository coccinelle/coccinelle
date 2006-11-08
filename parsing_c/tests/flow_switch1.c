static void se401_video_irq(struct urb *urb)
{
  switch(se401->scratch[se401->scratch_next].state) {
    //  case BUFFER_READY:
  case BUFFER_BUSY: {
    se401->dropped++;
    break;
  }
  }

  return;
}
