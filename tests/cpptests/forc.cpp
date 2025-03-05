static void Ptngc_widediv()
{
  while (rmask)
    {
      if ((s_U<hi) || ((s_U==hi) && (s_L<=lo)))
        {
          if (s_L>lo)
            {
              unsigned int t;
              hi--; /* Borrow */
            }
        }
    }
}
