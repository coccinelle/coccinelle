int func() {
  int c;
  Packet * p1 = SCMalloc(SIZE_OF_PACKET),* p2 = SCMalloc(SIZE_OF_PACKET);
  int y;
  if (p1 == NULL)
    return 0;
  if (p2 == NULL)
    return 0;
  a = 3;
  SCFree(p1);
  SCFree(p2);
  return x+y;
}
