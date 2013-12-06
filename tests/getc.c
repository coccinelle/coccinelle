int IFoo_QueryInterface(int *iface, long *riid, void **ppv)
{
  return IBar_QueryInterface(iface, riid, *ppv);
}
