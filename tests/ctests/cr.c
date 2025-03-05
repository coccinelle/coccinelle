int main(void)
{
#if ! HAVE_XYZ
  f1();
  f2();
#endif
}
