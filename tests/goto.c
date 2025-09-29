int main () {
  if (x)
   call(); /* some stuff */
  if (x)
   goto L; /* comment after */
  if (x) {
   goto L;
   /* comment after */
  }
L:
  call(); /* after call */
  return;
}
