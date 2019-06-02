/* before the function - if function were static this wouldn't get matched  */
int main() /* after the header */ {
  foo();
  /* a comment1 */
  foo(/*in arglist*/);
  /* a comment2 */
  foo();
  bar();
}
/* after the function */
