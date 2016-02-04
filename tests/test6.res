int i;

void main()
{
  /* a comment */

  f(4, "foo") + f(5, "foo");

  //f(f(3)); // if uncomment, should have the warning "already minused token"
  if(f(1, "foo"))
    f(1, "foo");
  else
    f(2, "foo");

  if(1) 
    g(1);
  else 
    g(2);
}


void mainbis() {
  f(10, "foo");
}


