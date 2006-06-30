int i;

void main()
{
  /* a comment */

  f(4) + f(5);

  f(f(3)); // if uncomment, should have the warning "already minused token"
  if(f(1))
    f(1);
  else
    f(2);

  if(1) 
    g(1);
  else 
    g(2);
}


void mainbis() {
  f(10);
}


