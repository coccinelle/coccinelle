void main(int i) {

  if(1) goto out1;
  if(1) goto out2;

  f(1);
  return 2;
out1: 
  f(2);

out2: 
  f(2);

final:
  return 1;


}
