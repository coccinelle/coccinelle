
#ifdef xxx
struct x { 
  int i;
};
#endif


void main(int i) {

#ifdef xxx
  i++;
#endif
  return i;
}


void main(int i) {

#ifdef xxx
  i++;
  //return i;
#else
  i--;
  //return i;
#endif
}


/*
void main(int i) {

#ifdef xxx
  i++;
#ifdef xxx2
  i++;
  return i;
#else
  return i;
#endif
#endif

  return i;
}
*/
