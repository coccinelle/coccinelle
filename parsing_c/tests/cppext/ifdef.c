
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

#ifdef DEBUG
	printk("hd%c: read: sector %ld, remaining = %ld, buffer=0x%08lx\n",
		dev+'a', CURRENT->sector, CURRENT->nr_sectors,
 		(unsigned long) CURRENT->buffer+512));
#endif

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
