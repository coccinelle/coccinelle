int main(){
	int i,a;
// the pragma following breaks parsing this program (--parse-c)
  	for (i = 0; i < 1; i++)
 	#pragma acc parallel
             a++;
}
