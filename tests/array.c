#include <stdio.h> 

void main(int i)
{
	float x[] = { 0.1, 0.2, 0.3};
	int y;
	y = sizeof(x) / sizeof(float);
	printf ("size array = %d\n", y);
	
	
}
